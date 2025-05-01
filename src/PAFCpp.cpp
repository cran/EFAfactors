#include <RcppArmadillo.h>
#include <algorithm>
#include <cmath>
#include <vector>
#include <random>

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// 反向排列向量和矩阵列
std::tuple<arma::vec, arma::mat> eigen_decomposition(const arma::mat& Cor_Matrix) {
  arma::vec eigval;
  arma::mat eigvec;
  eig_sym(eigval, eigvec, Cor_Matrix);
  return std::make_tuple(reverse(eigval), reverse(eigvec, 1));
}

// [[Rcpp::export]]
List PAFCpp(arma::mat Cor_Matrix, int N_Factors, int Max_Iter = 1000, double Criterion = 0.0001) {

  int k = Cor_Matrix.n_cols;

  arma::vec Old_H2(k, arma::fill::zeros);
  arma::vec H2(k, arma::fill::zeros);

  double Change = 1.0;
  int Iter = 0;

  arma::mat Factor_Loadings(k, N_Factors, arma::fill::zeros);
  arma::vec Eig_Values;
  arma::mat Eig_Vectors;

  while ((Change >= Criterion) && (Iter < Max_Iter)) {
    Iter += 1;

    // 特征分解
    std::tie(Eig_Values, Eig_Vectors) = eigen_decomposition(Cor_Matrix);

    // 只取前N_Factors个特征值与向量
    arma::vec L = sqrt(Eig_Values.head(N_Factors));

    for (int i = 0; i < N_Factors; i++) {
      Factor_Loadings.col(i) = Eig_Vectors.col(i) * L(i);
    }

    // 调整因子载荷方向：保证每列绝对值最大的元素为正
    for (int j = 0; j < N_Factors; j++) {
      arma::uword max_idx = index_max(abs(Factor_Loadings.col(j)));
      if (Factor_Loadings(max_idx, j) < 0) {
        Factor_Loadings.col(j) *= -1.0;
        Eig_Vectors.col(j) *= -1.0; // 同步调整特征向量
      }
    }

    // 更新公因子方差 H2
    for (int i = 0; i < k; i++) {
      H2(i) = sum(square(Factor_Loadings.row(i)));
    }

    // 判断是否收敛
    Change = sum(abs(Old_H2 - H2));
    Old_H2 = H2;
    Cor_Matrix.diag() = H2; // 对角线更新为新的公因子方差
  }

  // 只返回前N_Factors的特征值（因只取了前N因子）
  arma::vec final_eigenvalues = Eig_Values.head(N_Factors);

  return List::create(
    Named("eigen") = final_eigenvalues,
    Named("loadings") = Factor_Loadings,
    Named("iterations") = Iter,
    Named("H2") = H2
  );
}
