# EFAfactors 1.2.3

-   Note    - Due to the correction of `factor.analysis` in version 1.2.3, methods involving `factor.analysis` may produce different outputs compared to versions prior to 1.2.3, such as `CD` and `FF`. Please take the results from the new version as the standard.
-   Fixed   - Fixed the issue where `factor.analysis` only returned `nfact` eigenvalues.
-   Change  - At the request of CRAN, some `Examples` have been removed to avoid excessive time consumption during automatic checks.

# EFAfactors 1.2.2

-   Fixed   - Fixed the issue where the loadings in `factor.analysis` may be inverted.

# EFAfactors 1.2.1

-   Change  - Optimized the `CDF` function code to improve its execution efficiency.
-   Fixed   - Resolved the loop error in `GenData` when the number of questions is very large.
-   Fixed   - Fixed `src/Makevars` to achieve better portability.

# EFAfactors 1.2.0

-   Fixed   -  The function, originally dependent on the R package `ParamHelpers`, has been rewritten based on version 1.14.1 to support the `FF` function and no longer depends on the R package `ParamHelpers`.
-   Fixed   -  The function `EFAsim.data` no longer requires the parameter `seed`.
-   Added   -  A new function `check_python_libraries` has been provided to help users check if the Python libraries `numpy` and `onnxruntime` are missing, and users can easily install them using this function when they are not present.

# EFAfactors 1.1.1

-   Fixed   -  Corrected the error in `Hull`.
-   Added   -  Added the URL of the online manual to the `DESCRIPTION` field.

# EFAfactors 1.1.0

-   Added   -  Added factor analysis eigenvalues for `EFAscreet`, `Hull`, and `KGC`.
-   Fixed   -  Corrected the erroneous `DESCRIPTION`.
-   Fixed   -  Corrected the error in the `Hull` plot.
-   Fixed   -  Corrected the calculation method for reference eigenvalues in factor analysis based `PA`.

# EFAfactors 1.0.0

-   Initial release.

