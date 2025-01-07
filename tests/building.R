
# library(rstudioapi)
#
# script_path <- rstudioapi::getActiveDocumentContext()$path
# print(script_path)
# working_directory <- dirname(dirname(script_path))
# setwd(working_directory)

# library(devtools)
# clean_dll() ## hidden files and directories

## code  in Makevars.win for Windows
# PKG_CXXFLAGS = $(SHLIB_OPENMP_CXXFLAGS) $(RCPP_CPPFLAGS) $(RCPPARMADILLO_CPPFLAGS)
# PKG_LIBS = $(SHLIB_OPENMP_CXXFLAGS) $(RCPP_LIBS) $(RCPPARMADILLO_LIBS) -L$(R_HOME)/bin/x64 -lRblas -lRlapack

## code  in Makevars for MacOS
# PKG_CXXFLAGS = $(SHLIB_OPENMP_CXXFLAGS) $(RCPP_CPPFLAGS) $(RCPPARMADILLO_CPPFLAGS)
# PKG_LIBS = $(SHLIB_OPENMP_CXXFLAGS) $(RCPP_LIBS) $(RCPPARMADILLO_LIBS) -L/usr/lib -llapack -lblas

## Roxygen2
# devtools::document()  ## build .Rd file

# pkgdown::build_site()  ## buile website manual

# # devtools::build()  ## build .tar.gz file

# devtools::check(manual = TRUE)

# devtools::spell_check()
# devtools::check_rhub()
# devtools::check_win_devel()

# library(checkhelper)
# check_clean_userspace()

# library(EFAfactors)

# devtools::release()

# pack <- available.packages()
# which(rownames(pack) == "EFAfactors")

# library(cranlogs)
# downloads <- cran_downloads(packages = "EFAfactors", from = "2024-01-01", to = "2024-11-19")
# print(downloads)
# sum(downloads$count)

