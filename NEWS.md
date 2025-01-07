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

