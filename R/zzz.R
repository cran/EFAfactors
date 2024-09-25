
.onAttach <- function(libname, pkgname){
  packageStartupMessage(StartWelcomeMessage())
}

StartWelcomeMessage <- function(){
  paste("EFAfactors R Package ",
        "(version ", utils::packageDescription("EFAfactors")$Version,
        "; ", utils::packageDescription("EFAfactors")$Date, ")\n",
        sep="")
}
