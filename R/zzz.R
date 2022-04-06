.onLoad <- function(libname, pkgname){
  assign(".miniER.workspace",
         new.env(parent = globalenv()),
         globalenv())

  initialize_workspace(.miniER.workspace)
}


