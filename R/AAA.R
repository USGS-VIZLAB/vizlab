#set up package environment
vizlab.pkg.env <- new.env()

.onLoad = function(libname, pkgname){
  vizlab.pkg.env$baseURL <- "https://owi.usgs.gov/vizlab"
}