cleanup <- function(oldwd, testtmp) {
  setwd(oldwd)
  unlink(testtmp, recursive = TRUE)
  invisible()
}

setup <- function(copyTestViz = FALSE) {
  testtmp <- file.path(tempdir(), "testtmp")
  dir.create(testtmp)
  setwd(testtmp)
  # need to copy any other files needed for testing
  #just copy everything? should need it eventually for more tests?
  if(copyTestViz){
    file.copy(Sys.glob(paste0(system.file('testviz', package = 'vizlab'),"/*")),
              testtmp, recursive = TRUE)
    #create timestamp folder
    dir.create('vizlab/make/timestamps', recursive = TRUE)
  }
  return(testtmp)
}