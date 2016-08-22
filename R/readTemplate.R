#' Reads a template file and returns the contents
#'
#' @param x character vector representing template by name or file path
#' @return character vector containing template contents
#' @export
readTemplate <- function(x) {
  if (length(x) != 1 && !is.na(x)) {
    stop("readTemplate only reads one template at a time")
  }

  if (file.exists(x)) {
    template.file <- x
  }
  else {
    template.file <- getTemplateFromLibrary(x)
  }

  template <- readLines(template.file)

  return(template)
}

#' Internal function to get shared templates
#'
#' @param x character vector template name
#' @return character vector template contents
getTemplateFromLibrary <- function(x) {
  template <- rep(NA_character_, length(x))
  template.dir <- system.file("templates", package="vizlab")
  # convention all files are name.mustache
  template.files <- file.path(template.dir, paste0(x, ".mustache"))
  files.exist <- file.exists(template.files)
  template[files.exist] <- template.files[files.exist]
  return(template)
}
