#' My mayfly fetcher function
#' 
#' Could also accept the data.info arg but won't because we don't use it.
#' Intentionally switching up the arg order just to show that it can be done
#' (though it's not necessarily a great idea)
fetchData.mayflyNymph <- function(outfile, viz.id, ...) {
  # this function doesn't actually do, or need to do, anything
  message("Great! MayflyNymph.csv is already at ", outfile)
  system(paste0("touch ", outfile))
}