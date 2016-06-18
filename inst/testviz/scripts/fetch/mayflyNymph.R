fetchData.mayflyNymph <- function(viz.id, data.info) {
  # this function doesn't actually do, or need to do, anything
  message("Great! MayflyNymph.csv is already at ", data.info$location)
  system(paste0("touch ", data.info$location))
}