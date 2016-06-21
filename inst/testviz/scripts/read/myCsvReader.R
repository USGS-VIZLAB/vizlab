readData.myCsvReader <- function(viz.id, location, ...) {
  setNames(read.csv(location, header=TRUE), c('ID','Above','Below'))
}