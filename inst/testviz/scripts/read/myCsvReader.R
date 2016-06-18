readData.myCsvReader <- function(viz.id, data.info, ...) {
  setNames(read.csv(data.info$location, header=TRUE), c('ID','Above','Below'))
}