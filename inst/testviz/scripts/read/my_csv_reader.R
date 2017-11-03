readData.my_csv_reader <- function(viz) {
  location <- viz[['location']]
  setNames(read.csv(location, header=TRUE), c('ID','Above','Below'))
}
