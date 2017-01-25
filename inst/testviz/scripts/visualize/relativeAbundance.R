#' My visualizer function
#'
#' Could also accept these args: viz.id, data.info. but we're not using them so
#' won't list them.
visualize.relativeAbundance <- function(viz) {
  outfile <- viz[["location"]]
  data <- readDepends(viz)

  svg(outfile, height=4, width=4)
  plot(Above ~ Below, data[["MayflyNymph"]])
  dev.off()
}
