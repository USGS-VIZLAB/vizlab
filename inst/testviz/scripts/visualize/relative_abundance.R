#' My visualizer function
#'
#' Could also accept these args: viz.id, data.info. but we're not using them so
#' won't list them.
visualize.relative_abundance <- function(viz) {
  outfile <- viz[["location"]]
  data <- readDepends(viz)
  sizes <- data[["sizes"]]
  svg(outfile, height=sizes$height, width=sizes$width)
  plot(Above ~ Below, data[["mayfly_nymph"]])
  dev.off()
}
