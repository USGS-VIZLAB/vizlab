#' My visualizer function
#' 
#' Could also accept these args: viz.id, data.info. but we're not using them so
#' won't list them.
visualizeData.relativeAbundance <- function(CuyahogaShort, MayflyNymph, ..., outfile) {
  svg(outfile, height=4, width=4)
  plot(Above ~ Below, MayflyNymph)
  dev.off()
}