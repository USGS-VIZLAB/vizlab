#' My visualizer function
#' 
#' Could also accept these args: viz.id, data.info. but we're not using them so
#' won't list them.
visualize.relativeAbundance <- function(
  CuyahogaShort=readData('CuyahogaShort'), 
  MayflyNymph=readData('MayflyNymph'), 
  outfile='figures/relativeAbundanceFig.svg', ...) {
  
  svg(outfile, height=4, width=4)
  plot(Above ~ Below, MayflyNymph)
  dev.off()
}