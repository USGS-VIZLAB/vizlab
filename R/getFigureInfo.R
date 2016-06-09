getFigureInfo <- function(identifier=NULL) {
  viz.yaml <- yaml::yaml.load_file("viz.yaml")
  figures <- viz.yaml[["figures"]]
  figureInfo <- list()
  for (figure in figures) {
    figureOutputs <- generateFigureOutputs(figure)
    figure$outputs <- figureOutputs
    figureInfo[[figure$id]] <- figure
  }
  if (!is.null(identifier)) {
    figureInfo <- figureInfo[[identifier]]
  }
  return(figureInfo)
}

generateFigureOutputs <- function(figure) {
  figureOutputs <- list()
  for (tag in figure$tags) {
    figureOutputs[[tag]] <- paste0(figure$id, "-", tag, ".svg")
  }
  return(figureOutputs)
}