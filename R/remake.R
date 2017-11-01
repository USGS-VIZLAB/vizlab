# Functions to call exclusively from remake.yaml

#' Read all arguments declared for all items into an R list
#'
#' This function is used to create dependencies for individual remake items so
#' that those items are rebuilt when their viz arguments change. The returned
#' list excludes resources, which aren't listed explicitly in the viz.yaml and
#' therefore can't have arguments.
#' @param viz.yaml the name of the viz.yaml. should always be 'viz.yaml'.
#'   Included as an argument so remake can make the dependency on the viz.yaml
#'   explicit.
#' @export
collectItemArguments <- function(viz.yaml) {
  stopifnot(viz.yaml == 'viz.yaml')
  content.infos <- getContentInfos(block=c("parameter", "fetch", "process", "visualize", "publish"))
  setNames(content.infos, sapply(content.infos, `[[`, 'id'))
}
