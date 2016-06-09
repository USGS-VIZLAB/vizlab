#' Create data.yaml from the information in viz.yaml
#' 
#' Uses information in the data block of viz.yaml to create the remake file
#' data.yaml
#' 
#' @import yaml
makeDataRemakefile <- function() {
  data.info <- getDataInfo()
  yaml.list <- list(
    packages=c('vizlab'),
    #sources=c(),
    targets=c(
      list(all=list(depends=names(data.info))),
      lapply(names(data.info), function(di) {
        inf <- data.info[[di]]
        c(
          if('filename' %in% names(inf)) list(depends=inf$filename) else list(),
          if('filename' %in% names(inf)) list(command='readData(target_name)') else list()
        )
      })
    )
  )
  cat(as.yaml(yaml.list))
}