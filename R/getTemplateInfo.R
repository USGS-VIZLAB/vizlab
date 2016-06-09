#' @import yaml
getTemplateInfo <- function() {
  viz.yaml <- yaml::yaml.load_file("viz.yaml")
  templateInfo <- list(partials=list(), data=list())
  
  sections <- viz.yaml[["sections"]]
  for (section in sections) {
    id <- section[["id"]]
    templateInfo$partials[[id]] <- readLines(file.path(section[["template"]]))
    templateInfo$data[[id]] <- readData(section[["context"]])
  }
  
  return(templateInfo)
}