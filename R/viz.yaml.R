#' How to write a viz.yaml
#' 
#' See \link[testviz]{testviz} for an example, and see below for a detailed 
#' description.
#' 
#' @section fetch:
#'   
#'   The fetch section takes a list of items. Each item is a raw data file.
#'   Items always have a \code{id} field. They also always have information,
#'   either explicit or implicit, about how to (1) fetch and (2) read the data
#'   file.
#'   
#'   Fields describing (1) fetch include \code{fetcher}, which can take the 
#'   values \code{file} or \code{sciencebase}, If \code{sciencebase}, then the 
#'   \code{remoteItemId} field should also be included and should be the 
#'   24-digit hexadecimal ScienceBase item ID, and the \code{remoteFilename} 
#'   should be included and should be the filename as the item is stored on 
#'   ScienceBase.
#'   
#'   Fields describing (2) read include \code{mimeType}, which can take the 
#'   values \code{text/csv}, \code{text/tab-seperated-values}, \code{text/yaml},
#'   or 
#'   \code{application/vnd.openxmlformats-officedocument.spreadsheetml.sheet}. 
#'   If the reader function is unspecified then a default reader will use the 
#'   \code{mimeType} to determine how to read the data file into R. 
#'   Alternatively, the \code{customReader} field may be included with a value 
#'   describing a custom reading function defined in the scripts|lib folder, 
#'   where the function has the name \code{readData.mytype} and the value in the
#'   \code{customReader} field is \code{mytype}. For example, you could define a
#'   custom function called readData.netCDF and give \code{customReader: 
#'   netCDF}. If both customReader and mimeType are specified, mimeType will be 
#'   passed as a second argument to the customReader function.
#'   
#'   The \code{export} tag is by default true for figures, false for data and 
#'   munged. but can be set for any of those classes of items. Any item for 
#'   which export=true will be copied to the target directory when the full 
#'   visualization is built.
#'   
#' @name viz.yaml
NULL