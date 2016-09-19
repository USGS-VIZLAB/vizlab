#' Peform the publish step to ready the viz for hosting
#'
#' Determine the type and dispatch to that method to produce
#' files to serve up as the final viz
#'
#' I've been thinking maybe switching 'x' to 'viz' would be more clear
#'
#' @param viz vizlab object or identifier
#' @export
publish <- function(viz) UseMethod("publish")

#' publish a given id
#' @rdname publish
#' @export
publish.character <- function(viz) {
  viz <- as.viz(viz)
  viz <- as.publisher(viz)
  publish(viz)
}

#' publish a page
#' @rdname publish
#' @importFrom whisker whisker.render
#' @export
publish.page <- function(viz) {
  required <- c("template", "context")
  checkRequired(viz, required)

  template <- readTemplate(viz[['template']])

  dependencies <- lapply(viz[['depends']], publish)
  names(dependencies) <- viz[['depends']]

  context <- buildContext(viz, dependencies)

  viz.info <- getBlocks("info")

  # TODO separate out analytics bits
  context[["analytics-id"]] <- viz.info[['analytics-id']]
  context[["vizlab-page-path"]] <- viz.info[['path']]

  file <- export(viz)
  cat(whisker.render(template = template, data = context), file = file)
}

#' publish a section
#'
#' @details Sections are published individually but are returned invisibly
#' as text to be used directly.
#'
#' @importFrom whisker whisker.render
#' @rdname publish
#' @export
publish.section <- function(viz) {
  required <- c("template")
  checkRequired(viz, required)

  # TODO Watch out for cyclic depends
  dependencies <- lapply(viz[['depends']], publish)
  names(dependencies) <- viz[['depends']]

  context <- buildContext(viz, dependencies)

  template <- readTemplate(viz[['template']])

  output <- whisker.render(template = template, data = context)
  return(output)
}

#' publish a resource
#'
#' @details This copies static resources to the target directory, and invisibly
#' will return the preferred usage.
#'
#' The job of minification or css precompiling could also be added here, but
#' currently this is not handled.
#'
#' Also, templating the resources that make sense would be useful
#'
#' @rdname publish
#' @export
publish.resource <- function(viz) {
  # figure out resource type and hand to resource handler
  # going to start out with simple images
  file <- export(viz)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  file.copy(viz[['location']], file)
  viz[['relpath']] <- relativePath(file)
  return(viz)
}

#' Image publishing
#'
#' @rdname publish
#' @export
publish.img <- function(viz) {
  required <- c("alttext", "relpath")
  viz <- NextMethod()
  checkRequired(viz, required)

  alt.text <- viz[['alttext']]
  relative.path <- viz[['relpath']]
  return(sprintf('<img src="%s" alt="%s" />', relative.path, alt.text))
}

#' javascript publishing
#' TODO allow for cdn js
#'
#' @rdname publish
#' @export
publish.js <- function(viz) {
  required <- c("relpath", "mimetype")
  viz <- NextMethod()
  checkRequired(viz, required)

  output <- sprintf('<script src="%s" type="text/javascript"></script>', viz[['relpath']])
  return(output)
}

#' css publishing
#'
#' @rdname publish
#' @export
publish.css <- function(viz) {
  required <- c("relpath", "mimetype")
  viz <- NextMethod()
  checkRequired(viz, required)

  output <- sprintf('<link href="%s" rel="stylesheet" type="text/css" />', viz[['relpath']])
  return(output)
}

#' svg publishing
#'
#' @rdname publish
#' @export
publish.svg <- function(viz) {
  required <- c("relpath")
  viz <- NextMethod()
  checkRequired(viz, required)

  output <- sprintf('<object id="%s" type="image/svg+xml" class="svgFig" data="%s"></object>',
                    viz[['id']], viz[['relpath']])
  return(output)
}

#' coerce to a publisher
#' @param viz object describing publisher
#' @param ... not used, just for consistency
#' @export
as.publisher <- function(viz, ...) {
  # default to a resource
  publisher <- ifelse(exists("publisher", viz), viz[['publisher']], "resource")
  class(viz) <- c(publisher, "publisher", class(viz))
  if (publisher == "resource") {
    viz <- as.resource(viz)
  }
  return(viz)
}

#' coerce to resource
#' @param viz vizlab object
#' @param ... not used, following convention
#' @export
as.resource <- function(viz, ...) {
  required <- c("mimetype")
  checkRequired(viz, required)

  mimetype <- viz[['mimetype']]
  resource <- lookupMimetype(mimetype)
  if(length(resource) == 0){
    stop(mimetype, " not supported: ", viz[['id']])
  }
  
  class(viz) <- c(resource, class(viz))
  return(viz)
}
