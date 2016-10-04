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

  dependencies <- as.list(viz[['depends']])
  names(dependencies) <- viz[['depends']]

  # add automatic dependencies
  vizlabjs <- '_vizlabJS'
  dependencies[[vizlabjs]] <- getVizlabJS()

  # publish all dependencies
  dependencies <- lapply(dependencies, publish)

  context <- buildContext(viz, dependencies)

  #also manually put resources into context
  context[['resources']] <- append(context[['resources']], dependencies[[vizlabjs]])
  context[['info']] <- append(context[['info']], getBlocks("info", keep.block=F)[[1]])

  partials <- getPartialLibrary()
  file <- export(viz)
  cat(whisker.render(template = template, data = context, partials = partials), file = file)
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

  viz[['output']] <- whisker.render(template = template, data = context)
  if (!is.null(viz[['analytics']])) {
    viz <- analytics(viz)
  }
  return(viz[['output']])
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
  destFile <- export(viz)
  dir.create(dirname(destFile), recursive = TRUE, showWarnings = FALSE)
  srcFile <- viz[['location']]
  if (!is.null(viz[['packaging']]) && viz[['packaging']] == "vizlab") {
    srcFile <- system.file(srcFile, package = "vizlab")
  }
  file.copy(srcFile, destFile, overwrite = TRUE)
  viz[['relpath']] <- relativePath(destFile)
  return(viz)
}

#' Image publishing
#'
#' @rdname publish
#' @export
publish.img <- function(viz) {
  required <- c("alttext", "relpath", "title")
  viz <- NextMethod()
  checkRequired(viz, required)

  alt.text <- viz[['alttext']]
  relative.path <- viz[['relpath']]
  title.text <- viz[['title']]
  return(sprintf('<img src="%s" alt="%s" title="%s" />', relative.path, alt.text, title.text))
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
  required <- c("relpath", "title", "alttext")
  viz <- NextMethod()
  checkRequired(viz, required)

  output <- sprintf('<object id="%s" type="image/svg+xml" class="svgFig" data="%s" title="%s" > %s </object>',
                    viz[['id']], viz[['relpath']], viz[['title']], viz[['alttext']])
  return(output)
}

#' Footer publishing 
#' @param viz
#'
#' @rdname publish
#' @export

publish.footer <- function(viz) {
  #should also check blogs?  Or one or the other?
  checkRequired(viz, required = "vizzies")
  
  # TODO: move css to same dir
  index_loc_css <- 'target/css'
  if(!dir.exists(index_loc_css)) dir.create(index_loc_css, recursive=TRUE)
  file.copy(from=system.file('footer/css/footer.css', package="vizlab"), to=index_loc_css)
  
  #TODO: get thumbnail from repo, copy to images
  vizzies <- viz$vizzies
  for(v in 1:length(vizzies)){
    thumbURL <- getVizThumbnail(repo=viz[[v]]$repo, org=viz[[v]]$org)
    download.file(url=thumbURL, destfile = viz[[v]]$thumbLoc)
  }
  
  #TODO: stuff from publish.section
  dependencies <- lapply(viz[['depends']], publish)
  names(dependencies) <- viz[['depends']]
  
  context <- buildContext(viz, dependencies)
  
  template <- readTemplate(viz[['template']])
  
  viz[['output']] <- whisker.render(template = template, data = context)
  if (!is.null(viz[['analytics']])) {
    viz <- analytics(viz)
  }
  return(viz[['output']])
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
