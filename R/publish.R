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
#' @export
publish.page <- function(viz) {
  required <- c("template", "context")
  checkRequired(viz, required)

  template <- template(viz[['template']])

  dependencies <- gatherDependencyList(viz[['depends']], template[['depends']])
  # TODO handle accessing nested dependencies elsewhere

  #also manually put resources into context
  context <- replaceOrAppend(template[['context']], viz[['context']])
  context[['info']] <- replaceOrAppend(getBlocks("info", keep.block=F)[[1]], context[['info']])

  # replace ids in context with expanded dependencies
  context <- buildContext(viz, dependencies)

  partials <- getPartialLibrary()
  file <- export(viz)
  render(template, context, file)
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

  template <- template(viz[['template']])

  # TODO Watch out for cyclic depends
  dependencies <- gatherDependencyList(viz[['depends']], template[['depends']])

  names(dependencies) <- viz[['depends']]
  dependencies <- c(dependencies, recursive = TRUE)

  context <- replaceOrAppend(template[['context']], viz[['context']])
  context <- buildContext(viz, dependencies)

  viz[['output']] <- render(template, context)
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
  if (!is.null(destFile)) {
    dir.create(dirname(destFile), recursive = TRUE, showWarnings = FALSE)
    srcFile <- viz[['location']]
    if (!is.null(viz[['packaging']]) && viz[['packaging']] == "vizlab") {
      srcFile <- system.file(srcFile, package = "vizlab")
    }
    file.copy(srcFile, destFile, overwrite = TRUE)
    viz[['relpath']] <- relativePath(destFile)
  } else {
    viz[['relpath']] <- NA
  }
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

  html <- NULL
  if (!is.na(viz[['relpath']])) {
    alt.text <- viz[['alttext']]
    relative.path <- viz[['relpath']]
    title.text <- viz[['title']]
    html <- sprintf('<img src="%s?_c=%s" alt="%s" title="%s" />', relative.path, uniqueness(),
                    alt.text, title.text)
  }
  return(html)
}

#' Favicon resource
#'
#' @rdname publish
#' @export
publish.ico <- function(viz) {
  required <- c("relpath")
  viz <- NextMethod()
  checkRequired(viz, required)

  html <- NULL
  if (!is.na(viz[['relpath']])) {
    relative.path <- viz[['relpath']]
    html <- sprintf('<link rel="icon" type="image/ico" href="%s?_c=%s"/>',
                    relative.path, uniqueness())
  }
  return(html)
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

  output <- NULL
  if (!is.na(viz[['relpath']])) {
    output <- sprintf('<script src="%s?_c=%s" type="text/javascript"></script>',
                      viz[['relpath']], uniqueness())
  }
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

  output <- NULL
  if (!is.na(viz[['relpath']])) {
    output <- sprintf('<link href="%s?_c=%s" rel="stylesheet" type="text/css" />',
                      viz[['relpath']], uniqueness())
  }
  return(output)
}

#' svg publishing, may return NULL
#'
#' @rdname publish
#' @export
publish.svg <- function(viz) {
  required <- c("relpath", "title", "alttext")
  viz <- NextMethod()
  checkRequired(viz, required)

  output <- NULL
  if (is.null(viz[['inline']]) || !viz[['inline']]) {
    if (!is.na(viz[['relpath']])) {
      output <- sprintf(
        '<object id="%s" type="image/svg+xml" class="svgFig" data="%s" title="%s" >%s</object>',
        viz[['id']], viz[['relpath']], viz[['title']], viz[['alttext']])
    }
  } else {
    # skip prolog
    output <- scan(file = viz[['location']], what = "character", sep = "\n", skip = 1)
    output <- paste(output, collapse = "\n")
  }
  return(output)
}

#' Footer publishing
#' @importFrom utils download.file
#' @rdname publish
#' @export

publish.footer <- function(viz) {
  #should also check blogs?  Or one or the other?
  checkRequired(viz, required = "vizzies")

  template <- template(viz[['template']])
  dependencies <- lapply(c(viz[['depends']], template[['depends']]), publish)
  names(dependencies) <- viz[['depends']]

  context <- buildContext(viz, dependencies)

  #add info from viz.yaml to context to inject into template
  vizzies <- viz$vizzies
  for(v in 1:length(vizzies)){
    info <- getVizInfo(repo=vizzies[[v]]$repo, org=vizzies[[v]]$org)
    if (is.null(vizzies[[v]]$name)){ # don't replace it if it is already set
      vizzies[[v]]$name <- info$context$name
    }

    # if / is first char, treat as relative path. If not, treat as absolute path.
    if(strsplit(info$context$path, split = "")[[1]][1] == "/"){
      vizzies[[v]]$url <- paste0("https://owi.usgs.gov/vizlab", info$context$path)
      vizzies[[v]]$thumbLoc <- paste0(vizzies[[v]]$url, info$context$thumbnail)
    } else {
      vizzies[[v]]$url <- info$context$path
      vizzies[[v]]$thumbLoc <- paste0(vizzies[[v]]$url, info$context$thumbnail)
    }
  }
  context[['blogsInFooter']] <- viz$blogsInFooter
  context[['blogs']] <- viz$blogs
  context[['vizzies']] <- vizzies


  viz[['output']] <- whisker.render(template = template, data = context)
  if (!is.null(viz[['analytics']])) {
    viz <- analytics(viz)
  }
  return(viz[['output']])
}

#' publish landing page
#'
#' @rdname publish
#' @export
publish.landing <- function(viz){

  repos <- getRepoNames(viz[['org']])
  viz_info <- lapply(repos, getVizInfo, org=viz[['org']])
  names(viz_info) <- repos
  viz_info <- viz_info[!sapply(viz_info, is.null)]

  pageviz <- viz
  names(pageviz$depends) <- pageviz$depends
  pageviz$depends <- as.list(pageviz$depends)
  pageviz$depends <- append(pageviz$depends, viz_info)
  pageviz$context <- list(sections = c("usgsHeader", "owiNav", "header", names(viz_info), "usgsFooter"), #names of section ids
                          resources = c("landingCSS", "owiCSS", "jquery", "appJS"))
  pageviz$publisher <- "page"
  pageviz <- as.viz(pageviz)
  pageviz <- as.publisher(pageviz) #maybe/maybe not

  publish(pageviz)
}

#' publish template
#'
#' @rdname publish
#' @export
publish.template <- function(viz) {
  # do nothing for now
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
  } else if (publisher == "template") {
    viz <- as.template(viz)
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
    warning(mimetype, " will be treated as data: ", viz[['id']])
    resource <- "data"
  }

  class(viz) <- c(resource, class(viz))
  return(viz)
}
