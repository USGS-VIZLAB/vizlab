#' Peform the publish step to ready the viz for hosting
#'
#' Determine the type and dispatch to that method to produce
#' files to serve up as the final viz
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

#' publish a list representing a viz
#' @rdname publish
#' @export
publish.list <- function(viz) {
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

  dependencies <- gatherDependencyList(c(viz[['depends']], template[['depends']]))

  # also manually put resources into context
  context <- replaceOrAppend(template[['context']], viz[['context']])
  context[['info']] <- replaceOrAppend(getBlocks("info", keep.block=F)[[1]], context[['info']])

  # flatten dependencies before lookups
  dependencies <- c(dependencies, recursive = TRUE)

  # replace ids in context with expanded dependencies
  context <- buildContext(context, dependencies)

  file <- export(viz)
  render(template, context, file)
  return(relativePath(file))
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
  dependencies <- gatherDependencyList(c(viz[['depends']], template[['depends']]))

  context <- replaceOrAppend(template[['context']], viz[['context']])
  context[['info']] <- replaceOrAppend(getBlocks("info", keep.block=F)[[1]], context[['info']])

  # flatten dependencies before lookups
  dependencies <- c(dependencies, recursive = TRUE)

  context <- buildContext(context, dependencies)

  viz[['output']] <- render(template, context)
  if (!is.null(viz[['analytics']])) {
    viz <- analytics(viz)
  }
  if (!is.null(viz[['embed']]) && isTRUE(viz[['embed']])) {
    file <- export(viz)
    setupFoldersForFile(file)

    embedTmpl <- template("embed")
    context[['embed']] <- viz[['output']]
    context[['resources']] <- lapply(context[['resources']], gsub, pattern = '(href="|src=")(css|js|images)', 
                                     replacement = '\\1../\\2')
    render(embedTmpl, data = context, file = file)

    # viz[['output']] <- wrapEmbed(viz[['output']])
    # wrap or add embed links to page
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
    img.class <- ifelse(is.null(viz[['class']]), "",
                        paste0(" class=\"",
                          paste0(viz[['class']], collapse=" "),
                        "\""))
    html <- sprintf('<img src="%s?_c=%s" alt="%s" title="%s"%s />', relative.path, uniqueness(),
                    alt.text, title.text, img.class)
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

#' Add a font to the page
#'
#' @rdname publish
#' @importFrom utils URLencode
#' @export
publish.googlefont <- function(viz) {
  required <- c("family", "weight")
  checkRequired(viz, required)

  families <- paste(URLencode(viz[["family"]]), collapse = "|")
  weights <- paste(viz[["weight"]], collapse = ",")
  googlefont <- "//fonts.googleapis.com/css"
  html <- sprintf('<link href="%s?family=%s:%s" rel="stylesheet" type="text/css">',
                  googlefont, families, weights)
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
  dependencies <- gatherDependencyList(c(viz[['depends']], template[['depends']]))

  context <- replaceOrAppend(template[['context']], viz[['context']])

  # flatten dependencies before lookups
  dependencies <- c(dependencies, recursive = TRUE)

  context <- buildContext(context, dependencies)

  #add info from viz.yaml to context to inject into template
  vizzies <- viz$vizzies
  for(v in 1:length(vizzies)){
    info <- getVizInfo(repo=vizzies[[v]]$repo, org=vizzies[[v]]$org)
    if (is.null(vizzies[[v]]$name)){ # don't replace it if it is already set
      vizzies[[v]]$name <- info$context$name
    }
    
    if(is.null(vizzies[[v]]$url)){
      vizzies[[v]]$url <- info$context$path
    }
    
    if(is.null(vizzies[[v]]$thumbLoc)){
      vizzies[[v]]$thumbLoc <- info$context$thumbnail
    }
    
  }
  context[['blogsInFooter']] <- viz$blogsInFooter
  context[['blogs']] <- viz$blogs
  context[['vizzies']] <- vizzies


  viz[['output']] <- render(template, data = context)
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
  # rm null
  viz_info <- viz_info[!sapply(viz_info, is.null)]
  # sort reverse chronological
  viz_info <- viz_info[order(sapply(viz_info, '[[', 'publish-date'), decreasing=TRUE)]

  pageviz <- viz
  names(pageviz$depends) <- pageviz$depends
  pageviz$depends <- as.list(pageviz$depends)
  pageviz$depends <- append(pageviz$depends, viz_info)
  pageviz$context <- list(sections = c("owiNav", "header", names(viz_info)), #names of section ids
                          resources = c("landingCSS", "owiCSS", "jquery", "appJS"),
                          header = "usgsHeader",
                          footer = "usgsFooter")
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
  # nothing for now
}

#' check dimensions and size, publish thumbnail
#'
#' @rdname publish
#' @export
publish.thumbnail <- function(viz){
  checkRequired(viz, required = c("for", "location"))
  #compliance
  #dimensions in pixels, file sizes in bytes!
  if(tolower(viz[['for']]) == "facebook") {
    maxSize <- 8388608
    checkHeight <- 820
    checkWidth <- 1560
  } else if(tolower(viz[['for']]) == "twitter") {
    maxSize <- 1048576
    checkHeight <- 300
    checkWidth <- 560
  } else { #landing
    maxSize <- 1048576
    checkHeight <- 400
    checkWidth <- 400
  }
  dims <- checkThumbCompliance(file = viz[['location']], maxSize = maxSize,
                       checkHeight = checkHeight, checkWidth = checkWidth)
  #send to other publishers if all ok
  viz <- NextMethod()
  viz[['url']] <- pastePaths(getVizURL(), viz[['relpath']])#need to add slash between?
  viz[['width']] <- dims[['width']]
  viz[['height']] <- dims[['height']]
}

#' helper to check thumbnail compliance
#' @importFrom imager load.image width height
#' @param file char Name of thumbnail file
#' @param maxSize numeric Max size in bytes
#' @param checkHeight numeric Height in pixels to enforce
#' @param checkWidth numeric Width in pixels to enforce
checkThumbCompliance <- function(file, maxSize, checkHeight, checkWidth) {
  fileSize <- file.info(file)
  im <- imager::load.image(file)
  width <- imager::width(im)
  height <- imager::height(im)
  if(fileSize > maxSize || width != checkWidth || height != checkHeight) {
    stop(paste("Thumbnail", file, "does not meet site requirements"))
  }

  return(c(width = width, height = height))
}

#' coerce to a publisher
#' @param viz object describing publisher
#' @param ... not used, just for consistency
#' @export
as.publisher <- function(viz, ...) {
  # default to a resource
  publisher <- ifelse(exists("publisher", viz), viz[['publisher']], "resource")
  class(viz) <- c("publisher", class(viz))
  if (publisher %in% c("resource", "thumbnail")) {
    viz <- as.resource(viz)
  } else if (publisher == "template") {
    viz <- as.template(viz)
  } else {
    class(viz) <- c(publisher, class(viz))
  }
  return(viz)
}

#' coerce to resource
#' @param viz vizlab object
#' @param ... not used, following convention
#' @importFrom utils packageName
#' @export
as.resource <- function(viz, ...) {
  required <- c("mimetype", "location")
  checkRequired(viz, required)

  mimetype <- viz[['mimetype']]
  resource <- lookupMimetype(mimetype)

  if (!file.exists(viz[['location']])) {
    internal <- system.file(viz[['location']], package = packageName())
    if (file.exists(internal)) {
      viz[['location']] <- internal
    }
  }
  if(length(resource) == 0){
    warning(mimetype, " will be treated as data: ", viz[['id']])
    resource <- "data"
  }
  if ("publisher" %in% names(viz) && viz[['publisher']] == "thumbnail") {
    class(viz) <- c("thumbnail","resource", class(viz))
  } else {
    class(viz) <- c(resource, "resource",class(viz))
  }

  return(viz)
}
