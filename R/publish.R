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

  # gather a list of dependencies and IDs, publishing each dependency on the way
  dependencies <- gatherDependencyList(c(viz[['depends']], template[['depends']]))

  # also manually put resources into context
  context <- replaceOrAppend(template[['context']], viz[['context']])
  context[['info']] <- replaceOrAppend(getBlocks("info", keep.block=F)[[1]], context[['info']])
  context[['info']] <- updateThumbnails(context[['info']], context[['thumbnails']])
  # flatten dependencies before lookups
  dependencies <- c(dependencies, recursive = TRUE)

  # replace ids in context with expanded dependencies
  context <- buildContext(context, dependencies)

  file <- export(viz)
  render(template, context, file)
  return(relativePath(file))
}

#' Get thumbnail info for sematics
#' @param context list, should be just info section
#' @param thumbnails list, taken from full context
updateThumbnails <- function(context, thumbnails){
  
  thumb_names <- names(thumbnails)
  
  if("twitter" %in% thumb_names){
    twitter_thumb <- publish(thumbnails[["twitter"]])
    context[["thumbnail-twitter"]][["url"]] <- twitter_thumb[["url"]]
    context[["thumbnail-twitter"]][["alttext"]] <- twitter_thumb[["alttext"]]
    checkThumbCompliance(twitter_thumb[["width"]], 
                         twitter_thumb[["height"]], 
                         twitter_thumb[["size"]], 
                         "twitter")    
  }
  
  if("facebook" %in% thumb_names){
    face_thumb <- publish(thumbnails[["facebook"]])
    context[["thumbnail-facebook"]][["url"]] <- face_thumb[["url"]]
    context[["thumbnail-facebook"]][["height"]] <- face_thumb[["height"]]
    context[["thumbnail-facebook"]][["width"]] <- face_thumb[["width"]]
    context[["thumbnail-facebook"]][["type"]] <- face_thumb[["mimetype"]]
    
    checkThumbCompliance(face_thumb[["width"]], 
                         face_thumb[["height"]], 
                         face_thumb[["size"]], 
                         "facebook")
  }
  
  if("main" %in% thumb_names){
    main_thumb <- publish(thumbnails[["main"]])
    context[["thumbnail"]][["url"]] <- main_thumb[["url"]]
    context[["thumbnail"]][["height"]] <- main_thumb[["height"]]
    context[["thumbnail"]][["width"]] <- main_thumb[["width"]]
    context[["thumbnail"]][["alttext"]] <- main_thumb[["alttext"]]
    
    checkThumbCompliance(main_thumb[["width"]], 
                         main_thumb[["height"]], 
                         main_thumb[["size"]], "main")
  }

  
  if("landing" %in% thumb_names){
    landing_thumb <- publish(thumbnails[["landing"]])

    checkThumbCompliance(landing_thumb[["width"]], 
                         landing_thumb[["height"]], 
                         landing_thumb[["size"]], "landing")
  }
  
  return(context)
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

  }
  return(viz[['output']])
}

#' Publish a resource: determine whether a file should be exported (i.e., copied
#' to target/something/), copy the file, and update the viz item with the
#' relative path
#'
#' @details This copies static resources to the target directory, and attaches
#'   the relative path to the file if a file was copied in.
#'
#'   The job of minification or css precompiling could also be added here, but
#'   currently this is not handled.
#'
#'   Also, templating the resources that make sense would be useful
#'
#' @rdname publish
#' @export
publish.resource <- function(viz) {
  # figure out resource type and hand to resource handler
  destFile <- export(viz)
  if (!is.null(destFile)) {
    dir.create(dirname(destFile), recursive = TRUE, showWarnings = FALSE)
    srcFile <- viz[['location']]
    file.copy(srcFile, destFile, overwrite = TRUE)
    viz[['relpath']] <- relativePath(destFile)
  } else {
    viz[['relpath']] <- NA
  }
  return(viz)
}

#' JSON data publishing (json, geojson)
#'
#' @rdname publish
#' @export
publish.json <- function(viz) {
  required <- c("relpath", "mimetype")
  viz <- NextMethod()
  checkRequired(viz, required)
  
  # data should be read using AJAX or similar, with d3/jquery calls that
  # reference the data file and may be nested in a function chain or buried
  # within a multi-file queue. Because the code can vary widely by application,
  # we may often choose to hand-code the relative path, but this function
  # returns that string in case it can be templated in sometimes.
  html <- NULL
  if (!is.na(viz[['relpath']])) {
    html <- viz[['relpath']]
  }
  return(html)
}

#' Tabular data publishing (csv, tsv, etc.)
#'
#' @rdname publish
#' @export
publish.tabular <- function(viz) {
  required <- c("relpath", "mimetype")
  viz <- NextMethod()
  checkRequired(viz, required)
  
  # data should be read using AJAX or similar, with d3/jquery calls that
  # reference the data file and may be nested in a function chain or buried
  # within a multi-file queue. Because the code can vary widely by application,
  # we may often choose to hand-code the relative path, but this function
  # returns that string in case it can be templated in sometimes.
  html <- NULL
  if (!is.na(viz[['relpath']])) {
    html <- viz[['relpath']]
  }
  return(html)
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
    img.datasrc <- ifelse(is.null(viz[['data-src']]), "",
                         paste0(" data-src=\"",
                                paste0(viz[['data-src']], collapse=" "),
                                "\""))
    img.class <- ifelse(is.null(viz[['class']]), "",
                        paste0(" class=\"",
                          paste0(viz[['class']], collapse=" "),
                        "\""))
    html <- sprintf('<img src="%s?_c=%s" alt="%s" title="%s"%s%s />', relative.path, uniqueness(),
                    alt.text, title.text, img.datasrc, img.class)
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
#' from here on out will use svg-inject to get svg to dom
#'
#' also, svg will support landscape or portrait for mobile support
#'
#' @rdname publish
#' @export
publish.svg <- function(viz) {
  required <- c("relpath", "title", "alttext")
  viz <- NextMethod()
  checkRequired(viz, required)

  orientation = c()
  if (!is.null(viz[['orientation']]) && viz[['orientation']] == "landscape") {
    orientation <- "vizlab-landscape"
  } else if (!is.null(viz[['orientation']]) && viz[['orientation']] == "portrait"){
    orientation <- "vizlab-portrait"
  } else { # default or both
    orientation <- "vizlab-landscape vizlab-portrait"
  }

  if (!is.null(viz[['inline']])) {
    warning("inline option is deprecated, all SVGs now use svg-inject")
  }
  output <- NULL
  if (!is.na(viz[['relpath']])) {
    output <- sprintf('<img class="%s" src="%s" title="%s" alt="%s" />',
                      orientation, viz[['relpath']], viz[['title']], viz[['alttext']])
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
    info <- getVizInfo(repo=vizzies[[v]]$repo, org=vizzies[[v]]$org, dev=FALSE)
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
  context[['blogsInFooter']] <- viz[["blogsInFooter"]]
  context[['blogs']] <- viz[["blogs"]]
  context[['vizzies']] <- vizzies
  context[['github-url']] <- viz[["github-url"]]

  viz[['output']] <- render(template, data = context)
  if (!is.null(viz[['analytics']])) {
    viz <- analytics(viz)
  }
  return(viz[['output']])
}

#' Social icons
#' @importFrom utils download.file
#' @rdname publish
#' @export
publish.social <- function(viz) {

  template <- template(viz[['template']])
  
  context <- replaceOrAppend(template[['context']], viz[['context']])
  
  if("depends" %in% names(viz)){
    if("social-links" %in% viz[["depends"]]){
      links <- readDepends(viz)[["social-links"]]
      
      if(any(c("facebook","facebookLink") %in% names(links))){
        names(links)[names(links) == "facebookLink"] <- "facebook"
        context[["facebookLink"]] <- links[["facebook"]]
      }
      if(any(c("twitter","twitterLink") %in% names(links))){
        names(links)[names(links) == "twitterLink"] <- "twitter"
        context[["twitterLink"]] <- links[["twitter"]]
      }      
      if(any(c("github","githubLink") %in% names(links))){
        names(links)[names(links) == "githubLink"] <- "github"
        context[["githubLink"]] <- links[["github"]]
      }
      if(any(c("embed","embedLink") %in% names(links))){
        names(links)[names(links) == "embedLink"] <- "embed"
        context[["embedLink"]] <- links[["embed"]]
      } 

      viz[['depends']] <- viz[['depends']][viz[['depends']] != "social-links"]
      template[["depends"]] <- template[["depends"]][names(template[["depends"]]) != "social-links"]
    }
  }
  
  dependencies <- gatherDependencyList(c(viz[['depends']], template[['depends']]))
  # flatten dependencies before lookups
  dependencies <- c(dependencies, recursive = TRUE)
  
  context <- buildContext(context, dependencies)
  context[["mainEmbed"]] <- "embedLink" %in% names(context)
  
  viz[['output']] <- render(template, data = context)
  if (!is.null(viz[['analytics']])) {
    viz <- analytics(viz)
  }
  return(viz[['output']])
}

#' Header publishing
#' @rdname publish
#' @export
publish.header <- function(viz) {
  
  return(publish.section(viz))
}

#' publish landing page
#'
#' @rdname publish
#' @export
publish.landing <- function(viz){
  
  repos <- setdiff(getRepoNames(viz[['org']]), c('vizlab', 'D3Learners', 'viz-scratch')) # we know some aren't vizzies
  viz_info <- lapply(setNames(nm=repos), function(repo) {
    tryCatch(getVizInfo(repo, org=viz[['org']], viz[['dev']]),
             error=function(e) message(paste0("in getVizInfo(", repo, "): ", e$message), appendLF=TRUE)
             # warning=function(w) if(grepl("\\. is not a real", w$message)) return() else warning(w)
             )
  })
  # rm null
  viz_info <- viz_info[!sapply(viz_info, is.null)]
  # sort reverse chronological
  viz_info <- viz_info[order(sapply(viz_info, '[[', 'publish-date'), decreasing=TRUE)]

  pageviz <- viz
  names(pageviz$depends) <- pageviz$depends
  pageviz$depends <- as.list(pageviz$depends)
  pageviz$depends <- append(pageviz$depends, viz_info)
  pageviz$context <- list(sections = c("owiNav", "header", names(viz_info)), #names of section ids
                          resources = c("lib-vizlab-favicon", "landingCSS", "owiCSS", "jquery", "appJS"),
                          header = "usgsHeader",
                          footer = "usgsFooter", 
                          info = list(`analytics-id` = "UA-78530187-11"))
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
  
  required <- c("relpath", "title", "alttext")
  viz <- NextMethod()
  checkRequired(viz, required)
  stopifnot(tools::file_ext(viz[['location']]) == "png")
  
  im <- tryCatch({
    png::readPNG(viz[['location']])
  },
  error=function(cond){
    return(png::readPNG(system.file("testviz/images/landing-thumb.png", package = "vizlab")))
  })
  
  width <- dim(im)[2]
  height <- dim(im)[1]
  file_size  <- tryCatch({
    file.info(viz[['location']])
  },
  error=function(cond){
    return(file.info(system.file("testviz/images/landing-thumb.png", package = "vizlab")))
  })
  for(thumbType in unique(viz[['thumbType']])){
    dims <- checkThumbCompliance(width, height, file_size$size, thumbType)
  }

  viz[['url']] <- pastePaths(getVizURL(), viz[['relpath']])
  viz[['width']] <- width
  viz[['height']] <- height
  viz[['size']] <- file_size$size
  
  return(viz)
  
}

#' Took code from here:
#' https://stackoverflow.com/questions/10910688/converting-kilobytes-megabytes-etc-to-bytes-in-r
#' @param x character representing Xbytes to convert to bytes 
convb <- function(x){
  ptn <- "(\\d*(.\\d+)*)(.*)"
  num  <- as.numeric(sub(ptn, "\\1", x))
  unit <- sub(ptn, "\\3", x)             
  unit[unit==""] <- "1" 
  unit <- gsub(" ","", unit)
  unit <- toupper(unit)
  mult <- c("1"=1, "KB"=1024, "M"=1024^2, "G"=1024^3)
  num * unname(mult[unit])
}

#' helper to check thumbnail compliance
#' @importFrom png readPNG
#' @importFrom tools file_ext
#' @param width numeric pixal width of image
#' @param height numeric pixal height of image
#' @param size numeric file size in bytes
#' @param thumbType char Type of thumbnail, could be "facebook", "twitter", "landing", "main"

checkThumbCompliance <- function(width, height, size, thumbType){
  
  stopifnot(all(thumbType %in% c("facebook","twitter","main","landing")))
  
  minHeight <- NA
  minWidth <- NA
  maxSize <- NA
  maxHeight <- NA
  maxWidth <- NA
  exactHeight <- NA
  exactWidth <- NA
  
  if(thumbType == "facebook") {
    # On 10/26/2017:
    # Use images that are at least 1200 x 630 pixels for the best 
    #display on high resolution devices. At the minimum, you should
    #use images that are 600 x 315 pixels to display link page posts
    #with larger images. Images can be up to 8MB in size.
    #https://developers.facebook.com/docs/sharing/best-practices
    maxSize <- convb("8M")
    minHeight <- 630
    minWidth <- 1200
  } else if(thumbType == "twitter") {
    # On 10/26/2017:
    #A URL to a unique image representing the content of the page. 
    #You should not use a generic image such as your website logo, 
    #author photo, or other image that spans multiple pages. Images 
    #for this Card support an aspect ratio of 2:1 with minimum 
    #dimensions of 300x157 or maximum of 4096x4096 pixels. 
    #Images must be less than 5MB in size. JPG, PNG, WEBP and GIF 
    #formats are supported. Only the first frame of an animated GIF 
    #will be used. SVG is not supported.
    # https://developer.twitter.com/en/docs/tweets/optimize-with-cards/overview/summary-card-with-large-image
    maxSize <- convb("5M")
    maxHeight <- 4096
    maxWidth <- 4096
    minHeight <- 157
    minWidth <- 300
  } else if (thumbType == 'landing') { #landing
    maxSize <- 1048576
    exactHeight <- 400
    exactWidth <- 400
  } # didn't specify "main"....want to leave that flexible
  
  if(isTRUE(size > maxSize)){
    stop(paste("Thumbnail is too big"))
  }
  
  if(isTRUE(width > maxWidth || height > maxHeight)){
    stop(paste("Thumbnail is too big"))
  }
  
  if(isTRUE(width < minWidth || height < minHeight)){
    stop(paste("Thumbnail is too small"))
  }
  
  if(!is.na(exactWidth) & !is.na(exactHeight)){
    if(!isTRUE(width == exactWidth && height == exactHeight)){
      stop(paste("Thumbnail is too small"))
    }    
  }
  
  return(c(width = width, height = height, size=size))
}

#' coerce to a publisher
#' @param viz object describing publisher
#' @export
as.publisher <- function(viz) {
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
#' @importFrom utils packageName
#' @export
as.resource <- function(viz) {
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
    warning(mimetype, " is unknown and will be treated as data: ", viz[['id']])
    resource <- "data"
  }
  if ("publisher" %in% names(viz) && viz[['publisher']] == "thumbnail") {
    class(viz) <- c("thumbnail","resource", class(viz))
  } else {
    class(viz) <- c(resource, "resource",class(viz))
  }

  return(viz)
}
