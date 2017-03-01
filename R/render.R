#' Render contents to file
#'
#' @param viz vizlab object
#' @param data to render
#' @param file file to render to
#' @export
render <- function(viz, data, file) UseMethod("render")

#' Renter template, return output if file not specified
#'
#' @rdname render
#' @importFrom whisker whisker.render
#' @export
render.template <- function(viz, data, file = NULL) {
  partials <- sapply(viz[['partials']], function(x) {
    t <- template(x)
    return(t[['template']])
  })

  out <- whisker.render(template = viz[['template']], data = data, partials = partials)
  if (!is.null(file)) {
    cat(out, file = file)
  } else {
    return(out)
  }
}
