#' Delete the remake data but not the file from a publish target
#'
#' Running this function should be sufficient to force the republishing of
#' target_names when you next run vizmake(). But it won't do anything to files
#' currently in target, even if some of them have become obsolete or seem that
#' they should be "unpublished" by this call. To get rid of files, if desired,
#' you should directly delete them.
#'
#' This function is similar to remake::delete but specific to vizlab publish
#' targets, which are complicated because we use a file outside of target/ to
#' indicate whether we've copied that file (and potentially many unnamned
#' others) into target/. The approach we take here is simply to delete remake's
#' data about the targets without deleting any files.
#'
#' @param target_names character vector of targets to unpublish (i.e., for which
#'   the remake data should be deleted without touching the files). The default
#'   is the full list of targets that are built with vizlab::publish.
#' @param verbose should messages be printed?
#' @export
unpublish <- function (target_names=yaml::read_yaml('remake.yaml')$targets$Publish$depends, verbose = TRUE) {
  remake_file <- "remake.yaml" # the vizlab remake file is always fixed at this name
  remake:::assert_character(target_names)
  obj <- remake:::remake(remake_file, verbose = verbose, load_sources = FALSE)
  for (t in target_names) {
    remove_publish_target(obj, t)
  }
}

#' Helper to unpublish. similar to remake:::remake_remove_target, but without file deletion
#' 
#' @param obj argument as in remake:::remake_remove_target
#' @param target_name argument as in remake:::remake_remove_target
#' @keywords internal
remove_publish_target <- function(obj, target_name) {
  remake:::assert_has_targets(target_name, obj)
  target <- obj$targets[[target_name]]
  store <- obj$store
  if (target$type == "file") {
    # did_remove_obj <- store$files$del(target$name) # this is what we're NOT doing that remake::delete does do
    did_remove_db <- store$db$del(target$name)
    did_remove <- did_remove_db # || did_remove_obj
  }
  else if (target$type == "object") {
    did_remove_obj <- store$objects$del(target$name)
    did_remove_db <- store$db$del(target$name)
    did_remove <- did_remove_obj || did_remove_db
  }
  else {
    stop("Not something that can be deleted")
  }
  if (did_remove) {
    status <- "DEL"
    fn <- if (target$type == "object") 
      "rm"
    else "file_not_touched"
    cmd <- sprintf("%s(\"%s\")", fn, target_name)
  }
  else {
    status <- ""
    cmd <- NULL
  }
  remake:::remake_print_message(obj, status, target_name, cmd, "round")
}
