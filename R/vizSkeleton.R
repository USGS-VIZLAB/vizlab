#' vizlab vizSkelton
#'
#' Create a skeleton for a new visualization
#'
#' @param name character vector containing viz name
#' @param path character vector of path to create skeleton
#' @examples
#' path <- file.path(tempdir(), "test")
#' vizSkeleton(name="test", path=path)
#' file.remove(path) # cleanup
#'
#' @importFrom methods getPackageName
#' @importFrom utils packageVersion
#' @export
vizSkeleton <- function (name = "cool-viz", path = ".") {
  safe.dir.create <- function(path) {
    if (!dir.exists(path) && !dir.create(path))
      stop(gettextf("cannot create directory '%s'", path),
           domain = NA)
  }

  # Create directory structure
  message("Creating directories ...", domain = NA)
  safe.dir.create(path)
  safe.dir.create(file.path(path, "data"))
  safe.dir.create(file.path(path, "images"))
  safe.dir.create(file.path(path, "figures"))
  safe.dir.create(layout_dir <- file.path(path, "layout"))
  safe.dir.create(file.path(layout_dir, "css"))
  safe.dir.create(file.path(layout_dir, "js"))
  safe.dir.create(file.path(layout_dir, "templates"))
  safe.dir.create(script_dir <- file.path(path, "scripts"))
  safe.dir.create(file.path(script_dir, "fetch"))
  safe.dir.create(file.path(script_dir, "process"))
  safe.dir.create(file.path(script_dir, "visualize"))
  safe.dir.create(file.path(script_dir, "read"))

  # Write viz.yaml
  if(file.exists(file.path(path, 'viz.yaml'))) {
    message("viz.yaml already exists; leaving as-is")
  } else {
    message("Creating viz.yaml ...", domain = NA)
    viz.yaml <- file(file.path(path, "viz.yaml"))
    cat("vizlab: ", "\"", as.character(packageVersion(getPackageName())), "\"\n",
        "info:\n",
        "  id: ", name, "\n",
        "  name: ", "Visualization long name","\n",
        "  date: ", format(Sys.Date(), "%Y-%m-%d"), "\n",
        "  publish-date: ", format(Sys.Date() + 14, "%Y-%m-%d"), "\n",
        "  path: ", "/", name, "\n",
        "  analytics-id: ", "UA-78530187-1", "\n",
        "  description: >-\n",
        "    Describe project here\n",
        "fetch:\n",
        "process:\n",
        "visualize:\n",
        "publish:\n",
        file = viz.yaml, sep = "")
    close(viz.yaml)
  }

  # Write .gitignore file
  if (file.exists(file.path(path, '.gitignore'))) {
    message(".gitignore already exists, leaving as-is")
  } else {
    message("Creating .gitignore")
    gitignore <- file(file.path(path, ".gitignore"))
    cat(
        "cache/\n",
        "target/\n",
        "user/\n",
        "figures/\n",
        "vizlab/\n",
        "makefile\n",
        file = gitignore, sep = "")
    close(gitignore)
  }
  
  # Copy LICENSE file
  if (file.exists(file.path(path, 'LICENSE'))) {
    message("LICENSE already exists, leaving as-is")
  } else {
    message("Copying LICENSE")
    license <- file.copy(from=system.file('LICENSE', package="vizlab"),
                         to=file.path(path, 'LICENSE'))
  }
  
  # Read-and-delete-me type file would be good to describe next steps
}
