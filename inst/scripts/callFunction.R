# This file defines a script, not a function, so that it can be easily run from 
# a makefile in R CMD BATCH mode, which preserves output in a log file for
# better debugging

Sys.getenv("R_LIBS_USER")
.libPaths()
sessionInfo()

# Read in command-line arguments passed in from the makefile and evaluate the
# assignments (e.g., argname=argvalue) in the current environment
args.text <- commandArgs(TRUE)
message(paste(c("Arguments:", args.text), collapse='\n'))
readArg <- function(arg.text, cmd.args) {
  lhs <- strsplit(arg.text, '=')[[1]][1]
  rhs <- substring(arg.text, nchar(lhs)+2)
  cmd.args[[lhs]] <- eval(parse(text=rhs))
  cmd.args
}
# read just the scripts argument to start with, so we can build an environment
# for evaluating other arguments
cmd.args <- readArg(grep('^scripts=', args.text, value=TRUE), cmd.args=list())

# Load package dependencies...actually, don't. these should be loaded with
# library() calls within the script dependencies.
# if(exists('pkgs', cmdargs)) {
#   message("Loading libraries:\n", paste("  ", cmdargs$pkgs, collapse="\n"))
#   sapply(cmdargs$pkgs, library, character.only=TRUE, logical.return=TRUE)
# }
library(vizlab) # vizlab will always be available

# Load script dependencies
if(exists('scripts', cmd.args)) {
  scripts <- do.call(c, lapply(cmd.args$scripts, function(script) {
    if(dir.exists(script)) 
      file.path(script, dir(script))
    else
      script
  }))
  message("Sourcing scripts:\n", paste("  ", scripts, collapse="\n"))
  sapply(scripts, source)
}

# Read in the rest of the arguments
nonscript.args <- grep('^scripts=', args.text, invert=TRUE, value=TRUE)
for(nsa in nonscript.args) {
  cmd.args <- readArg(nsa, cmd.args)
}

# Check that the required arguments are given
expected <- c('fun', 'funargs')
if(length(missingargs <- setdiff(expected, names(cmd.args))) > 0)
  stop("missing argument[s] to callFunction: ", paste(missingargs, collapse=", "))

# Call function
message("Running function")
do.call(cmd.args$fun, cmd.args$funargs)
