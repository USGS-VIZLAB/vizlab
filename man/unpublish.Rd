% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unpublish.R
\name{unpublish}
\alias{unpublish}
\title{Delete the remake data but not the file from a publish target}
\usage{

  unpublish(target_names = yaml::read_yaml("remake.yaml")$targets$Publish$depends,
  verbose = TRUE)
}
\arguments{
\item{target_names}{character vector of targets to unpublish (i.e., for which
the remake data should be deleted without touching the files). The default
is the full list of targets that are built with vizlab::publish.}

\item{verbose}{should messages be printed?}
}
\description{
Running this function should be sufficient to force the republishing of
target_names when you next run vizmake(). But it won't do anything to files
currently in target, even if some of them have become obsolete or seem that
they should be "unpublished" by this call. To get rid of files, if desired,
you should directly delete them.
}
\details{
This function is similar to remake::delete but specific to vizlab publish
targets, which are complicated because we use a file outside of target/ to
indicate whether we've copied that file (and potentially many unnamned
others) into target/. The approach we take here is simply to delete remake's
data about the targets without deleting any files.
}
