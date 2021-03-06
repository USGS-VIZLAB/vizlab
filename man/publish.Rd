% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/publish.R
\name{publish}
\alias{publish}
\alias{publish.character}
\alias{publish.list}
\alias{publish.page}
\alias{publish.section}
\alias{publish.resource}
\alias{publish.json}
\alias{publish.tabular}
\alias{publish.img}
\alias{publish.ico}
\alias{publish.googlefont}
\alias{publish.js}
\alias{publish.css}
\alias{publish.svg}
\alias{publish.footer}
\alias{publish.social}
\alias{publish.header}
\alias{publish.landing}
\alias{publish.webpack}
\alias{publish.template}
\alias{publish.thumbnail}
\title{Peform the publish step to ready the viz for hosting}
\usage{
publish(viz)

\method{publish}{character}(viz)

\method{publish}{list}(viz)

\method{publish}{page}(viz)

\method{publish}{section}(viz)

\method{publish}{resource}(viz)

\method{publish}{json}(viz)

\method{publish}{tabular}(viz)

\method{publish}{img}(viz)

\method{publish}{ico}(viz)

\method{publish}{googlefont}(viz)

\method{publish}{js}(viz)

\method{publish}{css}(viz)

\method{publish}{svg}(viz)

\method{publish}{footer}(viz)

\method{publish}{social}(viz)

\method{publish}{header}(viz)

\method{publish}{landing}(viz)

\method{publish}{webpack}(viz)

\method{publish}{template}(viz)

\method{publish}{thumbnail}(viz)
}
\arguments{
\item{viz}{vizlab object or identifier}
}
\description{
Determine the type and dispatch to that method to produce
files to serve up as the final viz

from here on out will use svg-inject to get svg to dom
}
\details{
Sections are published individually but are returned invisibly
as text to be used directly.

This copies static resources to the target directory, and attaches
  the relative path to the file if a file was copied in.

  The job of minification or css precompiling could also be added here, but
  currently this is not handled.

  Also, templating the resources that make sense would be useful

also, svg will support landscape or portrait for mobile support
}
