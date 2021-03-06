% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table1c.R
\name{read_from_spec}
\alias{read_from_spec}
\title{Read a dataset from a data specification file}
\usage{
read_from_spec(spec, read.fun = read.csv, file = NULL, ...)
}
\arguments{
\item{spec}{Either the name of a YAML file that contains the data
specification, or an equivalent object (i.e. as returned by
\code{\link[yaml]{read_yaml}}).}

\item{read.fun}{A function that reads in data. It should accept a file name
as its first argument and return some sort of
\code{\link[base]{data.frame}}.}

\item{file}{The name of the file containing the data. if \code{NULL}, then
the file name will be taken from the \code{dataset} item of the data
specification.}

\item{...}{Additional arguments that get passed to \code{read.fun}.}
}
\value{
A \code{\link[base]{data.frame}}, as returned by \code{read.fun},
that has been augmented with meta-information from the data specification
(i.e. labels, factor levels).
}
\description{
This convenience function can be used to read a dataset and augment it with
meta-information from a specification file. The specification file is a YAML
file with certain format. Using the function allows a cleaner separation of
script logic from data and meta-data, resulting in more generic and
re-usable scripts (the data and meta-data can be centralized and segregated
from the scripting logic).
}
