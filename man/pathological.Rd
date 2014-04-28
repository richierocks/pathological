\docType{package}
\name{pathological}
\alias{pathological}
\alias{pathological-package}
\title{pathological: utilities for paths, files and directories.}
\description{
This package contains utilities for manipulating paths,
files and directories.
}
\details{
\code{decompose_path} splits a path into the directory
name, filename without extension, and extension.
\code{strip_extension} and \code{get_extension} provide
shortcuts to the second and third parts of the filename.
\code{recompose_path} takes the result of
\code{decompose_path} and returns complete paths.

\code{dir_copy} copies the contents of a directory,
possibly recursively.
}
\author{
Richie Cotton
}
\keyword{package}

