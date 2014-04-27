\name{decompose_path}
\alias{decompose_path}
\alias{get_extension}
\alias{recompose_path}
\alias{recompose_path.decomposed_path}
\alias{strip_extension}
\title{Split a path into its components}
\usage{
decompose_path(x = dir())

get_extension(x = dir())

recompose_path(x, ...)

\method{recompose_path}{decomposed_path}(x, ...)

strip_extension(x = dir())
}
\arguments{
  \item{x}{A character vector of file paths. Defaults to
  files in the current directory.}

  \item{...}{Not currently used.}
}
\value{
\code{decompose_path} returns a character matrix with three
columns named \code{"dirname"}, \code{"filename"} and
\code{"extension"}. \code{strip_extension} returns a
character vector of the second column, and
\code{get_extension} returns a character vector of the
third column. \code{recompose_path} returns a character
vector of paths.
}
\description{
\code{decompose_path} splits a path into the directory
name, filename without extension, and extension.
\code{strip_extension} and \code{get_extension} provide
shortcuts to the second and third parts of the filename.
\code{recompose_path} takes the result of
\code{decompose_path} and returns complete paths.
}
\examples{
x <- c(
  "somedir/foo.tgz",
  "another dir\\\\bar.tar.gz",
  "baz",
  "quux. quuux.tbz2",
  "~/quuuux.tar.xz",
  "",
  ".",
  "..",
  NA_character_
)
(decomposed <- decompose_path(x))
recompose_path(decomposed)
}

