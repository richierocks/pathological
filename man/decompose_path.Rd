\name{decompose_path}
\alias{decompose_path}
\alias{get_extension}
\alias{recompose_path}
\alias{recompose_path.decomposed_path}
\alias{replace_extension}
\alias{strip_extension}
\title{Split a path into its components}
\usage{
decompose_path(x = dir())

get_extension(x = dir())

recompose_path(x, ...)

\method{recompose_path}{decomposed_path}(x, ...)

replace_extension(x = dir(), new_extension)

strip_extension(x = dir(), include_dir = TRUE)
}
\arguments{
  \item{x}{A character vector of file paths. Defaults to
  files in the current directory.}

  \item{include_dir}{Should the directory part of the path
  be included?}

  \item{new_extension}{A new extension to replace the
  existing ones.}

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
  "somedir/foo.tgz",         # single extension
  "another dir\\\\bar.tar.gz", # double extension
  "baz",                     # no extension
  "quux. quuux.tbz2",        # single ext, dots in filename
  R.home(),                  # a dir
  "~",                       # another dir
  "~/quuuux.tar.xz",         # a file in a dir
  "",                        # empty
  ".",                       # current dir
  "..",                      # parent dir
  NA_character_              # missing
)
(decomposed <- decompose_path(x))
get_extension(x)
strip_extension(x)
strip_extension(x, FALSE)
recompose_path(decomposed)
}

