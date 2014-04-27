\name{dir_copy}
\alias{dir_copy}
\title{Copy the contents of a directory}
\usage{
dir_copy(source_dir, target_dir, pattern = NULL, overwrite = FALSE,
  recursive = TRUE)
}
\arguments{
  \item{source_dir}{String of directory to copy from.}

  \item{target_dir}{String of directory to copy to.}

  \item{pattern}{String regex or \code{NULL}. A filter for
  filenames, passed to \code{dir}.}

  \item{overwrite}{Logical value.  Should existing files be
  overwritten?}

  \item{recursive}{Logical value.  Should subdirectories
  and their contents be copied?}
}
\value{
A logical vector of whether or not each file was
successfully copied is invisibly returned.
}
\description{
Copies the contents of a directory, possibly recursively.
}
\note{
Target directories that don't exist are created, silently
(assuming write permission).
}
\examples{
\dontrun{
#Copy subdirs by default
dir_copy(R.home("etc"), file.path(tempdir(), "etc"))
#Just copy the top level
dir_copy(R.home("etc"), file.path(tempdir(), "etc2"), recursive = FALSE)
#Now copy deeper levels, without overwriting.
dir_copy(R.home("etc"), file.path(tempdir(), "etc2"), overwrite = FALSE)
#Cleanup
unlink(file.path(tempdir(), "etc"), recursive = TRUE)
unlink(file.path(tempdir(), "etc2"), recursive = TRUE)
}
}
\seealso{
\code{\link[base]{basename}}
}

