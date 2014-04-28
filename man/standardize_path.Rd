\name{standardize_path}
\alias{standardise_path}
\alias{standardize_path}
\title{Standardize paths}
\usage{
standardize_path(x, sep = c("/", "\\\\"))

standardise_path(x, sep = c("/", "\\\\"))
}
\arguments{
  \item{x}{A character vector of paths.}

  \item{sep}{String separator betwen directory levels in
  the output.}
}
\value{
A character vector of paths, pointing to the same locations
as the input, but in a standardized form.
}
\description{
Standardi[sz]e path names so that they can be more easily
compared.
}
\examples{
standardize_path(c("~", R.home()))
standardize_path(c("~", R.home()), "\\\\")
}
\seealso{
\code{\link[base]{normalizePath}},
\code{\link[base]{path.expand}}
}

