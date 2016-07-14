#' The OS path 
#' 
#' The locations in the operating system \code{PATH} environment variable.
#' @param sep String separator between directory levels in the output.
#' @param standardize Should the paths be standardized?
#' @param splitter The character to split the PATH environment variable on.
#' Defaults to a semi-colon on Windows systems and a colon elsewhere.
#' @return A character vector of paths.
#' @seealso \code{\link[base]{Sys.getenv}}
#' @examples
#' os_path()
#' @importFrom assertive.reflection is_windows
#' @importFrom assertive.types assert_is_a_bool
#' @importFrom assertive.types assert_is_a_string
#' @export
os_path <- function(sep = c("/", "\\"), standardize = TRUE, 
  splitter = if(is_windows()) ";" else ":")
{
  assert_is_a_bool(standardize)
  assert_is_a_string(splitter)
  
  path <- Sys.getenv("PATH")
  path <- if(!nzchar(path))
  {
    warning("The 'PATH' environment variable is unset or empty.")
    character()
  } else
  {
    strsplit(path, splitter)[[1]]
  }
  if(standardize)
  {
    standardize_path(path, sep = sep, include_names = FALSE)  
  } else path
}
