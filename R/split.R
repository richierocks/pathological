
#' Split a path into directory components
#' 
#' Splits a character vector of paths into directory components.  The opposite  
#' of \code{\link[base]{file.path}}.
#' @param x A character vector of file paths. Defaults to files in the 
#' current directory.
#' @return A named list of character vectors containing the split paths.
#' @note Paths are split on forward and back slashes, except for double forward 
#' or back slashes at the start of (UNC) paths.  These are included in the first 
#' element of that split path.
#' @examples
#' (splits <- split_path(c(getwd(), "~", r_home())))
#' # Reverse the operation
#' sapply(splits, paste, collapse = "/")
#' @importFrom assertive.properties is_empty
#' @importFrom assertive.base coerce_to
#' @importFrom stats setNames
#' @export
split_path <- function(x = dir())
{
  if(is_empty(x))
  {
    return(setNames(list(), character()))
  }
  original_x <- x <- coerce_to(x, "character")
  x <- standardize_path(x)
  split_x <- strsplit(x, "(?<=[^/\\\\])[/\\\\]", perl = TRUE)
  setNames(split_x, original_x)
}
