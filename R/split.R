
#' Split a path into directory components
#' 
#' \code{split_path} splits a character vector of paths into directory 
#' components.  The opposite of \code{\link[base]{file.path}}.
#' \code{split_dir} is a convenience wrapper equivalent to \code{dir} + 
#' \code{split_path}, making it easy to \code{\link[utils]{View}} directory 
#' contents.
#' @param x A character vector. For \code{split_path}, this should contain file 
#' paths, and it defaults to files in the current directory.  For 
#' \code{split_dir}, this should contain directory paths, and it defaults to
#' the current directory.
#' @param simplify A logical value. If \code{TRUE}, the return value is 
#' simplified from a list to a matrix.
#' @param pattern A string containing a regular expression, to filter the files
#' that are returned.  Passed to \code{\link[base]{dir}}.
#' @param all.files Logical. If \code{TRUE}, files whose name starts with a dot 
#' are included.  Passed to \code{\link[base]{dir}}.
#' @param recursive Logical. If \code{TRUE}, files in subdirectories are 
#' included.  Passed to \code{\link[base]{dir}}.
#' @return Either a named list of character vectors containing the split paths,
#' or a matrix.  See \code{simplify} argument in Usage section.
#' @note Paths are split on forward and back slashes, except for double forward 
#' or back slashes at the start of (UNC) paths.  These are included in the first 
#' element of that split path.
#' @seealso \code{\link[base]{file.path}}, \code{\link[base]{dir}}
#' @examples
#' (splits <- split_path(c(getwd(), "~", r_home())))
#' # Reverse the operation
#' sapply(splits, paste, collapse = "/")
#' 
#' base_r_files <- split_dir(R.home(), pattern = "\\.R$")
#' \donttest{
#'   # Viewing not needed for testing purposes
#'   utils::View(base_r_files)
#' }
#' @importFrom assertive.properties is_empty
#' @importFrom assertive.base coerce_to
#' @importFrom stats setNames
#' @export
split_path <- function(x = dir(), simplify = FALSE)
{
  if(is_empty(x))
  {
    return(setNames(list(), character()))
  }
  original_x <- x <- coerce_to(x, "character")
  x <- standardize_path(x)
  split_x <- strsplit(x, "(?<=[^/\\\\])[/\\\\]", perl = TRUE) %>% 
    setNames(original_x)
  if(simplify)
  {
    list2mat(split_x)
  } else 
  {
    split_x
  }
}

#' @rdname split_path
#' @export
split_dir <- function(x = ".", pattern = NULL, all.files = TRUE, 
  recursive = TRUE, simplify = TRUE)
{
  files <- dir(
    x, 
    pattern    = pattern, 
    all.files  = all.files, 
    recursive  = recursive, 
    full.names = TRUE,
    no..       = TRUE
  )
  split_path(files, simplify = simplify)
}
