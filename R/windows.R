#' Make a path suitable for cygwin
#' 
#' By default, cygwin complains about standard paths.  This function converts 
#' paths to a form that cygwin likes.
#' @param x A character vector of file paths. Defaults to files in the 
#' current directory.
#' @return A character vector of the cygwinified inputs.
#' @seealso \code{standardize_path}
#' @examples
#' \donttest{
#' # Connecting to a non-existent network drive is slow
#' cygwinify_path(c("c:/Program Files", "\\\\some/network/drive"))
#' }
#' @importFrom assertive.reflection is_windows
#' @importFrom stringi stri_detect_fixed
#' @importFrom stringi stri_split_fixed
#' @export
cygwinify_path <- function(x = dir())
{
  if(!is_windows())
  {
    warning(
      "This function is expecting to be run under Windows, but the OS is ", 
      .Platform$OS.type,
      ".  Returning x untouched."
    )
    return(invisible(x))
  }
  cygwinified_x <- standardize_path(x)
  has_drive <- stri_detect_fixed(cygwinified_x, ":")
  split_path <- stri_split_fixed(cygwinified_x[has_drive], ":", 2L, simplify = TRUE)
  cygwinified_x[has_drive] <- paste0(
    "/cygdrive/",
    split_path[, 1L],
    split_path[, 2L]
  )
  cygwinified_x
}

#' On Windows, return the drive of the path
#' 
#' On a Windows system, this returns the drive letter of the path followed by a 
#' colon.  On other systems, it returns a single forward slash.
#' @param x A character vector of file paths. Defaults to the current directory.
#' @return A character vector of drive paths on Windows systems, or forward 
#' slashes on Unix-based systems.
#' @seealso \code{\link{is_windows_drive}}
#' @examples
#' \donttest{ # often takes > 5s to run
#' get_windows_drive(c(".", "~", r_home(), temp_dir(), "\\\\foo/bar"))
#' }
#' @importFrom assertive.reflection is_windows
#' @importFrom utils head
#' @export
get_windows_drive <- function(x = getwd())
{
  if(is_windows())
  {
    vapply(strsplit(standardize_path(x), "/"), head, character(1), n = 1)
  } else
  {
    rep.int("/", length(x))
  }
}

#' @rdname get_windows_drive
#' @export
get_drive <- function(x = getwd())
{
  .Deprecated("get_windows_drive")
  get_windows_drive(x)
}

#' Is the path a Windows drive?
#' 
#' Checks to see if the path is a Windows drive.
#' @param x A character vector of file paths. Defaults to files in the 
#' current directory.
#' @return A logical vector, \code{TRUE} when the path is a Windows drive name.
#' On non-Windows machines, the return value is \code{FALSE} everywhere.
#' @note The check is done by regular expression: values are considered to be 
#' Windows drive name if they consist of a letter followed by a colon, 
#' optionally followed by a slash or backslash.
#' Paths are standardardized before checking, so \code{.} and \code{..} are 
#' resolved to their actual locations rather than always returning \code{FALSE}.
#' @seealso \code{\link{get_drive}}
#' @examples
#' x <- c("c:", "c:/", "c:\\", "C:", "C:/", "C:\\", "c:/c", "cc:", NA)
#' # Warnings about OS suppressed so package checks pass on non-Windows systems.
#' suppressWarnings(is_windows_drive(x))
#' @importFrom assertive.reflection is_windows
#' @importFrom assertive.base coerce_to
#' @importFrom stats setNames
#' @importFrom stringi stri_detect_regex
#' @export
is_windows_drive <- function(x)
{
  if(!is_windows())
  {
    warning(
      "This function is expecting to be run under Windows, but the OS is ", 
      .Platform$OS.type,
      "."
    )
    return(rep.int(FALSE, length(x)))
  }
  original_x <- x <- coerce_to(x, "character")
  # Want to resolve paths with . or ..
  starts_with_dots <- stri_detect_regex(x, "^\\.{1,2}[/\\\\]?")
  # Can't use standardize_path since we want that fn to use this
  x[starts_with_dots] <- normalizePath(x[starts_with_dots]) 
  yn <- stri_detect_regex(x, "^[[:alpha:]]:[/\\\\]?$")
  setNames(yn, original_x)
}
