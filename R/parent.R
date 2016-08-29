#' Get the parent dir
#' 
#' Gets the parent directory of the input.
#' @param x A character vector of file paths. 
#' @param sep String separator between directory levels in the output.
#' @return A character vector of parent directories of the input.
#' @note Missing values are returned as missing.  On Windows, the parent of a 
#' drive, e.g., \code{"c:/"} is itself.  Likewise, under Unix, the parent of 
#' \code{"/"} is itself.
#' @examples
#' (x <- c(
#'   sys_which("R"),
#'   r_home(),
#'   r_profile_site(),
#'   "c:/",  # different behaviour under Windows/Unix
#'   "~",
#'   "/",
#'   "foo/bar/nonexistent",
#'   NA
#' ))
#' parent_dir(x)
#' @importFrom stats setNames
#' @importFrom stringi stri_replace_all_fixed
#' @export
parent_dir <- function(x = ".", sep = c("/", "\\")) 
{
  sep <- match.arg(sep)
  original_x <- x <- coerce_to(x, "character")
  x <- standardize_path(x)
  not_missing <- is_not_na(x)
  win_drive <- suppressWarnings(is_windows_drive(x))
  pdir <- rep.int(NA_character_, length(x))
  pdir[not_missing] <- ifelse(
    win_drive[not_missing],
    x[not_missing],
    ifelse(
      strip_attributes(is_dir(x[not_missing])),
      dirname(x[not_missing]),
      dirname(dirname(x[not_missing]))
    )
  )
  if(sep == "\\")
  {
    pdir[not_missing] <- stri_replace_all_fixed(pdir[not_missing], "/", "\\")
  }
  setNames(pdir, original_x)
}
