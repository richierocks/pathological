#' Standardize paths
#' 
#' Standardi[sz]e path names so that they can be more easily compared.
#' @param x A character vector of file paths. Defaults to files in the 
#' current directory.
#' @param sep String separator between directory levels in the output.
#' @param include_names A logical value indicating whether the output should be 
#' named with the input file paths.
#' @return A character vector of paths, pointing to the same locations as the
#' input, but in a standardized form.
#' @details \code{standardize_path} wraps \code{\link[base]{normalizePath}},
#' providing additional tweaks to the output.
#' \itemize{
#' \item{Missing inputs always return \code{NA_character_}.}
#' \item{Leading double back slashes are preserved under all OSes regardless of
#' the values of \code{sep}.}
#' \item{Leading double forward slashes are converted to double back slash under
#' Windows (they are likely UNC paths), and a single forward slash under Unixes
#' (they are likely absolute paths).}
#' \item{Other back and forward slashes are replaced by \code{sep}.}
#' \item{Paths are always made absolute.}
#' \item{Trailing slashes are always stripped, except for root (\code{"/"}) and
#' Windows drives (\code{"C:/"}, etc.).}
#' \item{Windows drives are always capitalized.}
#' }
#' @seealso \code{\link[base]{normalizePath}}, \code{\link[base]{path.expand}},
#' \code{\link[R.utils]{getAbsolutePath}}
#' @examples
#' standardize_path(c(".", "..", "~", R.home(), NA))
#' standardize_path(c(".", "..", "~", R.home(), NA), "\\")
#' @importFrom assertive.base coerce_to
#' @importFrom assertive.properties is_empty
#' @importFrom assertive.reflection is_unix
#' @importFrom assertive.reflection is_windows
#' @importFrom assertive.strings is_non_missing_nor_empty_character
#' @importFrom stats setNames
#' @importFrom stringi stri_replace_first_regex
#' @importFrom stringi stri_replace_all_fixed
#' @importFrom stringi stri_detect_regex
#' @export
standardize_path <- function(x = dir(), sep = c("/", "\\"), include_names = TRUE)
{
  if(is_empty(x))
  {
    return(setNames(character(), character()))
  }
  sep <- match.arg(sep)
  x <- original_x <- coerce_to(x, "character")
  
  ok <- is_non_missing_nor_empty_character(x)

  # normalizePath gives a silly result for "c:" under Windows, returning either
  # r_home("bin") or temp_dir() or getwd()
  # Convert it to "c:/" to fix.  Unclear if this affects only the OS drive
  # or all mapped drives.  For safety, add a suffix to them all.
  is_slashless_windows_drive <- stri_detect_regex(x[ok], "^[a-zA-Z]:$")
  x[ok][is_slashless_windows_drive] <- paste0(x[ok][is_slashless_windows_drive], "/")
  
  # Normalize, with smarter defaults, and returning NA for NA inputs.
  x[ok] <- ifelse(
    is.na(x[ok]),
    NA_character_,
    normalizePath(x[ok], "/", mustWork = FALSE)
  )
  
  # Under Unix, normalizePath treats backslashes as characters in a filename.
  # It's often more useful to assume these are Windows file separators, that is,
  # We should replace all backslashes with (forward) slashes...
  # except leading double backslashed which are considered as UNC paths.
  # This code needs to come after the call to normalizePath, which has smart
  # OS-dependent detection of UNC paths.  That is:
  # Under Windows, normalizePath will have changed a leading // to \\.
  # Under Unix a leading // is 
  # - considered as an absolute path and changed to / if the path exists
  # - left as // if it doesn't exist
  # A leading \\ is preserved under all platforms.
  x[ok] <- ifelse(
    stri_detect_regex(x[ok], "^\\\\\\\\"), 
    paste0("\\\\", stri_replace_all_fixed(substring(x[ok], 3), "\\", "/")), 
    stri_replace_all_fixed(x[ok], "\\", "/")
  )
  
  # Turn leading // into / when normalizePath forgot to.
  if(is_unix())
  {
    has_leading_double_slash <- stri_detect_regex(x[ok], "^/{2}")
    x[ok][has_leading_double_slash] <- substring(x[ok][has_leading_double_slash], 2)
  }
  
  # Again under Unix, normalizePath won't always make path absolute
  if(is_unix())
  {
    is_absolute <- stri_detect_regex(x[ok], "^([/\\\\]|[a-zA-Z]:)")
    x[ok][!is_absolute] <- file.path(getwd(), x[ok], fsep = "/")
  }
  
  # Strip trailing slashes, except if it's a root dir
  # Root dir is either:
  #   / (Unix root dir) 
  #   an ascii letter then : then maybe a \ or / (Windows drive)
  # Usually \ (Windows current drive) too, but this will have been replaced by
  # normalizePath.
  is_root <- stri_detect_regex(x[ok], "^(/|[a-zA-Z]:[/\\\\]?)$")
  x[ok][!is_root] <- stri_replace_first_regex(x[ok][!is_root], "/?$", "")
  # Root dirs should always end in /
  needs_a_slash <- stri_detect_regex(x[ok][is_root], "[^/]$")  
  x[ok][is_root][needs_a_slash] <- paste0(x[ok][is_root][needs_a_slash], "/")
  
  # Windows drive paths are sometimes but not always converted to upper case
  # Make this consistently happen.
  # At this point, the path should always start with a letter or a slash
  x[ok] <- paste0(toupper(substring(x[ok], 1, 1)), substring(x[ok], 2))
  
  # Replace / with the chosen slash
  if(sep == "\\")
  {
    x[ok] <- stri_replace_all_fixed(x[ok], "/", "\\")
  }
  if(include_names)
  {
    setNames(x, original_x) 
  } else
  {
    unname(x)
  }
}

#' @rdname standardize_path
#' @export
standardise_path <- standardize_path
