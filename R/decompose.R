#' Split a path into its components
#' 
#' \code{decompose_path} splits a path into the directory name, filename 
#' without extension, and extension. \code{strip_extension}, 
#' \code{get_extension} and \code{replace_extension} provide shortcuts to 
#' manipulate the file extension. \code{recompose_path} takes the result of 
#' \code{decompose_path} and returns complete paths.
#' @param x A character vector of file paths. Defaults to files in the 
#' current directory.
#' @param new_extension A new extension to replace the existing ones.
#' @param include_dir Should the directory part of the path be included? If 
#' \code{NA}, the default, keep the directory from the input.  If \code{TRUE},
#' standardize the directory.  If \code{FALSE}, strip the directory.
#' @param ... Not currently used.
#' @return \code{decompose_path} returns a character matrix with three 
#' columns named \code{"dirname"}, \code{"filename"} and \code{"extension"}.
#' \code{strip_extension} returns a character vector of the filename, possibly 
#' with a directory (see \code{include_dir} argument).
#' \code{replace_extension} returns a character vector of the filename with a  
#' newextension, possibly with a directory (see \code{include_dir} argument).
#' \code{get_extension} returns a character vector of the third column.
#' \code{recompose_path} returns a character vector of paths.
#' @note Decomposing and then recomposing a path is usually equivalent to 
#' standardizing that path (though slower).  That is, usually
#' \code{recompose_path(decompose_path(x)) == standardize_path(x)}.
#' One exception to this is when the directory of x is a symbolic link to 
#' another directory.  In this case \code{decompose_path} will follow the
#' link but \code{standardize_path} won't.
#' @section Warning:
#' A few of the tests for this function don't pass under the CRAN Windows 
#' machine.  It is unclear exactly why this is happening, and the failing tests
#' have not been reproduced elsewhere.  If you have unexpected behaviour with
#' this function, please report it on the package issue tracker.
#' \url{https://github.com/richierocks/pathological/issues}
#' @seealso \code{file_ext} on the \code{\link[tools]{fileutils}} page, 
#' a primitive version of \code{get_extension}, and \code{\link[fs]{path_file}}
#' @examples
#' x <- c(
#'   "somedir/foo.tgz",         # single extension
#'   "another dir\\bar.tar.gz", # double extension
#'   "baz",                     # no extension
#'   "quux. quuux.tbz2",        # single ext, dots in filename
#'   R.home(),                  # a dir
#'   "~",                       # another dir
#'   "~/quuuux.tar.xz",         # a file in a dir
#'   "",                        # empty 
#'   ".",                       # current dir
#'   "..",                      # parent dir
#'   NA_character_              # missing
#' )
#' (decomposed <- decompose_path(x))
#' get_extension(x)
#' strip_extension(x)
#' strip_extension(x, FALSE)
#' recompose_path(decomposed)
#' @importFrom assertive.properties is_empty
#' @importFrom assertive.base coerce_to
#' @importFrom assertive.base is_not_na
#' @importFrom assertive.files is_dir
#' @importFrom assertive.base strip_attributes
#' @importFrom stringi stri_detect_fixed
#' @importFrom stringi stri_match_first_regex
#' @importFrom stringi stri_replace_first_regex
#' @export
decompose_path <- function(x = dir())
{
  if(is_empty(x))
  {
    return(
      structure(
        data.frame(
          dirname          = character(), 
          filename         = character(), 
          extension        = character(),
          stringsAsFactors = FALSE,
          row.names        = character()
        ),
        class = c("decomposed_path", "data.frame")
      )
    )
  }
  original_x <- x <- coerce_to(x, "character")
  x <- standardize_path(x)
  not_missing <- strip_attributes(is_not_na(x))
  is_dir_x <- is_dir(x)
  
  basename_x <- ifelse(
    not_missing,
    ifelse(is_dir_x, "", basename(x)),
    NA_character_
  )
  has_extension <- stri_detect_fixed(basename_x, ".")
    
  # match one or more letters, numbers and allowed punctuation characters
  # (the filename without extension)
  # then a single period
  # then match one of more letters numbers and periods
  # (the file extension)
  rx <- "^([]\\[[:alnum:] `!@#$%^&()_=+{},.;'-]+?)\\.([[:alnum:].]+)$"
  
  filename_x <- ifelse(not_missing, basename_x, NA_character_)
  extension_x <- ifelse(not_missing, "", NA_character_)
  not_missing_and_has_extension <- not_missing & has_extension
  
  if(any(not_missing_and_has_extension))
  {
    split_name <- stri_match_first_regex(
      basename_x[not_missing_and_has_extension], 
      rx
    )
  
    filename_x[not_missing_and_has_extension] <- split_name[, 2L]
    extension_x[not_missing_and_has_extension] <- split_name[, 3L]
  }
  
  decomposed_x <- data.frame(
    dirname   = ifelse(
      not_missing,
      ifelse(is_dir_x, x, standardize_path(dirname(x))), #restandardisation required
      NA_character_
    ),
    filename  = filename_x, 
    extension = extension_x,
    row.names = ifelse(is.na(original_x), "<NA>", original_x),
    stringsAsFactors = FALSE
  )
  
  structure(decomposed_x, class = c("decomposed_path", "data.frame"))
}

#' @rdname decompose_path
#' @importFrom stats setNames
#' @export
get_extension <- function(x = dir())
{
  setNames(decompose_path(x)$extension, x)  
}

#' @rdname decompose_path
#' @export
recompose_path <- function(x, ...)
{
  UseMethod("recompose_path")
}

#' @rdname decompose_path
#' @method recompose_path decomposed_path
#' @importFrom assertive.base is_not_na
#' @export
recompose_path.decomposed_path <- function(x, ...)
{
  not_missing <- is_not_na(x$filename)
  has_an_extension <- nzchar(as.character(x[not_missing, "extension"]))
  path <- rep.int(NA_character_, nrow(x))
  base_x <- ifelse(
    has_an_extension,
    paste(x[not_missing, "filename"], x[not_missing, "extension"], sep = "."),
    x[not_missing, "filename"]
  )
  has_a_dir <- nzchar(as.character(x[not_missing, "dirname"]))
  path[not_missing] <- ifelse(
    has_a_dir,
    file.path(x[not_missing, "dirname"], base_x),
    base_x
  )
  # strip trailing slashes
  path <- stri_replace_first_regex(path, "/?$", "")  
  
  path
}

#' @rdname decompose_path
#' @importFrom assertive.files is_dir
#' @importFrom assertive.types assert_is_a_bool
#' @importFrom assertive.types assert_is_character
#' @importFrom assertive.base strip_attributes
#' @importFrom stats setNames
#' @export
replace_extension <- function(x = dir(), new_extension, include_dir = NA)
{
  assert_is_character(new_extension)
  assert_is_a_bool(include_dir)
  if(!nzchar(new_extension))
  {
    warning("'new_extension' is empty.  Did you want strip_extension instead?")
  }
  is_dir_x <- strip_attributes(is_dir(x))
  if(any(is_dir_x))
  {
    warning(
      "The directories ", 
      toString(sQuote(x[is_dir_x])), 
      " have no file extensions to replace."
    )
  }
  stripped <- strip_extension(x, include_dir = include_dir)
  setNames(
    ifelse(
      is_dir_x,
      stripped,
      paste(stripped, new_extension, sep = ".")
    ),
    names(stripped)
  )
}

#' @rdname decompose_path
#' @importFrom assertive.files is_dir
#' @export
strip_extension <- function(x = dir(), include_dir = NA)
{
  # Empty string x gets returned as empty string
  stripped <- character(length(x))
  # Missing x gets returned as NA
  stripped[is.na(x)] <- NA_character_
  
  #Everything else
  ok <- nzchar(x) & !is.na(x)
  decomposed <- decompose_path(x[ok])
  
  stripped[ok] <- if(is.na(include_dir))
  {
    # For include_dir = NA, keep directory same as input
    dirname_x <- ifelse(is_dir(x[ok]), x[ok], dirname(x[ok]))
    ifelse(
      nzchar(decomposed$filename),
      ifelse(
        dirname_x == ".",
        decomposed$filename,                              # no directory
        file.path(dirname_x, decomposed$filename)         # both
      ),
      dirname_x                                           # no filename
    )    
  } else if(include_dir) 
  {
    # For include_dir = TRUE, add standardized directory
    ifelse(
      nzchar(decomposed$filename),
      file.path(decomposed$dirname, decomposed$filename), # both
      decomposed$dirname                                  # no filename
    )
  } else
  {
    # For include_dir = FALSE, strip directory
    decomposed$filename
  }
  setNames(stripped, x)
}
