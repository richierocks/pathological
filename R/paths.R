#' Make a path suitable for cygwin
#' 
#' By default, cygwin complains about standard paths.  This function converts 
#' paths to a form that cygwin likes.
#' @param x A character vector of file paths. Defaults to files in the 
#' current directory.
#' @return A character vector of the cygwinified inputs.
#' @seealso \code{standardize_path}
#' @examples
#' cygwinify_path(c("~", "\\\\some/network/drive"))
#' @export
cygwinify_path <- function(x)
{
  if(!assertive::is_windows())
  {
    warning(
      "This function is expecting to be run under windows, but the OS is ", 
      .Platform$OS.type
    )
  }
  cygwinified_x <- standardize_path(x)
  colon <- stringr::fixed(":")
  has_drive <- stringr::str_detect(cygwinified_x, colon)
  split_path <- stringr::str_split_fixed(cygwinified_x[has_drive], colon, 2L)
  cygwinified_x[has_drive] <- paste0(
    "/cygdrive/",
    split_path[, 1L],
    split_path[, 2L]
  )
  cygwinified_x
}

#' Split a path into its components
#' 
#' \code{decompose_path} splits a path into the directory name, filename 
#' without extension, and extension. \code{strip_extension} and 
#' \code{get_extension} provide shortcuts to the second and third parts
#' of the filename. \code{recompose_path} takes the result of 
#' \code{decompose_path} and returns complete paths.
#' @param x A character vector of file paths. Defaults to files in the 
#' current directory.
#' @param include_dir Should the directory part of the path be included?
#' @param new_extension A new extension to replace the existing ones.
#' @param ... Not currently used.
#' @return \code{decompose_path} returns a character matrix with three 
#' columns named \code{"dirname"}, \code{"filename"} and \code{"extension"}.
#' \code{strip_extension} returns a character vector of the second column,
#' and \code{get_extension} returns a character vector of the third column.
#' \code{recompose_path} returns a character vector of paths.
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
#' @export
decompose_path <- function(x = dir())
{
  if(assertive::is_empty(x))
  {
    return(
      structure(
        data.frame(
          dirname = character(), 
          filename = character(), 
          extension = character(),
          stringsAsFactors = FALSE
        ),
        class = c("decomposed_path", "data.frame")
      )
    )
  }
  original_x <- x <- assertive::coerce_to(x, "character")
  x <- standardize_path(x)
  not_missing <- assertive::is_not_na(x)
  is_dir_x <- assertive::is_dir(x)
  
  basename_x <- ifelse(
    not_missing,
    ifelse(is_dir_x, "", basename(x)),
    NA_character_
  )
  has_extension <- stringr::str_detect(basename_x, stringr::fixed("."))
    
  # match one or more letters, numbers and allowed punctuation characters
  # (the filename without extension)
  # then a single period
  # then match one of more letters numbers and periods
  # (the file extension)
  rx <- "^([][[:alnum:] `!@#$%^&()_=+{},.;'-]+?)\\.([[:alnum:].]+)$"
  
  filename_x <- ifelse(not_missing, basename_x, NA_character_)
  extension_x <- ifelse(not_missing, "", NA_character_)
  not_missing_and_has_extension <- not_missing & has_extension
  
  if(any(not_missing_and_has_extension))
  {
    split_name <- stringr::str_match(
      basename_x[not_missing_and_has_extension], 
      rx
    )
  
    filename_x[not_missing_and_has_extension] <- split_name[, 2L]
    extension_x[not_missing_and_has_extension] <- split_name[, 3L]
  }
  
  decomposed_x <- data.frame(
    dirname   = ifelse(
      not_missing,
      ifelse(is_dir_x, x, dirname(x)), 
      NA_character_
    ),
    filename  = filename_x, 
    extension = extension_x,
    row.names = ifelse(is.na(original_x), "<NA>", original_x),
    stringsAsFactors = FALSE
  )
  
  structure(decomposed_x, class = c("decomposed_path", "data.frame"))
}

#' Copy the contents of a directory
#' 
#' Copies the contents of a directory, possibly recursively.
#' @param source_dir String of directory to copy from.
#' @param target_dir String of directory to copy to.
#' @param pattern String regex or \code{NULL}. A filter for filenames, passed  
#' to \code{dir}.
#' @param overwrite Logical value.  Should existing files be overwritten?
#' @param recursive Logical value.  Should subdirectories and their contents 
#' be copied?
#' @note Target directories that don't exist are created, silently (assuming  
#' write permission).
#' @return A logical vector of whether or not each file was successfully  
#' copied is invisibly returned.
#' @seealso \code{\link[base]{basename}}
#' @examples
#' \dontrun{
#' #Copy subdirs by default
#' dir_copy(R.home("etc"), file.path(tempdir(), "etc"))
#' #Just copy the top level
#' dir_copy(R.home("etc"), file.path(tempdir(), "etc2"), recursive = FALSE)
#' #Now copy deeper levels, without overwriting.
#' dir_copy(R.home("etc"), file.path(tempdir(), "etc2"), overwrite = FALSE)
#' #Cleanup
#' unlink(file.path(tempdir(), "etc"), recursive = TRUE)
#' unlink(file.path(tempdir(), "etc2"), recursive = TRUE)
#' }
#' @export
dir_copy <- function(source_dir, target_dir, pattern = NULL, overwrite = FALSE, 
  recursive = TRUE)
{
  #Retrieve all file and directory names
  filenames <- dir(
    source_dir,
    pattern      = pattern, 
    recursive    = recursive, 
    all.files    = TRUE,
    full.names   = FALSE,
    include.dirs = TRUE
  )
  
  #Create missing directories, silently.
  is_directory <- assertive::is_dir(file.path(source_dir, filenames))
  directories <- c(target_dir, file.path(target_dir, filenames[is_directory]))
  plyr::tryapply(
    directories,
    dir.create,
    showWarnings = FALSE, 
    recursive    = recursive
  )
 
  out_dir <- file.path(target_dir, dirname(filenames[!is_directory]))
  out_dir <- gsub("/\\.$", "", out_dir)   
  
  if(length(out_dir) == 0) return()
  ok <- mapply(
    file.copy,
    from      = file.path(source_dir, filenames[!is_directory]), 
    to        = out_dir,
    overwrite = overwrite, 
    recursive = FALSE
  )
  if(!all(ok))
  {
    warning(
      "The files ", 
      toString(sQuote(filenames[!ok])), 
      " were not copied successfully."
    )
  }
  names(ok) <- filenames[!is_directory]
  invisible(ok)
}

#' @rdname decompose_path
#' @export
get_extension <- function(x = dir())
{
  decompose_path(x)[, 3L]
}

#' @rdname decompose_path
#' @export
recompose_path <- function(x, ...)
{
  UseMethod("recompose_path")
}

#' @rdname decompose_path
#' @method recompose_path decomposed_path
#' @export
recompose_path.decomposed_path <- function(x, ...)
{
  not_missing <- assertive::is_not_na(x[, "filename"])
  has_an_extension <- nzchar(as.character(x[not_missing, "extension"]))
  path <- rep.int(NA_character_, nrow(x))
  base_x <- ifelse(
    has_an_extension,
    paste(x[not_missing, "filename"], x[not_missing, "extension"], sep = "."),
    x[not_missing, "filename"]
  )
  path[not_missing] <- file.path(x[not_missing, "dirname"], base_x)
  path
}

#' @rdname decompose_path
#' @export
replace_extension <- function(x = dir(), new_extension)
{
  paste(strip_extension(x), new_extension, sep = ".")
}

#' Standardize paths
#' 
#' Standardi[sz]e path names so that they can be more easily compared.
#' @param x A character vector of paths.
#' @param sep String separator betwen directory levels in the output.
#' @return A character vector of paths, pointing to the same locations as the
#' input, but in a standardized form.
#' @seealso \code{\link[base]{normalizePath}}, \code{\link[base]{path.expand}}
#' @examples
#' standardize_path(c(".", "..", "~", R.home(), NA))
#' standardize_path(c(".", "..", "~", R.home(), NA), "\\")
#' @export
standardize_path <- function(x = dir(), sep = c("/", "\\"))
{
  if(assertive::is_empty(x))
  {
    return(character())
  }
  sep <- match.arg(sep)
  x <- assertive::coerce_to(x, "character")
  ifelse(
    is.na(x),
    NA_character_,
    normalizePath(path.expand(x), sep, FALSE)
  )
}

#' @rdname standardize_path
#' @export
standardise_path <- standardize_path

#' @rdname decompose_path
#' @export
strip_extension <- function(x = dir(), include_dir = TRUE)
{
  decomposed <- decompose_path(x)
  if(include_dir) 
  {
    ifelse(
      is.na(x),
      NA_character_,
      file.path(decomposed[, 1L], decomposed[, 2L])
    )
  } else
  {
    decomposed[, 2L]
  }
}
