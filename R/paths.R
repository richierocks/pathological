#' Split a path into its components
#' 
#' \code{decompose_path} splits a path into the directory name, filename 
#' without extension, and extension. \code{strip_extension} and 
#' \code{get_extension} provide shortcuts to the second and third parts
#' of the filename. \code{recompose_path} takes the result of 
#' \code{decompose_path} and returns complete paths.
#' @param x A character vector of file paths. Defaults to files in the 
#' current directory.
#' @param ... Not currently used.
#' @return \code{decompose_path} returns a character matrix with three 
#' columns named \code{"dirname"}, \code{"filename"} and \code{"extension"}.
#' \code{strip_extension} returns a character vector of the second column,
#' and \code{get_extension} returns a character vector of the third column.
#' \code{recompose_path} returns a character vector of paths.
#' @examples
#' x <- c(
#'   "somedir/foo.tgz", 
#'   "another dir\\bar.tar.gz", 
#'   "baz", 
#'   "quux. quuux.tbz2", 
#'   "~/quuuux.tar.xz",
#'   "", 
#'   ".",
#'   "..",
#'   NA_character_
#' )
#' (decomposed <- decompose_path(x))
#' recompose_path(decomposed)
#' @export
decompose_path <- function(x = dir())
{
  x <- assertive::coerce_to(x, "character")
  base_x <- basename(x)
  not_missing <- assertive::is_not_na(base_x)
  has_an_extension <- !(base_x %in% c(".", "..")) &
    stringr::str_detect(base_x, stringr::fixed("."))
  
  decomposed_x <- cbind(
    dirname   = dirname(x), 
    filename  = base_x, 
    extension = ifelse(not_missing, "", NA_character_)
  )
  if(length(base_x) > 0L)
  {  
    rownames(decomposed_x) <- x
    decomposed_x[not_missing & has_an_extension, 2L:3L] <- stringr::str_match(
      base_x[not_missing & has_an_extension], 
      "([][\\-^!\"#$%&'\\(\\)+,.;=@_`{}~ [:alnum:]]+?)\\.([[:alnum:].]+)"
    )[, 2L:3L]
  }
  structure(decomposed_x, class = c("decomposed_path", "matrix"))
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
dir_copy <- function(source_dir, target_dir, pattern = NULL, overwrite = FALSE, recursive = TRUE)
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
  has_an_extension <- nzchar(x[not_missing, "extension"])
  path <- rep.int(NA_character_, nrow(x))
  base_x <- ifelse(
    has_an_extension,
    paste(x[not_missing, "filename"], x[not_missing, "extension"], sep = "."),
    x[not_missing, "filename"]
  )
  path[not_missing] <- file.path(x[not_missing, "dirname"], base_x)
  path
}

#' Standardize paths
#' 
#' Standari[sz]e path names so that they can be more easily compared.
#' @param x A character vector of paths.
#' @param sep String separator betwen directory levels in the output.
#' @return A character vector of paths, pointing to the same locations as the
#' input, but in a standardized form.
#' @seealso \code{\link[base]{normalizePath}}, \code{\link[base]{path.expand}}
#' @examples
#' standardize_path(c("~", R.home()))
#' standardize_path(c("~", R.home()), "\\")
#' @export
standardize_path <- standardise_path <- function(x, sep = c("/", "\\"))
{
  sep <- match.arg(sep)
  normalizePath(path.expand(x), sep, FALSE)
}

#' @rdname decompose_path
#' @export
strip_extension <- function(x = dir())
{
  decompose_path(x)[, 2L]
}
