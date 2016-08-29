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
#' @param ... Passed from the defunct \code{dir_copy} to \code{copy_dir}.
#' @note Target directories that don't exist are created, silently (assuming  
#' write permission).
#' @return A logical vector of whether or not each file was successfully  
#' copied is invisibly returned.
#' @seealso \code{\link[base]{basename}}
#' @examples
#' \dontrun{
#' #Copy subdirs by default
#' copy_dir(R.home("etc"), file.path(tempdir(), "etc"))
#' #Just copy the top level
#' copy_dir(R.home("etc"), file.path(tempdir(), "etc2"), recursive = FALSE)
#' #Now copy deeper levels, without overwriting.
#' copy_dir(R.home("etc"), file.path(tempdir(), "etc2"), overwrite = FALSE)
#' #Cleanup
#' unlink(file.path(tempdir(), "etc"), recursive = TRUE)
#' unlink(file.path(tempdir(), "etc2"), recursive = TRUE)
#' }
#' @importFrom assertive.files is_dir
#' @importFrom plyr tryapply
#' @export
copy_dir <- function(source_dir, target_dir, pattern = NULL, overwrite = FALSE, 
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
  is_directory <- is_dir(file.path(source_dir, filenames))
  directories <- c(target_dir, file.path(target_dir, filenames[is_directory]))
  tryapply(
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

#' @rdname copy_dir
#' @export
dir_copy <- function(...)
{
  .Defunct("copy_dir")
  copy_dir(...)
}
