#' List the Files in a Directory/Folder
#' 
#' This function extends the `base` function `list.files` to return a data frame of the full path
#' name of a list of files.  
#'
#' The result is a tibble data frame with a single column named `fullname`. 
#' @inheritParams base::list.files
#' @param full.names see `list.files`, but set to `TRUE` by default
#' 
#' @return data frame (tibble form)
#' @export
#'
#' @examples
#' list_files()
#' list_files(system.file(package = "pathological"))
#' list_files(system.file(package = "base"), full.names = FALSE)
list_files <- function(path = ".", pattern = NULL, all.files = FALSE, full.names = TRUE, 
                       recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, 
                       no.. = FALSE) {
  files <- list.files(path = path, pattern = pattern, all.files = all.files, 
             full.names = full.names, recursive = recursive, ignore.case = ignore.case, 
             include.dirs = include.dirs, no.. = no..)
  out <- list(filename = files)
  ## in case we don't want the tibble import
  out <- structure(out, row.names = seq_len(length(files)), class = c("tbl_df", "tbl", "data.frame"))
  ## otherwise
  ## out <- tibble::as_tibble(out)
  if (full.names) out$filename <- normalizePath(out$filename)
  out
}