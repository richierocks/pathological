# Null operator
# Borrowed from testthat, devtools, etc.
`%||%` <- function(a, b)
{
  if (is.null(a)) b else a
}

#' Create an NTFS junction
#' 
#' On Windows only, it creates an NTFS junction from one directory to another.
#' @param source_dir A string giving a path to the source directory.  Should not
#' exist already.
#' @param target_dir A string giving a path to the target directory.  Will be 
#' created if it does not already exist.
#' @return 0 for success; otherwise a positive integer error code from mklink is
#' invisibly returned.
#' @details This runs the \code{mklink} Windows utility with the \code{/J}
#' switch to create an NTFS junction.
#' @note This function mostly exists because CRAN uses NTFS junctions, and a 
#' local way to test \code{decompose_path} etc. when junctions are present is
#' needed.  In the future, other link types (and links on other OSes) may be 
#' supported. 
#' @references \code{mklink} refence page:
#' \url{https://technet.microsoft.com/en-us/library/cc753194(v=ws.11).aspx}
#' Hard links and junctions:
#' \url{https://msdn.microsoft.com/en-us/library/windows/desktop/aa365006(v=vs.85).aspx}
#' @noRd
create_ntfs_junction <- function(source_dir = tempfile("source"), target_dir = tempfile("target"))
{
  assertive.reflection::assert_is_windows()
  if(file.exists(source_dir))
  {
    stop(sprintf("The source directory %s already exists.", source_dir))
  }
  if(missing(source_dir))
  {
    message("The source directory is ", source_dir)
  }
  if(missing(target_dir))
  {
    message("The target directory is ", target_dir)
  }
  if(!file.exists(target_dir))
  {
    dir.create(target_dir, recursive = TRUE) 
  }
  result <- shell(paste("mklink /J", source_dir, target_dir), intern = TRUE)
  invisible(attr(result, "status") %||% 0)
}

# Inspired by base::simplify2array, rather than listless:list_to_data.frame
#' @importFrom magrittr %>%
#' @noRd
list2mat <- function(l)
{
  max_depth <- l %>% 
    vapply(length, integer(1)) %>% 
    max
  l %>% 
    vapply(`length<-`, character(max_depth), max_depth) %>% 
    t
}

# AppVeyor doesn't seem to be correctly installing dependencies.  In this case
# the assertive.numbers dependency of assertive.files isn't installed. Import
# a function from it to force the issue.
#' @importFrom assertive.numbers is_equal_to
NULL
