#' Create or remove files and directories
#' 
#' A vectorized version of \code{dir.create} (see \code{\link[base]{files}} and 
#' \code{\link[base]{files2}}) and \code{\link[base]{unlink}} with more 
#' convenient defaults.
#' @param x A character vector of paths of directories to create/remove. 
#' For \code{create_dirs}, it defaults to a directory inside \code{tempdir()}.
#' @return A logical vector of successes of failures.
#' @note \code{create_dirs} will only attempt to create directories that don't 
#' already exist.
#' @seealso \code{\link[base]{files}}, \code{\link[base]{files2}}, 
#' \code{\link[base]{unlink}}, \code{\link[fs]{create}}
#' @examples
#' \donttest{
#' dirs <- temp_dir(c("foo", "bar/baz"))
#' create_dirs(dirs)
#' 
#' # Check this worked:
#' assertive.files::assert_all_are_dirs(dirs)
#' 
#' files <- temp_dir("blah/blah/blah", LETTERS)
#' create_files(files)
#' 
#' assertive.files::assert_all_are_existing_files(files)
#' 
#' # Clean up
#' remove_dirs(temp_dir(c("foo", "bar", "blah")))
#' }
#' @importFrom stats setNames
#' @export
create_dirs <- function(x = temp_file(pattern = "dir"))
{
  doesnt_yet_exist <- !file.exists(x)
  yn <- setNames(logical(length(x)), x)
  yn[doesnt_yet_exist] <- vapply(
    x[doesnt_yet_exist],
    dir.create,
    logical(1), 
    recursive = TRUE
  )
  yn
}

#' @rdname create_dirs
#' @export
create_files <- function(x = temp_file())
{
  dirs <- unique(dirname(standardize_path(x)))
  create_dirs(dirs)
  setNames(file.create(x), x)
}

#' @rdname create_dirs
#' @export
remove_dirs <- function(x)
{
  unlink(x, recursive = TRUE, force = TRUE)
}
