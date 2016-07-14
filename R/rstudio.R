#' Get the RStudio project directory
#' 
#' Gets the current RStudio project directory.
#' @param sep String separator between directory levels in the output.
#' @return A string giving the path to the current RStudio project directory,
#' or character vector with length zero if you are running RStudio without a
#' project open.
#' @note This only works when your IDE is RStudio. Otherwise an error is thrown.
#' @examples 
#' assertive.base::dont_stop(rstudio_project_dir())
#' @importFrom assertive.reflection assert_is_rstudio
#' @export
rstudio_project_dir <- function(sep = c("/", "\\"))
{
  assert_is_rstudio()
  e <- as.environment("tools:rstudio")
  x <- e$.rs.getProjectDirectory()
  standardize_path(x, sep = sep, include_names = FALSE)
}
