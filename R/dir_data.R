#' List directory contents in a tidy data frame
#' 
#' Wrapper to \code{\link[base]{dir}} that returns the results as a tidy data 
#' frame, for easier \code{\link[utils]{View}}ing.
#' 
#' @seealso \code{\link[base]{dir}}, \code{\link{split_path}}
#' @examples
#' base_r_files <- dir_data(R.home(), pattern = "\\.R$")
#' \donttest{
#'   # Viewing not needed for testing purposes
#'   utils::View(base_r_files)
#' }
#' @importFrom assertive.properties is_empty
#' @importFrom magrittr %>%
#' @export
dir_data <- function(x = ".", pattern = NULL, all.files = FALSE)
{
  files <- dir(
    x, 
    pattern = pattern, 
    all.files = all.files, 
    recursive = TRUE, 
    full.names = TRUE
  )
  if(is_empty(files))
  {
    return(data.frame())
  }
  splits <- strsplit(files, "/")
  depth <- vapply(splits, length, integer(1))
  max_depth <- max(depth)
  vapply(splits, `length<-`, character(max_depth), max_depth) %>% 
    simplify2array %>% 
    t %>% 
    data.frame
}
