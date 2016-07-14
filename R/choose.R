#' @rdname choose_files
#' @importFrom assertive.reflection assert_is_windows
#' @export
choose_dir <- function(default = "", sep = c("/", "\\"))
{
  assert_is_windows()
  sep <- match.arg(sep)
  caption <- gettext("Select a folder", domain = "R-pathological")
  x <- utils::choose.dir(default, caption)
  standardize_path(x, sep, include_names = FALSE)
}

#' Choose files interactively
#' 
#' Choose one or more files or a directory interactively using a pop-up dialog.
#' @param default The default file to be selected.  See the Details section of
#' \code{choose.files} for how to specify.  Only on Windows.
#' @param multi Logical value indicating if multiple files can be selected.
#' Only on Windows.
#' @param sep String separator between directory levels in the output.
#' @return A character vector of standardized file paths that were chosen.
#' @note \code{choose_files} uses \code{choose.files} under Windows 
#' and \code{\link[base]{file.choose}} under other platforms.
#' @importFrom assertive.reflection is_windows
#' @examples 
#' \donttest{
#' if(interactive())
#' {
#'   choose_files()
#'   if(assertive.reflection::is_windows())
#'   {
#'     choose_dir()
#'   }
#' }
#' }
#' @export
choose_files <- function(default = "", multi = FALSE, sep = c("/", "\\"))
{
  if(!interactive())
  {
    stop("You are not running R interactively; use dir() instead.")
  }
  sep <- match.arg(sep)
  x <- if(is_windows())
  {
    filters <- matrix(
      c(
        "All Files", "*",
        "R source files (R, c, cpp, Rnw, Rmd, Rhtml, Rd)", "*.R;*.c;*.cpp;*.Rnw;*.Rmd;*.Rhtml;*.Rd",
        "Delimited text files (csv, dlm, dat)", "*.csv;*.dlm;*.dat",
        "Text files (txt)", "*.txt",
        "Spreadsheets (xlsx, xls, ods)", "*.xlsx;*.xls;*.ods",
        "Archives (zip, tar, tar.gz, tar.xz)", "*.zip;*.tar;*.tar.gz;*.tar.xz",
        "Image files (png, jpeg/jpg, pdf, ps, emf, bmp, svg)", "*.png;*.jpeg;*.jpg;*.pdf;*.ps;*.emf;*.bmp;*.svg",
        "R workspaces and variables (RData, rda, rds)", "*.RData;*.rda;*.rds",
        "Web files (html, xhtml, css, js)", "*.html;*.xhtml;*.css;*.js"
      ),
      ncol = 2,
      byrow = TRUE
    )
    caption <- if(multi)
    {
      gettext("Select at least one file", domain = "R-pathological")
    } else
    {
      gettext("Select a file", domain = "R-pathological")
    }
    # Have to use :: rather than @importFrom because fn only exists on Windows
    utils::choose.files(default, caption, multi, filters = filters, index = 1)
  } else
  {
    if(nzchar(default))
    {
      warning("Setting the default file is only supported under Windows.")
    }
    if(multi)
    {
      warning("Choosing multiple files is only supported under Windows.")
    }
    tryCatch(
      file.choose(),
      error = function(e) character()
    )
  }
  standardize_path(x, sep, include_names = FALSE)
}
