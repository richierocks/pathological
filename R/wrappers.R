#' Get the libraries on your machine
#' 
#' Wrapper to \code{\link[base]{.libPaths}} that gets all the libraries that R 
#' knows about on your machine.
#' @param index A numeric or logical vector specifying the index of the 
#' libraries to return.  By default, all libraries are returned.
#' @param sep String separator between directory levels in the output.
#' @return A character vector of paths to libraries.
#' @seealso \code{\link[base]{.libPaths}}
#' @references \url{http://cran.r-project.org/doc/FAQ/R-FAQ.html#What-is-the-difference-between-package-and-library_003f}
#' @examples
#' get_libraries()
#' get_libraries(1)
#' @export
get_libraries <- function(index = TRUE, sep = c("/", "\\"))
{
  standardize_path(.libPaths()[index], sep = sep, include_names = FALSE)
}

#' @rdname r_profile
#' @export
r_environ <- function(sep = c("/", "\\"))
{
  sep <- match.arg(sep)
  # From ?Startup:
  # "The name of the user file can be specified by the R_ENVIRON_USER 
  # environment variable"
  x <- Sys.getenv("R_ENVIRON_USER", NA)
  if(is.na(x))
  {
    # "if this is unset, the files searched for are '.Renviron' in the current"
    x <- if(file.exists(".Renviron"))
    {
      ".Rprofile"
    } else if(file.exists("~/.Renviron"))
    {
      # "or in the user's home directory (in that order)"
      "~/.Rprofile"
    } else 
    {
      NA_character_
    }
  } 
  x <- standardize_path(x, sep = sep, include_names = FALSE)    
  unname(x)
}

#' @rdname r_profile
#' @export
r_environ_site <- function(sep = c("/", "\\"))
{
  sep <- match.arg(sep)
  # From ?Startup:
  # "The name of the site file is the one pointed to by the environment variable 
  # R_ENVIRON"
  x <- Sys.getenv("R_ENVIRON", NA)
  if(is.na(x))
  {
    # "if this is unset, 'R_HOME/etc/Renviron.site' is used"
    x <- r_home("etc", "Renviron.site", sep = sep)
    x <- if(file.exists(x))
    {
      x
    } else
    {
      NA_character_
    }
  } else
  {
    x <- standardize_path(x, sep = sep, include_names = FALSE)
  }
  unname(x)
}

#' The R home directory
#' 
#' Return a path to a file in the R home directory.  A vectorized, standardized
#' version of \code{R.home}.
#' @param component \code{"home"} for the root of the R installation directory,
#' or the name of a subdirectory.
#' @param ... Further subdirectories passed to \code{file.path}.
#' @param sep String separator between directory levels in the output.
#' @return A character vector of paths inside the R installation dir.
#' @note The \code{component} argument has special behaviour for the values
#' "home", "bin", "doc", "etc", "include", "modules", and "share". See the
#' help page for \code{\link[base]{R.home}}.
#' @seealso \code{\link[base]{R.home}}
#' @examples
#' r_home()
#' r_home("etc", "Rprofile.site")
#' r_home(c("home", "bin", "share"), c("", "i386", "zoneinfo"))
#' @export
r_home <- function(component = "home", ..., sep = c("/", "\\"))
{
  sep <- match.arg(sep)
  roots <- vapply(component, R.home, character(1))
  standardize_path(file.path(roots, ...), sep = sep, include_names = FALSE)
}

#' Get the location of the R profile/environ
#' 
#' Gets the location of the user or site R profile and environ startup files.
#' @param sep String separator between directory levels in the output.
#' @return A string giving the path the \code{".Rprofile"}, \code{".Renviron"}, 
#' \code{"Rprofile.site"}, or \code{".Renviron.site"}.  If the file cannot be 
#' found, NA is returned.
#' @seealso \code{\link[base]{Startup}} for how this is calculated.
#' @examples
#' r_environ()
#' r_environ_site()
#' r_profile()
#' r_profile_site()
#' @aliases startup environ
#' @export
r_profile <- function(sep = c("/", "\\"))
{
  sep <- match.arg(sep)
  # From ?Startup:
  # "The path of this file can be specified by the R_PROFILE_USER environment 
  # variable"
  x <- Sys.getenv("R_PROFILE_USER", NA)
  if(is.na(x))
  {
    # "If this is unset, a file called '.Rprofile' is searched for in the 
    # current directory"
    x <- if(file.exists(".Rprofile"))
    {
      ".Rprofile"
    } else if(file.exists("~/.Rprofile"))
    {
      # "or in the user's home directory (in that order)"
      "~/.Rprofile"
    } else 
    {
      NA_character_
    }
  } 
  x <- standardize_path(x, sep = sep, include_names = FALSE)    
  unname(x)
}

#' @rdname r_profile
#' @export
r_profile_site <- function(sep = c("/", "\\"))
{
  sep <- match.arg(sep)
  # From ?Startup:
  # "The path of this file is taken from the value of the R_PROFILE environment 
  # variable"
  x <- Sys.getenv("R_PROFILE", NA)
  if(is.na(x))
  {
    # "If this variable is unset, the default is 'R_HOME/etc/Rprofile.site'"
    x <- r_home("etc", "Rprofile.site", sep = sep)
    x <- if(file.exists(x))
    {
      x
    } else
    {
      NA_character_
    }
  } else
  {
    x <- standardize_path(x, sep = sep, include_names = FALSE)
  }
  unname(x)
}

#' Find paths to executables
#' 
#' Wrapper to \code{Sys.which}, that returns standardized paths.
#' @param x A character vector of executables.
#' @param sep String separator between directory levels in the output.
#' @return A character vector of paths to those executables, or \code{NA} if it 
#' doesn't exist. (This behaviour for missing executables differs from 
#' \code{Sys.which}.)
#' @seealso \code{\link[base]{Sys.which}}
#' @examples
#' sys_which("R")              # R executable
#' sys_which(c("make", "gcc")) # tools for running Rcpp
#' @export
sys_which <- function(x, sep = c("/", "\\"))
{
  std_x <- standardize_path(Sys.which(x), sep = sep, include_names = FALSE)
  ifelse(nzchar(std_x), std_x, NA_character_)
}

#' Find a file in a package
#' 
#' Wrapper to \code{system.file} that returns standardized paths.
#' @param ... Character vectors specifying subdirectories and files within some 
#' package. The default, none, returns the root of the package. Wildcards are 
#' not supported.
#' @param package A string with the name of a single package. An error occurs if 
#' more than one package name is given.
#' @param library_location a character vector with path names of R libraries. 
#' See the 'Details section of \code{\link[base]{system.file}} for the meaning 
#' of the default value of NULL.
#' @param must_work If \code{TRUE}, an error is given if there are no matching 
#' files.
#' @param sep String separator between directory levels in the output.
#' @return A character vector of positive length, containing the file paths that 
#' matched \code{...}, or a missing string, \code{NA}, if none matched (unless 
#' \code{mustWork = TRUE}).  (This behaviour for missing paths differs from 
#' \code{system.file}.)
#' If matching the root of a package, there is no trailing separator.
#' system.file() with no arguments gives the root of the base package.
#' @seealso \code{\link[base]{system.file}}
#' @examples
#' # Examples taken from ?system.file
#' system_file()                  # The root of the 'base' package
#' system_file(package = "stats") # The root of package 'stats'
#' system_file("INDEX")
#' system_file("help", "AnIndex", package = "splines")
#' @export
system_file <- function(..., package = "base", library_location = NULL, 
  must_work = FALSE, sep = c("/", "\\"))
{
  paths <- standardize_path(
    system.file(
      ..., 
      package  = package, 
      lib.loc  = library_location, 
      mustWork = must_work
    ), 
    sep = sep, 
    include_names = FALSE
  )
  ifelse(nzchar(paths), paths, NA_character_)
}

#' Return paths to files or dirs within the temp dir
#' 
#' Vectorized wrappers to \code{tempdir} and \code{tempfile} that return  
#' standardized paths.
#' @param ... Character vectors of further directories within the temp 
#' directory. Passed to \code{\link[base]{file.path}}.
#' @param pattern Character vector of prefixes for the temp file name. 
#' Passed to \code{\link[base]{tempfile}}.
#' @param fileext Character vector of file extensions for the temp file. Passed 
#' to \code{\link[base]{tempfile}}.
#' @param sep String separator between directory levels in the output.
#' @return For \code{temp_file} a character vector giving the names of possible 
#' (temporary) files. Note that no files are generated by \code{temp_file}.
#' For \code{temp_dir}, the path of the per-session temporary directory.
#' @seealso \code{\link[base]{tempdir}}
#' @examples
#' temp_dir(c("foo", "bar/baz"))
#' temp_file(c("foo", "bar/baz"), fileext = c(".txt", ".R"))
#' @export
temp_dir <- function(..., sep = c("/", "\\"))
{
  sep <- match.arg(sep)
  tmp <- file.path(tempdir(), ..., fsep = sep)
  standardize_path(tmp, sep = sep, include_names = FALSE)
}

#' @rdname temp_dir
#' @export
temp_file <- function(..., pattern = "file", fileext = "", sep = c("/", "\\"))
{
  x <- temp_dir(...)
  pattern <- rep_len(pattern, length(x))
  fileext <- rep_len(fileext, length(x))
  tmp <- mapply(
    tempfile, 
    x,
    pattern = pattern, 
    fileext = fileext
  )
  standardize_path(tmp, sep = sep, include_names = FALSE)
}
