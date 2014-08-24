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
#' @param ... Passed from the deprecated \code{dir_copy} to \code{copy_dir}.
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
#' @importFrom assertive is_dir
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
#' @importFrom assertive is_windows
#' @importFrom stringr fixed
#' @importFrom stringr str_detect
#' @importFrom stringr str_split_fixed
#' @export
cygwinify_path <- function(x = dir())
{
  if(!is_windows())
  {
    warning(
      "This function is expecting to be run under Windows, but the OS is ", 
      .Platform$OS.type
    )
  }
  cygwinified_x <- standardize_path(x)
  colon <- fixed(":")
  has_drive <- str_detect(cygwinified_x, colon)
  split_path <- str_split_fixed(cygwinified_x[has_drive], colon, 2L)
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
#' @param include_dir Should the directory part of the path be included? If 
#' \code{NA}, the default, keep the directory from the input.  If \code{TRUE},
#' standardize the directory.  If \code{FALSE}, strip the directory.
#' @param new_extension A new extension to replace the existing ones.
#' @param ... Not currently used.
#' @return \code{decompose_path} returns a character matrix with three 
#' columns named \code{"dirname"}, \code{"filename"} and \code{"extension"}.
#' \code{strip_extension} returns a character vector of the filename, possibly 
#' with a directory (see \code{include_dir} argument).
#' \code{strip_extension} returns a character vector of the filename with a new 
#' extension, possibly with a directory (see \code{include_dir} argument).
#' \code{get_extension} returns a character vector of the third column.
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
#' @importFrom assertive is_empty
#' @importFrom assertive coerce_to
#' @importFrom assertive is_not_na
#' @importFrom assertive is_dir
#' @importFrom stringr str_detect
#' @importFrom stringr fixed
#' @importFrom stringr str_match
#' @export
decompose_path <- function(x = dir())
{
  if(is_empty(x))
  {
    return(
      structure(
        data.frame(
          dirname = character(), 
          filename = character(), 
          extension = character(),
          stringsAsFactors = FALSE,
          row.names = character()
        ),
        class = c("decomposed_path", "data.frame")
      )
    )
  }
  original_x <- x <- coerce_to(x, "character")
  x <- standardize_path(x)
  not_missing <- is_not_na(x)
  is_dir_x <- is_dir(x)
  
  basename_x <- ifelse(
    not_missing,
    ifelse(is_dir_x, "", basename(x)),
    NA_character_
  )
  has_extension <- str_detect(basename_x, fixed("."))
    
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
    split_name <- str_match(
      basename_x[not_missing_and_has_extension], 
      rx
    )
  
    filename_x[not_missing_and_has_extension] <- split_name[, 2L]
    extension_x[not_missing_and_has_extension] <- split_name[, 3L]
  }
  
  decomposed_x <- data.frame(
    dirname   = ifelse(
      not_missing,
      ifelse(is_dir_x, x, standardize_path(dirname(x))), #restandardisation required
      NA_character_
    ),
    filename  = filename_x, 
    extension = extension_x,
    row.names = ifelse(is.na(original_x), "<NA>", original_x),
    stringsAsFactors = FALSE
  )
  
  structure(decomposed_x, class = c("decomposed_path", "data.frame"))
}

#' @rdname copy_dir
#' @export
dir_copy <- function(...)
{
  .Deprecated("copy_dir")
  copy_dir(...)
}

#' On Windows, return the drive of the path
#' 
#' On a Windows system, this returns the drive letter of the path followed by a 
#' colon.  On other systems, it returns a single forward slash.
#' @param x A character vector of file paths. Defaults to the current directory.
#' @return A character vector of drive paths on Windows systems, or forward 
#' slashes on Unix-based systems.
#' @examples
#' get_drive(c("~", r_home(), temp_dir()))
#' @importFrom assertive is_windows
#' @export
get_drive <- function(x = getwd())
{
  if(is_windows())
  {
    vapply(strsplit(standardize_path(x), "/"), head, character(1), n = 1)
  } else
  {
    rep.int("/", length(x))
  }
}

#' @rdname decompose_path
#' @export
get_extension <- function(x = dir())
{
  setNames(decompose_path(x)$extension, x)  
}

#' The OS path 
#' 
#' The locations in the operating system \code{PATH} environment variable.
#' @param sep String separator between directory levels in the output.
#' @param standardize Should the paths be standardized?
#' @return A character vector of paths.
#' @seealso \code{\link[base]{Sys.getenv}}
#' @examples
#' os_path()
#' @export
os_path <- function(sep = c("/", "\\"), standardize = TRUE)
{
  path <- Sys.getenv("PATH", NA)
  if(is.na(path))
  {
    warning("The 'PATH' environment variable has not been set.")
    return(character())
  }
  path <- strsplit(path, ";")[[1]]
  if(standardize)
  {
    standardize_path(path, sep = sep)  
  } else path
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
#' @seealso \code{\link[base]{R.home}}
#' @examples
#' r_home()
#' r_home("etc", "Rprofile.site")
#' r_home(c("home", "bin", "share"), c("", "i386", "zoneinfo"))
#' @export
r_home <- function(component = "home", ..., sep = c("/", "\\"))
{
  standardize_path(file.path(Vectorize(R.home)(component), ...), sep = sep)
}

#' @rdname decompose_path
#' @export
recompose_path <- function(x, ...)
{
  UseMethod("recompose_path")
}

#' @rdname decompose_path
#' @method recompose_path decomposed_path
#' @importFrom assertive is_not_na
#' @export
recompose_path.decomposed_path <- function(x, ...)
{
  not_missing <- is_not_na(x$filename)
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
replace_extension <- function(x = dir(), new_extension, include_dir = NA)
{
  if(!nzchar(new_extension))
  {
    warning("'new_extension' is empty.  Did you want strip_extension instead?")
  }
  is_dir_x <- is_dir(x)
  if(any(is_dir_x))
  {
    warning(
      "The directories ", 
      toString(sQuote(x[is_dir_x])), 
      " have no file extensions to replace."
    )
  }
  stripped <- strip_extension(x, include_dir = include_dir)
  ifelse(
    is_dir_x,
    stripped,
    paste(stripped, new_extension, sep = ".")
  )
}

#' Split a path into directory components
#' 
#' Splits a character vector of paths into directory components.  The opposite  
#' of \code{\link[base]{file.path}}.
#' @param x A character vector of file paths. Defaults to files in the 
#' current directory.
#' @return A named list of character vectors containing the split paths.
#' @note Paths are split on forward and back slashes, except for double forward 
#' or back slashes at the start of (UNC) paths.  These are included in the first 
#' element of that split path.
#' @examples
#' (splits <- split_path(c(getwd(), "~", r_home())))
#' # Reverse the operation
#' sapply(splits, paste, collapse = "/")
#' @importFrom assertive is_empty
#' @importFrom assertive coerce_to
#' @export
split_path <- function(x = dir())
{
  if(is_empty(x))
  {
    return(setNames(list(), character()))
  }
  original_x <- x <- coerce_to(x, "character")
  x <- standardize_path(x)
  split_x <- strsplit(x, "(?<=[^/\\\\])[/\\\\]", perl = TRUE)
  # setting names in a list, as of R3.1.1 processes backslashes cat-style, so 
  # need to duplicate them
  original_x <- str_replace_all(original_x, fixed("\\"), "\\\\")
  setNames(split_x, original_x)
}

#' Standardize paths
#' 
#' Standardi[sz]e path names so that they can be more easily compared.
#' @param x A character vector of file paths. Defaults to files in the 
#' current directory.
#' @param sep String separator between directory levels in the output.
#' @return A character vector of paths, pointing to the same locations as the
#' input, but in a standardized form.
#' @seealso \code{\link[base]{normalizePath}}, \code{\link[base]{path.expand}},
#' \code{\link[R.utils]{getAbsolutePath}}
#' @examples
#' standardize_path(c(".", "..", "~", R.home(), NA))
#' standardize_path(c(".", "..", "~", R.home(), NA), "\\")
#' @importFrom assertive is_empty
#' @importFrom assertive is_not_missing_nor_empty_character
#' @importFrom assertive coerce_to
#' @importFrom assertive is_unix
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_detect
#' @export
standardize_path <- function(x = dir(), sep = c("/", "\\"))
{
  if(is_empty(x))
  {
    return(character())
  }
  sep <- match.arg(sep)
  x <- coerce_to(x, "character")
  
  ok <- is_not_missing_nor_empty_character(x)
  
  # standardize = expand + normalize
  # normalizePath is uncomfortable with backslashes under Unix.
  x[ok] <- str_replace_all(x[ok], "[/\\\\]", "/")
  x[ok] <- ifelse(
    is.na(x[ok]),
    NA_character_,
    normalizePath(x[ok], "/", FALSE)
  )
  
  # again under Unix, normalizePath won't make path absolute
  if(is_unix())
  {
    x[ok] <- ifelse(
      str_detect(x[ok], "^/"),
      x[ok], 
      file.path(getwd(), x[ok], fsep = "/")
    )
  }
  
  # strip trailing slashes
  x[ok] <- str_replace(x[ok], "/?$", "")  
  
  # Replace / with the chosen slash
  if(sep == "\\")
  {
    x[ok] <- str_replace_all(x[ok], "/", "\\")
  }
  x
}

#' @rdname standardize_path
#' @export
standardise_path <- standardize_path

#' @rdname decompose_path
#' @export
strip_extension <- function(x = dir(), include_dir = NA)
{
  # Empty string x gets returned as empty string
  stripped <- character(length(x))
  # Missing x gets returned as NA
  stripped[is.na(x)] <- NA_character_
  
  #Everything else
  ok <- nzchar(x) & !is.na(x)
  decomposed <- decompose_path(x[ok])
  
  stripped[ok] <- if(is.na(include_dir))
  {
    # For include_dir = NA, keep directory same as input
    dirname_x <- ifelse(is_dir(x[ok]), x[ok], dirname(x[ok]))
    ifelse(
      nzchar(decomposed$filename),
      ifelse(
        dirname_x == ".",
        decomposed$filename,                              # no directory
        file.path(dirname_x, decomposed$filename)         # both
      ),
      dirname_x                                           # no filename
    )    
  } else if(include_dir) 
  {
    # For include_dir = TRUE, add standardized directory
    ifelse(
      nzchar(decomposed$filename),
      file.path(decomposed$dirname, decomposed$filename), # both
      decomposed$dirname                                  # no filename
    )
  } else
  {
    # For include_dir = FALSE, strip directory
    decomposed$filename
  }
  setNames(stripped, x)
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
#' sys_which(c("make", "gcc")) # tools for running Rcpp
#' @export
sys_which <- function(x, sep = c("/", "\\"))
{
  std_x <- standardize_path(Sys.which(x), sep = sep)
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
    sep = sep
  )
  ifelse(nzchar(paths), paths, NA_character_)
}

#' Create a temp file/dir
#' 
#' Wrappers to \code{tempdir} and \code{tempfile} that returns standardized 
#' paths.
#' @param ... Passed to \code{tempfile}
#' @param sep String separator between directory levels in the output.
#' @return For \code{temp_file} a character vector giving the names of possible 
#' (temporary) files. Note that no files are generated by \code{temp_file}.
#' For \code{temp_dir}, the path of the per-session temporary directory.
#' @seealso \code{\link[base]{tempdir}}
#' @examples
#' temp_dir()
#' temp_file()
#' @export
temp_dir <- function(sep = c("/", "\\"))
{
  standardize_path(tempdir(), sep = sep)
}

#' @rdname temp_dir
#' @export
temp_file <- function(..., sep = c("/", "\\"))
{
  standardize_path(tempfile(...), sep = sep)
}
