pathological
============

R package of utilities for handling paths, files and directories

### Decomposing paths

`decompose_path` splits a path into a directory name, file name (without 
extension), and a file extension. `recompose_path` reverses the effects of
this function.

`get_extension`, `strip_extension`, and `replace_extension` allow manipulation
of file extensions.  They are smart enough to deal with double extensions like
`tar.gz`.

On Windows, `get_drive` returns the drive name of the path.

### Copying directories

`copy_dir` copies the contents of a directory.

### Standarizing paths

`standardize_path` (and `standardise_path`, for British English fans) is a 
wrapper to `base::normalizePath` that standardizes the form of a path, to make
them more easily comparable.  You can choose whether you want back or forward 
slashes, and the inputs paths are given as names.

`r_home`, `temp_dir`, `temp_file`, `system_file`, and `sys_which` provide 
standardized versions of the similarly named base R functions.

### Other utilities

`os_path` returns the operating system `PATH` environment variable as a 
character vector of stanardized paths.

On Windows, `cygwinify_path` makes paths suitable for use with cygwin.
