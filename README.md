[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/0.1.0/active.svg)](http://www.repostatus.org/#active)
[![Is the package on CRAN?](http://www.r-pkg.org/badges/version/pathological)](http://www.r-pkg.org/pkg/pathological)
[![SemaphoreCI Build Status](https://semaphoreci.com/api/v1/projects/57d8160a-a325-49f2-bdac-f11a45c2bef2/635255/badge.svg)](https://semaphoreci.com/richierocks/pathological)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/bijxs8tpi1iwkt9w?svg=true)](https://ci.appveyor.com/project/richierocks/pathological)

pathological
============

R package of utilities for handling paths, files and directories

### Installation

To install, you first need the devtools package.

```{r}
install.packages("devtools")
```

Then you can install the pathological package using

```{r}
library(devtools)
install_github("richierocks/pathological")
```

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

### Standardizing paths

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
