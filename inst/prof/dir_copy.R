## Profiling script 

require("microbenchmark")

##------------------------------------------------------------------------------
## Efficiency comparison
## Details: 'dir_copy()' vs. 'dir_copy_old()'
##------------------------------------------------------------------------------

## Clean-up function //
carefulCleanup <- function(x, pattern=basename(tempdir())) {
    sapply(x, function(ii) {
        out <- NA
        if (grepl(pattern, ii)) {
            out <- unlink(ii, recursive=TRUE, force=TRUE)
        }        
        out
    })
}

## Example content //
path 		<- file.path(tempdir(), "dir_copy")
target_dir 	<-  file.path("from")
from_dirs 	<- file.path(path, "from", paste0(letters, 1:(3*24)))
to 			<- file.path(path, "to")

sapply(from_dirs, dir.create, recursive=TRUE, showWarnings=FALSE)
sapply(to, dir.create, recursive=TRUE, showWarnings=FALSE)

fpaths <- unlist(lapply(from_dirs, file.path, paste0(letters, 1:(3*24), ".txt")))
sapply(fpaths, function(ii) {
    write("hello world!", file=ii)
})

## Source and target //
source_dir <- unique(dirname(from_dirs))
target_dir <- to

## New version //
message("Running 'microbenchmark' on 'dir_copy()' (10 times) ...")
t1 <- median(microbenchmark(
    dir_copy(source_dir=source_dir, target_dir=target_dir, 
        overwrite=TRUE),
    times=10
)$time)
carefulCleanup(x=target_dir)

## Old version //
message("Running 'microbenchmark' on 'dir_copy_old()' (10 times) ...")
t2 <- median(microbenchmark(
    dir_copy_old(source_dir=source_dir, target_dir=target_dir, 
        overwrite=TRUE),
    times=10
)$time)
carefulCleanup(x=target_dir)

## Assessment //
t1/t2
t2/t1

## Carefull cleanup //
carefulCleanup(x=path)