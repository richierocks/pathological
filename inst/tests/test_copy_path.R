library(testthat)
library(assertive)
library(stringr)

test_that(
  "copy_path",
  {
      
    source_dir <- R.home("etc")
    source_dir2 <- R.home("tests")
    target_dir <- file.path(tempdir(), "dir_copy/etc")
    target_dir2 <- file.path(tempdir(), "dir_copy/etc2")
    
    carefulCleanup <- function(x, pattern=basename(tempdir())) {
        sapply(x, function(ii) {
            out <- NA
            if (grepl(pattern, ii)) {
                out <- unlink(ii, recursive=TRUE, force=TRUE)
            }        
            out
        })
    }
    
    ################
    ## New version #
    ################
    
    expected_raw <- list(list(from=NA, to=NA, elements=c("."=TRUE)))
    
    ## Copy subdirs by default
    expected <- expected_raw
    expected[[1]]$from <- source_dir
    expected[[1]]$to <- target_dir
    expect_equal(
        res <- dir_copy(source_dir=source_dir, target_dir=target_dir),
        expected
    )
    carefulCleanup(x=target_dir)
    
    ## One source, two targets
    expected <- expected_raw
    expected[[1]]$from <- source_dir
    expected[[1]]$to <- target_dir
    expected[[2]] <- expected[[1]]
    expected[[2]]$to <- target_dir2
    expect_equal(
        res <- dir_copy(source_dir=source_dir, 
            target_dir=c(target_dir, target_dir=target_dir2)
        ),
        expected
    )
    carefulCleanup(x=c(target_dir, target_dir2))
    
    ## Two source, one target
    expected <- expected_raw
    expected[[1]]$from <- source_dir
    expected[[1]]$to <- target_dir
    expected[[2]] <- expected[[1]]
    expected[[2]]$from <- source_dir2
    expected[[2]]$to <- target_dir
    expect_equal(
        res <- dir_copy(source_dir=c(source_dir, source_dir2), 
            target_dir=target_dir, overwrite=TRUE
        ),
        expected
    )
    carefulCleanup(x=c(target_dir, target_dir2))
    
    ## Two sources, two targets
    expected <- expected_raw
    expected[[1]]$from <- source_dir
    expected[[1]]$to <- target_dir
    expected[[2]] <- expected[[1]]
    expected[[2]]$from <- source_dir2
    expected[[2]]$to <- target_dir2
    expect_equal(
        res <- dir_copy(source_dir=c(source_dir, source_dir2), 
            target_dir=c(target_dir, target_dir=target_dir2),
            overwrite=TRUE
        ),
        expected
    )
    carefulCleanup(x=c(target_dir, target_dir2))
    
    ## Just copy the top level
    expected <- expected_raw
    expected[[1]]$from <- source_dir
    expected[[1]]$to <- target_dir2
    tmp <- list.files(source_dir)
    expected[[1]]$elements <- rep(TRUE, length(tmp))
    names(expected[[1]]$elements) <- tmp
    rm(tmp)
    expect_equal(
        res <- dir_copy(source_dir, target_dir=target_dir2, recursive=FALSE),
        expected
    )
    
    ## Now copy deeper levels, without overwriting.
    expected <- expected_raw
    expected[[1]]$from <- source_dir
    expected[[1]]$to <- target_dir2
    expect_equal(
        res <- dir_copy(source_dir, target_dir=target_dir2, overwrite=FALSE),
        expected
    )
    
    ## Careful cleanup
    carefulCleanup(x=c(target_dir, target_dir2))
    
    ################
    ## Old version #
    ################
    
    #Copy subdirs by default
    tmp <- list.files(source_dir, recursive=TRUE)
    expected <- rep(TRUE, length(tmp))
    names(expected) <- tmp
    rm(tmp)
    expect_equal(
        res <- dir_copy_old(source_dir, target_dir=target_dir),
        expected        
    )
    carefulCleanup(x=target_dir)
    
    ## One source, two targets
    expect_equal(
        res <- dir_copy_old(source_dir=source_dir, 
            target_dir=c(target_dir, target_dir=target_dir2)
        ),
        expected
    )
    ## TODO: probably undesired result
    carefulCleanup(x=c(target_dir, target_dir2))
    
    ## Two source, one target
    tmp <- list.files(source_dir, recursive=TRUE)
    e1 <- rep(TRUE, length(tmp))
    names(e1) <- tmp
    tmp2 <- list.files(source_dir2, recursive=TRUE)
    e2 <- rep(TRUE, length(tmp2))
    names(e2) <- tmp2
    expected <- c(e1, e2)
    rm(tmp, tmp2, e1, e2)
    expect_equal(
        res <- dir_copy_old(source_dir=c(source_dir, source_dir2), 
            target_dir=target_dir
        ),
        expected
    )
    ## TODO: probably undesired result
    carefulCleanup(x=c(target_dir, target_dir2))
    
    ## Two sources, two targets
    expect_equal(
        res <- dir_copy_old(source_dir=c(source_dir, source_dir2), 
            target_dir=c(target_dir, target_dir=target_dir2),
            overwrite=TRUE
        ),
        expected
    )
    ## TODO: probably undesired result
    carefulCleanup(x=c(target_dir, target_dir2))
    
    ## Just copy the top level
    tmp <- list.files(source_dir, full.names=TRUE)
    tmp <- basename(tmp[!file.info(tmp)$isdir])
    expected <- rep(TRUE, length(tmp))
    names(expected) <- tmp
    rm(tmp)
    expect_equal(
        res <- dir_copy_old(source_dir, target_dir=target_dir2, recursive=FALSE),
        expected
    )
    
    ## Now copy deeper levels, without overwriting.
    tmp <- list.files(source_dir, recursive=TRUE)
    expected <- rep(TRUE, length(tmp))
    names(expected) <- tmp
    rm(tmp)
    expect_equal(
        res <- dir_copy_old(source_dir, target_dir=target_dir2, overwrite=FALSE),
        expected
    )
    ## Careful cleanup
    carefulCleanup(x=c(target_dir, target_dir2))
    
  }
)
