library(testthat)

dir2 <- function(path = ".", pattern = NULL, recursive = TRUE, ...)
{
  dir(  
    path,
    pattern      = pattern, 
    recursive    = recursive, 
    all.files    = TRUE,
    full.names   = FALSE,
    include.dirs = TRUE,
    ...
  )
}

test_that(
  "dir_copy works with recursive = FALSE",
  {
    source_dir <- R.home("etc")
    target_dir <- file.path(tempdir(), "etc")
    on.exit(unlink(target_dir))
    dir_copy(source_dir, target_dir, recursive = FALSE)
    expect_identical(
      dir2(source_dir, recursive = FALSE), 
      dir2(target_dir, recursive = FALSE)
    )   
  }
)

test_that(
  "dir_copy works with recursive = TRUE",
  {
    source_dir <- R.home("etc")
    target_dir <- file.path(tempdir(), "etc")
    on.exit(unlink(target_dir, recursive = TRUE))
    dir_copy(source_dir, target_dir)
    expect_identical(dir2(source_dir), dir2(target_dir))   
  }
)

