test_that(
  "os_path works correctly with standardize = FALSE",
  {
    expected <- strsplit(Sys.getenv("PATH"), ";")[[1]]
    expect_identical(os_path(standardize = FALSE), expected)
  }
)

test_that(
  "os_path works correctly with standardize = TRUE",
  {
    expected <- normalizePath(
      strsplit(Sys.getenv("PATH"), ";")[[1]], 
      "/", 
      mustWork = FALSE
    )
    expect_identical(os_path(), expected)
  }
)
