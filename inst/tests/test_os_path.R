test_that(
  "os_path works correctly",
  {
    expected <- strsplit(Sys.getenv("PATH"), ";")[[1]]
    expect_identical(os_path(standardize = FALSE), expected)
  }
)

test_that(
  "os_path works correctly",
  {
    expected <- normalizePath(
      strsplit(Sys.getenv("PATH"), ";")[[1]], 
      "/", 
      mustWork = FALSE
    )
    expect_identical(os_path(), expected)
  }
)
