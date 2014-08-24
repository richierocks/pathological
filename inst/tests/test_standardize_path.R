test_that(
  "standardize_path works with relative paths with forward slashes.",
  {
    x <- "somedir/foo.tar.gz"
    pwd <- getwd()
    expected <- file.path(pwd, "somedir", "foo.tar.gz", fsep = "/")
  }
)

test_that(
  "standardize_path works with relative paths with back slashes.",
  {
    x <- "somedir\\foo.tar.gz"
    pwd <- getwd()
    expected <- file.path(pwd, "somedir", "foo.tar.gz", fsep = "/")
  }
)

test_that(
  "standardize_path works with relative paths with mixed forward and back slashes.",
  {
    x <- "somedir/another dir\\foo.tar.gz"
    pwd <- getwd()
    expected <- file.path(pwd, "somedir", "another dir", "foo.tar.gz", fsep = "/")
  }
)
