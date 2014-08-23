test_that(
  "standardize_path works with paths with forward slashes.",
  {
    x <- "somedir/foo.tar.gz"
    pwd <- getwd()
    expected <- file.path(pwd, "somedir", "foo.tar.gz")
  }
)
