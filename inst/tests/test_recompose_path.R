library(testthat)
library(assertive)
library(stringr)

test_that(
  "recompose_path",
  {
    x <- c(
      "somedir/foo.tgz", 
      "another dir\\bar.tar.gz", 
      "baz", 
      "quux. quuux.tbz2", 
      "~/quuuux.tar.xz",
      "", 
      ".",
      "..",
      NA_character_
    )
    expected <- c(
      file.path(getwd(), "somedir/foo.tgz"), 
      file.path(getwd(), "another dir/bar.tar.gz"), 
      file.path(getwd(), "baz"), 
      file.path(getwd(), "quux. quuux.tbz2"), 
      normalizePath(path.expand("~/quuuux.tar.xz"), winslash="/", mustWork=FALSE),
      "", 
      getwd(),
      dirname(getwd()),
      NA_character_
    )
    expect_equal(recompose_path(x=decompose_path(x)), expected)
    
    expected <- c(
      "somedir/foo.tgz", 
      "another dir/bar.tar.gz", 
      "baz", 
      "quux. quuux.tbz2", 
      "~/quuuux.tar.xz",
      "", 
      ".",
      "..",
      NA_character_
    )
    expect_equal(recompose_path(x=decompose_path(x), use_shortform=TRUE), expected)
  }
)
