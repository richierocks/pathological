
test_that(
  "recompose_path is the inverse of decompose_path, except standardized",
  {
    x <- c(
      character(),
      NULL,
      "foo.tgz",
      "somedir/foo.tgz",
      "foo.tar.gz",
      "somedir/foo.tar.gz",
      "foo",
      "somedir/foo",
      "foo. bar.zip",
      "somedir\\foo.tgz",
      "somedir\\another dir/foo.tgz",
      R.home(),
      "~",
      "~/foo.tgz",
      ".",
      "..",
      "./foo.tgz",
      "",
      NA
    )
    expected <- standardize_path(x)
    actual <- recompose_path(decompose_path(x))
    expect_equivalent(actual, expected)
  }
)

