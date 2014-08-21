setNamesInList <- function(object, nm)
{
  nm <- gsub("\\", "\\\\", nm, fixed = TRUE)
  setNames(object, nm)
}

test_that(
  "split_path works with a zero length input",
  {
    x <- character()
    x2 <- NULL
    expected <- setNames(list(), character())
    expect_equal(split_path(x), expected)
    expect_equal(split_path(x2), expected)
  }
)

# test_that(
#   "split_path works with various inputs",
#   {
#     x <- c(
#       "c:/foo/bar", 
#       "c:\\foo\\bar", 
#       "\\\\myserver/myshare", 
#       "~", 
#       "a/relative/path", 
#       "/a/path/", 
#       ".",
#       ".."
#     )
#     cfoobar <- c("c:", "foo", "bar")
#     pwd <- strsplit(getwd(), "/")[[1]]
#     expected <- setNames(
#       list(
#         cfoobar,
#         cfoobar,
#         c("\\\\myserver", "myshare"),
#         strsplit(path.expand("~"), "[/\\]")[[1]],
#         c(pwd, "a", "relative", "path"),
#         c(get_drive(), "a", "path"),
#         pwd,
#         pwd[seq_len(length(pwd) - 1)]
#       ),
#       x
#     )
#   }
# )

test_that(
  "split_path works with absolute Windows paths with forward slashes.",
  {
    x <- "c:/foo/bar"
    expected <- setNamesInList(list(c("c:", "foo", "bar")), x)
    expect_equal(split_path(x), expected)
  }
)

test_that(
  "split_path works with absolute Windows paths with back slashes.",
  {
    x <- "c:\\foo\\bar"
    expected <- setNamesInList(list(c("c:", "foo", "bar")), x)
    expect_equal(split_path(x), expected)
  }
)

test_that(
  "split_path works with absolute Windows paths with mixed forward and back slashes.",
  {
    x <- "c:/foo\\bar"
    expected <- setNamesInList(list(c("c:", "foo", "bar")), x)
    expect_equal(split_path(x), expected)
  }
)

test_that(
  "split_path works with absolute UNC paths with forward slashes.",
  {
    x <- "//foo/bar"
    expected <- setNamesInList(list(c("\\\\foo", "bar")), x)
    expect_equal(split_path(x), expected)
  }
)

test_that(
  "split_path works with absolute UNC paths with back slashes.",
  {
    x <- "\\\\foo\\bar"
    expected <- setNamesInList(list(c("\\\\foo", "bar")), x)
    expect_equal(split_path(x), expected)
  }
)


