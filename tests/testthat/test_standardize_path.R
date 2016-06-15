test_that(
  "standardize_path works with a zero length input",
  {
    x <- character()
    x2 <- NULL
    expected <- setNames(character(), character())
    expect_equal(standardize_path(x), expected)
    expect_equal(standardize_path(x2), expected)
  }
)

test_that(
  "standardize_path works with empty strings",
  {
    x <- ""
    expected <- setNames("", "")
    expect_equal(standardize_path(x), expected)
  }
)

test_that(
  "standardize_path works with missing inputs",
  {
    x <- NA
    expected <- setNames(NA_character_, NA)
    expect_warning(
      actual <- standardize_path(x),
      "Coercing .+ to class .{1,3}character.{1,3}\\."
    )
    expect_equal(actual, expected)
  }
)

test_that(
  "standardize_path works with relative paths with forward slashes.",
  {
    x <- "somedir/foo.tgz"
    pwd <- std_getwd()
    expected <- setNames(
      file.path(pwd, "somedir", "foo.tgz", fsep = "/"),
      x
    )
    expect_equal(standardize_path(x), expected)
  }
)

test_that(
  "standardize_path works with relative paths with back slashes.",
  {
    x <- "somedir\\foo.tgz"
    pwd <- std_getwd()
    expected <- setNames(
      file.path(pwd, "somedir", "foo.tgz", fsep = "/"),
      x
    )
    expect_equal(standardize_path(x), expected)
  }
)

test_that(
  "standardize_path works with relative paths with mixed forward and back slashes.",
  {
    x <- "somedir/another dir\\foo.tgz"
    pwd <- std_getwd()
    expected <- setNames(
      file.path(pwd, "somedir", "another dir", "foo.tgz", fsep = "/"),
      x
    )
    expect_equal(standardize_path(x), expected)
  }
)

test_that(
  "standardize_path works with absolute Windows paths with forward slashes.",
  {
    x <- "c:/foo/bar"
    expected <- setNames("C:/foo/bar", x)
    expect_equal(standardize_path(x), expected)
  }
)

test_that(
  "standardize_path works with absolute Windows paths with back slashes.",
  {
    x <- "c:\\foo\\bar"
    expected <- setNames("C:/foo/bar", x)
    expect_equal(standardize_path(x), expected)
  }
)

test_that(
  "standardize_path works with absolute Windows paths with mixed forward and back slashes.",
  {
    x <- "c:/foo\\bar"
    expected <- setNames("C:/foo/bar", x)
    expect_equal(standardize_path(x), expected)
  }
)

test_that(
  "standardize_path works with absolute UNC paths with forward slashes.",
  {
    x <- "//foo/bar"
    expected <- if(assertive.reflection::is_windows())
    {
      # under windows this is a UNC path
      setNames("\\\\foo/bar", x)
    } else 
    {
      # under unix, this is an absolute path
      setNames("/foo/bar", x)
    }
    expect_equal(standardize_path(x), expected)
  }
)

test_that(
  "standardize_path works with absolute UNC paths with back slashes.",
  {
    x <- "\\\\foo\\bar"
    expected <- setNames("\\\\foo/bar", x)
    expect_equal(standardize_path(x), expected)
  }
)

test_that(
  "standardize_path works with Windows drive name with trailing slash.",
  {
    x <- "c:/"
    expected <- setNames("C:/", x)
    expect_equal(standardize_path(x), expected)
  }
)

test_that(
  "standardize_path works with Windows drive name with trailing backslash.",
  {
    x <- "c:\\"
    expected <- setNames("C:/", x)
    expect_equal(standardize_path(x), expected)
  }
)

test_that(
  "standardize_path works with Windows drive name no trailing slash.",
  {
    x <- "c:"
    expected <- setNames("C:/", x)
    expect_equal(standardize_path(x), expected)
  }
)

test_that(
  "standardize_path works with /.",
  {
    x <- "/"
    expected <- if(assertive.reflection::is_windows())
    {
      # under windows this is the current drive
      setNames(toupper(normalizePath("/", "/")), x)
    } else 
    {
      # under unix, this is root
      setNames("/", x)
    }
    expect_equal(standardize_path(x), expected)
  }
)

test_that(
  "standardize_path works with \\.",
  {
    x <- "\\"
    expected <- if(assertive.reflection::is_windows())
    {
      # under windows this is the current drive
      setNames(toupper(normalizePath("/", "/")), x)
    } else 
    {
      # under unix, this is root
      setNames("/", x)
    }
    expect_equal(standardize_path(x), expected)
  }
)
