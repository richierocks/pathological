create_expected_decomposed_path <- function(dirname, filename, extension, row.names)
{
  structure(
    data.frame(
      dirname          = dirname,
      filename         = filename,
      extension        = extension,
      row.names        = row.names, 
      stringsAsFactors = FALSE
    ), 
    class = c("decomposed_path", "data.frame")
  )
}

test_that(
  "decompose_path works with a zero length input",
  {
    x <- character()
    x2 <- NULL
    expected <- create_expected_decomposed_path(
      dirname   = character(), 
      filename  = character(), 
      extension = character(),
      rowname   = character()
    )
    expect_equal(decompose_path(x), expected)
    expect_equal(decompose_path(x2), expected)
  }
)

test_that(
  "decompose_path handles paths with no directory and a single extension in the filename.",
  {
    x <- "foo.tgz"
    pwd <- getwd()
    expected <- create_expected_decomposed_path(
      dirname          = pwd,
      filename         = "foo",
      extension        = "tgz",
      row.names        = x
    )
    expect_equal(decompose_path(x), expected)
  }
)

test_that(
  "decompose_path handles paths with a directory and a single extension in the filename.",
  {
    x <- "somedir/foo.tgz"
    pwd <- getwd()
    expected <- create_expected_decomposed_path(
      dirname          = file.path(pwd, "somedir"),
      filename         = "foo",
      extension        = "tgz",
      row.names        = x
    )
    expect_equal(decompose_path(x), expected)
  }
)

test_that(
  "decompose_path handles paths with no directory and a double extension in the filename.",
  {
    x <- "foo.tar.gz"
    pwd <- getwd()
    expected <- create_expected_decomposed_path(
      dirname          = pwd,
      filename         = "foo",
      extension        = "tar.gz",
      row.names        = x
    )
    expect_equal(decompose_path(x), expected)
  }
)

test_that(
  "decompose_path handles paths with a directory and a double extension in the filename.",
  {
    x <- "somedir/foo.tar.gz"
    pwd <- getwd()
    expected <- create_expected_decomposed_path(
      dirname          = file.path(pwd, "somedir"),
      filename         = "foo",
      extension        = "tar.gz",
      row.names        = x, 
      stringsAsFactors = FALSE
    )
    expect_equal(decompose_path(x), expected)
  }
)


test_that(
  "decompose_path handles paths with no directory and no extension in the filename.",
  {
    x <- "foo"
    pwd <- getwd()
    expected <- create_expected_decomposed_path(
      dirname          = pwd,
      filename         = "foo",
      extension        = "",
      row.names        = x
    )
    expect_equal(decompose_path(x), expected)
  }
)

test_that(
  "decompose_path handles paths with a directory and no extension in the filename.",
  {
    x <- "somedir/foo"
    pwd <- getwd()
    expected <- create_expected_decomposed_path(
      dirname          = file.path(pwd, "somedir"),
      filename         = "foo",
      extension        = "",
      row.names        = x
    )
    expect_equal(decompose_path(x), expected)
  }
)

test_that(
  "decompose_path handles filenames containing a '.' and an extension.",
  {
    x <- "foo. bar.zip"
    pwd <- getwd()
    expected <- create_expected_decomposed_path(
      dirname          = pwd,
      filename         = "foo. bar",
      extension        = "zip",
      row.names        = x
    )
    expect_equal(decompose_path(x), expected)
  }
)

test_that(
  "decompose_path handles backslashes in the directory name.",
  {
    x <- "somedir\\foo.tgz"
    pwd <- getwd()
    expected <- create_expected_decomposed_path(
      dirname          = file.path(pwd, "somedir"),
      filename         = "foo",
      extension        = "tgz",
      row.names        = x
    )
    expect_equal(decompose_path(x), expected)
  }
)

test_that(
  "decompose_path handles mixed forward and backslashes in the directory name.",
  {
    x <- "somedir\\another dir/foo.tgz"
    pwd <- getwd()
    expected <- create_expected_decomposed_path(
      dirname          = file.path(pwd, "somedir", "another dir"),
      filename         = "foo",
      extension        = "tgz",
      row.names        = x
    )
    expect_equal(decompose_path(x), expected)
  }
)

test_that(
  "decompose_path handles absolute paths to directories.",
  {
    x <- R.home()
    expected <- create_expected_decomposed_path(
      dirname          = normalizePath(R.home(), "/", mustWork = FALSE),
      filename         = "",
      extension        = "",
      row.names        = x
    )
    expect_equal(decompose_path(x), expected)
  }
)

test_that(
  "decompose_path handles '~'.",
  {
    x <- "~"
    pwd <- getwd()
    expected <- create_expected_decomposed_path(
      dirname          = normalizePath("~", "/", mustWork = FALSE),
      filename         = "",
      extension        = "",
      row.names        = x
    )
    expect_equal(decompose_path(x), expected)
  }
)

test_that(
  "decompose_path handles files inside '~'.",
  {
    x <- "~/foo.tgz"
    expected <- create_expected_decomposed_path(
      dirname          = dirname(normalizePath(x, "/", mustWork = FALSE)),
      filename         = "foo",
      extension        = "tgz",
      row.names        = x
    )
    expect_equal(decompose_path(x), expected)
  }
)

test_that(
  "decompose_path handles the current directory as '.'.",
  {
    x <- "."
    pwd <- getwd()
    expected <- create_expected_decomposed_path(
      dirname          = pwd,
      filename         = "",
      extension        = "",
      row.names        = x
    )
    expect_equal(decompose_path(x), expected)
  }
)

test_that(
  "decompose_path handles the parent directory as '..'.",
  {
    x <- ".."
    pwd <- getwd()
    expected <- create_expected_decomposed_path(
      dirname          = dirname(pwd),
      filename         = "",
      extension        = "",
      row.names        = x
    )
    expect_equal(decompose_path(x), expected)
  }
)

test_that(
  "decompose_path handles files inside '.'.",
  {
    x <- "./foo.tgz"
    pwd <- getwd()
    expected <- create_expected_decomposed_path(
      dirname          = pwd,
      filename         = "foo",
      extension        = "tgz",
      row.names        = x
    )
    expect_equal(decompose_path(x), expected)
  }
)

test_that(
  "decompose_path handles empty strings.",
  {
    x <- ""
    expected <- create_expected_decomposed_path(
      dirname          = "",
      filename         = "",
      extension        = "",
      row.names        = x
    )
    expect_equal(decompose_path(x), expected)
  }
)

test_that(
  "decompose_path handles missing paths.",
  {
    x <- NA
    expected <- create_expected_decomposed_path(
      dirname          = NA_character_,
      filename         = NA_character_,
      extension        = NA_character_,
      row.names        = "<NA>"
    )
    expect_warning(
      actual <- decompose_path(x), 
      "Coercing .+ to class [[:punct:]]character[[:punct:]]\\."
    )
    expect_equal(actual, expected)
  }
)

catz <- c(
  "catz/lolcat.gif",
  "moar cats/nyan cat.jpeg",
  "catz\\catz in loft\\ceiling cat.jpg",
  "catz/musical catz\\keyboard cat.bmp",
  "catbread.png",
  "kitties\\bonsai kitten.tiff",
  "kitties\\hipster kitty.pdf"
)

expected_catz <- create_expected_decomposed_path(
  dirname   = c(
    file.path(pwd, "catz"),
    file.path(pwd, "moar cats"),
    file.path(pwd, "catz/catz in loft"),
    file.path(pwd, "catz/musical catz"), getwd(),
    file.path(pwd, "kitties"),
    file.path(pwd, "kitties")
  ),
  filename  = c(
    "lolcat", "nyan cat", "ceiling cat", "keyboard cat", 
    "catbread", "bonsai kitten", "hipster kitty"
  ),
  extension = c(
    "gif", "jpeg", "jpg", "bmp", "png", "tiff", "pdf"
  ),
  row.names = x
)

test_that(
  "decompose_path works with a character vector input.",
  {
    x <- catz
    expect_equal(decompose_path(x), expected_catz)
  }
)

test_that(
  "decompose_path works with a factor input.",
  {
    x <- factor(catz)
    expect_warning(
      actual <- decompose_path(x), 
      "Coercing .+ to class [[:punct:]]character[[:punct:]]\\."
    )
    expect_equal(actual, expected_catz)
  }
)


test_that(
  "get_extension works with paths with no directory and a single extension in the filename.",
  {
    x <- "foo.tgz"
    expected <- "tgz"
    names(expected) <- x
    expect_equal(get_extension(x), expected)
  }
)

test_that(
  "get_extension works with paths with a directory and a single extension in the filename.",
  {
    x <- "somedir/foo.tgz"
    expected <- "tgz"
    names(expected) <- x
    expect_equal(get_extension(x), expected)
  }
)

test_that(
  "get_extension works with paths with no directory and a double extension in the filename.",
  {
    x <- "foo.tar.gz"
    expected <- "tar.gz"
    names(expected) <- x
    expect_equal(get_extension(x), expected)
  }
)

test_that(
  "get_extension works with paths with a directory and a double extension in the filename.",
  {
    x <- "somedir/foo.tar.gz"
    expected <- "tar.gz"
    names(expected) <- x
    expect_equal(get_extension(x), expected)
  }
)

test_that(
  "get_extension works with paths with no directory and no extension in the filename.",
  {
    x <- "foo"
    expected <- ""
    names(expected) <- x
    expect_equal(get_extension(x), expected)
  }
)

test_that(
  "get_extension works with paths with a directory and no extension in the filename.",
  {
    x <- "somedir/foo"
    expected <- ""
    names(expected) <- x
    expect_equal(get_extension(x), expected)
  }
)

test_that(
  "get_extension handles filenames containing a '.' and an extension.",
  {
    x <- "foo. bar.zip"
    expected <- "zip"
    names(expected) <- x
    expect_equal(get_extension(x), expected)
  }
)

test_that(
  "get_extension handles directories.",
  {
    x <- R.home()
    expected <- ""
    names(expected) <- x
    expect_equal(get_extension(x), expected)
  }
)

test_that(
  "strip_extension works with paths with no directory and a single extension in the filename.",
  {
    x <- "foo.tgz"
    pwd <- getwd()
    expected <- "foo"
    names(expected) <- x
    expected2 <- file.path(pwd, "foo")
    names(expected2) <- x
    expect_equal(strip_extension(x, include_dir = FALSE), expected)
    expect_equal(strip_extension(x), expected2)
  }
)

test_that(
  "strip_extension works with paths with a directory and a single extension in the filename.",
  {
    x <- "somedir/foo.tgz"
    pwd <- getwd()
    expected <- "foo"
    names(expected) <- x
    expected2 <- file.path(pwd, "somedir", "foo")
    names(expected2) <- x
    expect_equal(strip_extension(x, include_dir = FALSE), expected)
    expect_equal(strip_extension(x), expected2)
  }
)

test_that(
  "strip_extension works with paths with no directory and a double extension in the filename.",
  {
    x <- "foo.tar.gz"
    pwd <- getwd()
    expected <- "foo"
    names(expected) <- x
    expected2 <- file.path(pwd, "foo")
    names(expected2) <- x
    expect_equal(strip_extension(x, include_dir = FALSE), expected)
    expect_equal(strip_extension(x), expected2)
  }
)

test_that(
  "strip_extension works with paths with a directory and a double extension in the filename.",
  {
    x <- "somedir/foo.tar.gz"
    pwd <- getwd()
    expected <- "foo"
    names(expected) <- x
    expected2 <- file.path(pwd, "somedir", "foo")
    names(expected2) <- x
    expect_equal(strip_extension(x, include_dir = FALSE), expected)
    expect_equal(strip_extension(x), expected2)
  }
)

test_that(
  "strip_extension works with paths with no directory and no extension in the filename.",
  {
    x <- "foo"
    pwd <- getwd()
    expected <- "foo"
    names(expected) <- x
    expected2 <- file.path(pwd, "foo")
    names(expected2) <- x
    expect_equal(strip_extension(x, include_dir = FALSE), expected)
    expect_equal(strip_extension(x), expected2)
  }
)

test_that(
  "strip_extension works with paths with a directory and no extension in the filename.",
  {
    x <- "somedir/foo"
    pwd <- getwd()
    expected <- "foo"
    names(expected) <- x
    expected2 <- file.path(pwd, "somedir", "foo")
    names(expected2) <- x
    expect_equal(strip_extension(x, include_dir = FALSE), expected)
    expect_equal(strip_extension(x), expected2)
  }
)

test_that(
  "strip_extension handles filenames containing a '.' and an extension.",
  {
    x <- "foo. bar.zip"
    pwd <- getwd()
    expected <- "foo. bar"
    names(expected) <- x
    expected2 <- file.path(pwd, "foo. bar")
    names(expected2) <- x
    expect_equal(strip_extension(x, include_dir = FALSE), expected)
    expect_equal(strip_extension(x), expected2)
  }
)

test_that(
  "strip_extension handles directories.",
  {
    x <- R.home()
    pwd <- getwd()
    expected <- ""
    names(expected) <- x
    expected2 <- normalizePath(x, "/", mustWork = FALSE)
    names(expected2) <- x
    expect_equal(strip_extension(x, include_dir = FALSE), expected)
    expect_equal(strip_extension(x), expected2)
  }
)


test_that(
  "replace_extension works correctly",
  {
    x <- c(
      "somedir/foo.tgz",         # single extension
      "another dir\\bar.tar.gz", # double extension
      "baz",                     # no extension
      "quux. quuux.tbz2",        # single ext, dots in filename
      r_home()                   # a dir
    )
    new_extension <- "NEW"
    expected <- c(
      paste(
        standardize_path(
          c("somedir/foo", "another dir/bar", "baz", "quux. quuux"), 
        ), 
        new_extension, 
        sep = "."
      ),
      r_home()
    )
    names(expected) <- x
    expect_warning(
      actual <- replace_extension(x, new_extension), 
      "The directories .* have no file extensions to replace."
    )
    expect_identical(actual, expected)
  }
)




