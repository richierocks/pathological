create_expected_decomposed_path <- function(dirname, filename, extension, row.names)
{
  out <- structure(
    data.frame(
      dirname          = dirname,
      filename         = filename,
      extension        = extension,
      stringsAsFactors = FALSE
    ), 
    class = c("decomposed_path", "data.frame")
  )
  rownames(out) <- row.names
  out
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
      row.names   = character()
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
      row.names        = x
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

pwd <- getwd()
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
  row.names = catz
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
    expected <- list(
      na    = "foo", 
      true  = file.path(getwd(), "foo", fsep = "/"), 
      false = "foo"
    )
    expected <- lapply(expected, function(y) setNames(y, x))
    expect_equal(strip_extension(x), expected$na)
    expect_equal(strip_extension(x, include_dir = TRUE), expected$true)
    expect_equal(strip_extension(x, include_dir = FALSE), expected$false)
  }
)

test_that(
  "strip_extension works with paths with a directory and a single extension in the filename.",
  {
    x <- "somedir/foo.tgz"
    expected <- list(
      na    = "somedir/foo", 
      true  = file.path(getwd(), "somedir", "foo", fsep = "/"), 
      false = "foo"
    )
    expected <- lapply(expected, function(y) setNames(y, x))
    expect_equal(strip_extension(x), expected$na)
    expect_equal(strip_extension(x, include_dir = TRUE), expected$true)
    expect_equal(strip_extension(x, include_dir = FALSE), expected$false)
  }
)

test_that(
  "strip_extension works with paths with no directory and a double extension in the filename.",
  {
    x <- "foo.tar.gz"
    expected <- list(
      na    = "foo", 
      true  = file.path(getwd(), "foo", fsep = "/"), 
      false = "foo"
    )
    expected <- lapply(expected, function(y) setNames(y, x))
    expect_equal(strip_extension(x), expected$na)
    expect_equal(strip_extension(x, include_dir = TRUE), expected$true)
    expect_equal(strip_extension(x, include_dir = FALSE), expected$false)
  }
)

test_that(
  "strip_extension works with paths with a directory and a double extension in the filename.",
  {
    x <- "somedir/foo.tar.gz"
    expected <- list(
      na    = "somedir/foo", 
      true  = file.path(getwd(), "somedir", "foo", fsep = "/"), 
      false = "foo"
    )
    expected <- lapply(expected, function(y) setNames(y, x))
    expect_equal(strip_extension(x), expected$na)
    expect_equal(strip_extension(x, include_dir = TRUE), expected$true)
    expect_equal(strip_extension(x, include_dir = FALSE), expected$false)
  }
)

test_that(
  "strip_extension works with paths with no directory and no extension in the filename.",
  {
    x <- "foo"
    expected <- list(
      na    = "foo", 
      true  = file.path(getwd(), "foo", fsep = "/"), 
      false = "foo"
    )
    expected <- lapply(expected, function(y) setNames(y, x))
    expect_equal(strip_extension(x), expected$na)
    expect_equal(strip_extension(x, include_dir = TRUE), expected$true)
    expect_equal(strip_extension(x, include_dir = FALSE), expected$false)
  }
)

test_that(
  "strip_extension works with paths with a directory and no extension in the filename.",
  {
    x <- "somedir/foo"
    expected <- list(
      na    = "somedir/foo", 
      true  = file.path(getwd(), "somedir", "foo", fsep = "/"), 
      false = "foo"
    )
    expected <- lapply(expected, function(y) setNames(y, x))
    expect_equal(strip_extension(x), expected$na)
    expect_equal(strip_extension(x, include_dir = TRUE), expected$true)
    expect_equal(strip_extension(x, include_dir = FALSE), expected$false)
  }
)

test_that(
  "strip_extension handles filenames containing a '.' and an extension.",
  {
    x <- "foo. bar.zip"
    expected <- list(
      na    = "foo. bar", 
      true  = file.path(getwd(), "foo. bar", fsep = "/"), 
      false = "foo. bar"
    )
    expected <- lapply(expected, function(y) setNames(y, x))
    expect_equal(strip_extension(x), expected$na)
    expect_equal(strip_extension(x, include_dir = TRUE), expected$true)
    expect_equal(strip_extension(x, include_dir = FALSE), expected$false)
  }
)

test_that(
  "strip_extension handles directories.",
  {
    x <- R.home()
    expected <- list(
      na    = x, 
      true  = r_home(), 
      false = ""
    )
    expected <- lapply(expected, function(y) setNames(y, x))
    expect_equal(strip_extension(x), expected$na)
    expect_equal(strip_extension(x, include_dir = TRUE), expected$true)
    expect_equal(strip_extension(x, include_dir = FALSE), expected$false)
  }
)


test_that(
  "replace_extension handles filenames with a single extension.",
  {
    x <- "somedir/foo.tgz"
    new_extension <- "NEW"
    expected <- list(
      na    = "somedir/foo.NEW", 
      true  = file.path(getwd(), "somedir", "foo.NEW", fsep = "/"), 
      false = "foo.NEW"
    )
    expected <- lapply(expected, function(y) setNames(y, x))
    expect_equal(
      replace_extension(x, new_extension), 
      expected$na
    )
    expect_equal(
      replace_extension(x, new_extension, include_dir = TRUE), 
      expected$true
    )
    expect_equal(
      replace_extension(x, new_extension, include_dir = FALSE), 
      expected$false
    )
  }
)

test_that(
  "replace_extension handles filenames with a double extension.",
  {
    x <- "somedir/foo.tar.gz"
    new_extension <- "NEW"
    expected <- list(
      na    = "somedir/foo.NEW", 
      true  = file.path(getwd(), "somedir", "foo.NEW", fsep = "/"), 
      false = "foo.NEW"
    )
    expected <- lapply(expected, function(y) setNames(y, x))
    expect_equal(
      replace_extension(x, new_extension), 
      expected$na
    )
    expect_equal(
      replace_extension(x, new_extension, include_dir = TRUE), 
      expected$true
    )
    expect_equal(
      replace_extension(x, new_extension, include_dir = FALSE), 
      expected$false
    )
  }
)

test_that(
  "replace_extension handles filenames with no extension.",
  {
    x <- "somedir/foo"
    new_extension <- "NEW"
    expected <- list(
      na    = "somedir/foo.NEW", 
      true  = file.path(getwd(), "somedir", "foo.NEW", fsep = "/"), 
      false = "foo.NEW"
    )
    expected <- lapply(expected, function(y) setNames(y, x))
    expect_equal(
      replace_extension(x, new_extension), 
      expected$na
    )
    expect_equal(
      replace_extension(x, new_extension, include_dir = TRUE), 
      expected$true
    )
    expect_equal(
      replace_extension(x, new_extension, include_dir = FALSE), 
      expected$false
    )
  }
)

test_that(
  "replace_extension handles directories.",
  {
    # This has to be a real directory since it is not possible to tell if
    # a non-existent 'foo' refers to a directory or filename.
    x <- R.home()
    new_extension <- "NEW"
    expected <- list(
      na    = x, 
      true  = r_home(), 
      false = ""
    )
    expected <- lapply(expected, function(y) setNames(y, x))
    error_rx <- "The directories .* have no file extensions to replace."
    actual <- list()
    expect_warning(
      actual$na <- replace_extension(x, new_extension), 
      error_rx
    )
    expect_warning(
      actual$true <- replace_extension(x, new_extension, include_dir = TRUE), 
      error_rx
    )
    expect_warning(
      actual$false <- replace_extension(x, new_extension, include_dir = FALSE), 
      error_rx
    )
    expect_equal(actual$na, expected$na)
    expect_equal(actual$true, expected$true)
    expect_equal(actual$false, expected$false)
  }
)

test_that(
  "replace_extension handles empty replacement extensions.",
  {
    x <- "somedir/foo.tgz"
    new_extension <- ""
    expected <- list(
      na    = "somedir/foo.", 
      true  = file.path(getwd(), "somedir", "foo.", fsep = "/"), 
      false = "foo."
    )
    expected <- lapply(expected, function(y) setNames(y, x))
    error_rx <- "'new_extension' is empty.  Did you want strip_extension instead?"
    actual <- list()
    expect_warning(
      actual$na <- replace_extension(x, new_extension), 
      error_rx
    )
    expect_warning(
      actual$true <- replace_extension(x, new_extension, include_dir = TRUE), 
      error_rx
    )
    expect_warning(
      actual$false <- replace_extension(x, new_extension, include_dir = FALSE), 
      error_rx
    )
    expect_equal(actual$na, expected$na)
    expect_equal(actual$true, expected$true)
    expect_equal(actual$false, expected$false)
  }
)


