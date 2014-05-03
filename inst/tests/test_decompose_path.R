library(testthat)
library(assertive)
library(stringr)

test_that(
  "decompose_path works with a zero length input",
  {
    x <- character()
    expected <- data.frame(dirname = x, filename = x, extension = x)
    expect_equal(decompose_path(x), expected)
  }
)

test_that(
  "decompose_path handles filenames with directories, a variety of file extensions, and dots in filenames.",
  {
    x <- c(
      "somedir/foo.tgz",         # single extension
      "another dir\\bar.tar.gz", # double extension
      "baz",                     # no extension
      "quux. quuux.tbz2",        # single ext, dots in filename
      R.home(),                  # a dir
      "~",                       # another dir
      "~/quuuux.tar.xz",         # a file in a dir
      "",                        # empty 
      ".",                       # current dir
      "..",                      # parent dir
      NA_character_              # missing
    )
    pwd <- getwd()
    expected <- structure(
      data.frame(
        dirname = c(
          file.path(pwd, "somedir"), 
          file.path(pwd, "another dir"),
          pwd, 
          pwd, 
          normalizePath(R.home(), "/"),
          path.expand("~"),
          path.expand("~"), 
          "",
          pwd, 
          dirname(pwd),
          NA
        ),
        filename = c(
          "foo", "bar", "baz", "quux. quuux", "", "", "quuuux", "", "", "", NA
        ),
        extension = c(
          "tgz", "tar.gz", "", "tbz2", "", "", "tar.xz", "", "", "", NA
        ),
        row.names = ifelse(is.na(x), "<NA>", x), 
        stringsAsFactors = FALSE
      ), 
      class = c("decomposed_path", "data.frame")
    )
    
    expect_equal(decompose_path(x), expected)
  }
)

test_that(
  "decompose_path returns a zero row matrix with an empty character vector or NULL as an input.",
  {
    x <- character()
    x2 <- NULL
    expected <- matrix(
      character(),
      ncol     = 3,
      dimnames = list(x, c("dirname", "filename", "extension"))
    )
    expect_equal(decompose_path(x), expected)
    expect_warning(answer <- decompose_path(x2), "Coercing .+ to class 'character'\\.")
    expect_equal(answer, expected)
  }
)

test_that(
  "decompose_path returns a zero row matrix with an empty character vector or NULL as an input.",
  {
    x <- factor(
      c(
        "catz/lolcat.gif", 
        "moar cats/nyan cat.jpeg", 
        "catz\\catz in loft\\ceiling cat.jpg", 
        "catz/musical catz\\keyboard cat.bmp", 
        "catbread.png", 
        "kitties\\bonsai kitten.tiff", 
        "kitties\\hipster kitty.pdf"
      )
    )
    expected <- matrix(
      c(
        "catz", "moar cats", "catz/catz in loft", "catz/musical catz", ".", "kitties", "kitties",
        "lolcat", "nyan cat", "ceiling cat", "keyboard cat", "catbread", "bonsai kitten", "hipster kitty",
        "gif", "jpeg", "jpg", "bmp", "png", "tiff", "pdf"
      ),
      ncol     = 3,
      dimnames = list(x, c("dirname", "filename", "extension"))
    )    
    expect_warning(answer <- decompose_path(x), "Coercing .+ to class 'character'\\.")
    expect_equal(answer, expected)
  }
)






