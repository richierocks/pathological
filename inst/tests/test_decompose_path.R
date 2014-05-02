library(testthat)
library(assertive)
library(stringr)

test_that(
  "decompose_path handles filenames with directories, a variety of file extensions, and dots in filenames.",
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
    expected <- structure(matrix(
      c(
        file.path(getwd(), "somedir"), file.path(getwd(), "another dir"), 
        getwd(), getwd(), dirname("~/ "), "", 
        getwd(), dirname(getwd()), NA,
        "foo", "bar", "baz", "quux. quuux", "quuuux", "", "", "", NA,
        "tgz", "tar.gz", "", "tbz2", "tar.xz", "", "", "", NA
      ),
      ncol     = 3,
      dimnames = list(x, c("dirname", "filename", "extension"))
    ), class = c("decomposed_path", "matrix"))
    
    expect_equal(
        res <- decompose_path(x), 
        expected
    )
  }
)

test_that(
  "decompose_path returns a zero row matrix with an empty character vector or NULL as an input.",
  {
    x <- character()
    x2 <- NULL
    expected <- structure(matrix(
      character(),
      ncol     = 3,
      dimnames = list(x, c("dirname", "filename", "extension"))
    ), class = c("decomposed_path", "matrix"))
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
    expected <- structure(matrix(
      c(
        file.path(getwd(), "catz"), 
        file.path(getwd(), "moar cats"), 
        file.path(getwd(), "catz/catz in loft"), 
        file.path(getwd(), "catz/musical catz"), getwd(), 
        file.path(getwd(), "kitties"), 
        file.path(getwd(), "kitties"),
        "lolcat", "nyan cat", "ceiling cat", "keyboard cat", "catbread", "bonsai kitten", "hipster kitty",
        "gif", "jpeg", "jpg", "bmp", "png", "tiff", "pdf"
      ),
      ncol     = 3,
      dimnames = list(x, c("dirname", "filename", "extension"))
    ), class = c("decomposed_path", "matrix"))
    
    expect_warning(answer <- decompose_path(x), "Coercing .+ to class 'character'\\.")
    expect_equal(answer, expected)
  }
)






