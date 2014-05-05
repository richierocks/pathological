library(testthat)
library(assertive)
library(stringr)

test_that(
  "decompose_path works with a zero length input",
  {
<<<<<<< HEAD
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
=======
    x <- character()
    x2 <- NULL
    expected <- structure(
      data.frame(
        dirname = character(), 
        filename = character(), 
        extension = character(),
        stringsAsFactors = FALSE
      ),
      class = c("decomposed_path", "data.frame")
    )
    expect_equal(decompose_path(x), expected)
    expect_equal(decompose_path(x2), expected)
>>>>>>> master
  }
)

test_that(
  "decompose_path handles filenames with directories, a variety of file extensions, and dots in filenames.",
  {
<<<<<<< HEAD
    x <- character()
    x2 <- NULL
    expected <- structure(matrix(
      character(),
      ncol     = 3,
      dimnames = list(x, c("dirname", "filename", "extension"))
    ), class = c("decomposed_path", "matrix"))
=======
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
    
>>>>>>> master
    expect_equal(decompose_path(x), expected)
  }
)

test_that(
  "decompose_path works with a factor input.",
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
<<<<<<< HEAD
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
    
=======
    pwd <- getwd()
    expected <- structure(
      data.frame(
        dirname = c(
          file.path(pwd, "catz"),
          file.path(pwd, "moar cats"),
          file.path(pwd, "catz/catz in loft"),
          file.path(pwd, "catz/musical catz"), getwd(),
          file.path(pwd, "kitties"),
          file.path(pwd, "kitties")
        ),
        filename = c(
          "lolcat", "nyan cat", "ceiling cat", "keyboard cat", 
          "catbread", "bonsai kitten", "hipster kitty"
        ),
        extension = c(
          "gif", "jpeg", "jpg", "bmp", "png", "tiff", "pdf"
        ),
        row.names = x,
        stringsAsFactors = FALSE
      ), 
      class = c("decomposed_path", "matrix")
    )    
>>>>>>> master
    expect_warning(answer <- decompose_path(x), "Coercing .+ to class 'character'\\.")
    expect_equal(answer, expected)
  }
)






