library(testthat)

std_getwd <- function() 
{
  pwd <- getwd()
  # Enforce upper case Windows drive letters
  substring(pwd, 1, 1) <- toupper(substring(pwd, 1, 1))
  pwd
}

test_check("pathological")
