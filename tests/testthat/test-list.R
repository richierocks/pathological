context("list")

test_that("listing files works", {
  list_files() %>% expect_s3_class("tbl_df")
  
  self <- list_files(system.file(package = "pathological"))
  expect_gt(nrow(self), 2)
  base <- list_files(system.file(package = "base"), full.names = FALSE)
  expect_true(all(c("CITATION", "DESCRIPTION", "INDEX") %in% base$filename))
  
  td <- tempdir()
  expect_lt(nrow(list_files(td)), 1L) 
  })

# # A tibble: 8 x 1
#      filename
#         <chr>
# 1    CITATION
# 2        demo
# 3 DESCRIPTION
# 4        help
# 5        html
# 6       INDEX
# 7        Meta
# 8           R
