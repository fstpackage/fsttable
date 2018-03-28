
context("interface")

# clean testdata directory
if (dir.exists("tempdata")) {
  unlink("tempdata", recursive = TRUE, force = TRUE)
}

dir.create("tempdata")

# some test data in a fst_table reference
x <- data.frame(X = 1:100, Y = LETTERS[1 + (1:100) %% 26])
fst::write_fst(x, "tempdata/1.fst")
ft <- fst_table("tempdata/1.fst")

test_that("empty i and j", {

  ft_copy <- ft[]
  expect_identical(ft, ft_copy)
})


test_that("column selection", {

  # integer selection
  ft_row_selection <- ft[1:10]

  expect_equal(nrow(ft_row_selection), 10)
})
