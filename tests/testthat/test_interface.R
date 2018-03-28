
context("interface")

# clean testdata directory
if (dir.exists("tempdata")) {
  unlink("tempdata", recursive = TRUE, force = TRUE)
}

dir.create("tempdata")

# some test data
x <- data.frame(X = 1:100, Y = LETTERS[1 + (1:100) %% 26])
fst::write_fst(x, "1.fst")

# creates an instance of a `datatableproxy` class with
# a `fstproxy` class as the remote proxy
ft <- fsttable::fst_table("1.fst")


test_that("empty i and j", {

  ft_copy <- ft[]
  expect_identical(ft, ft_copy)
})


test_that("column selection", {

  # integer selection
  ft2 <- ft[, .(Y)]

  expect_equal(ncol(ft2), 1)
})

test1 <- function() {
  ft2 <- ft[, .(Y)]
}

test1()

test_that("row selection", {

  # integer selection
  ft_row_selection <- ft[1:10]

  expect_equal(nrow(ft_row_selection), 10)
})
