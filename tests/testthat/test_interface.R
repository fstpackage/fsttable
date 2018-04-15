
context("interface")

# clean testdata directory
if (dir.exists("tempdata")) {
  unlink("tempdata", recursive = TRUE, force = TRUE)
}

dir.create("tempdata")

# some test data
x <- data.table(X = 1:100, Y = LETTERS[1 + (1:100) %% 26])
fst::write_fst(x, "1.fst")

# creates an instance of a `datatableproxy` class with
# a `fstproxy` class as the remote proxy
ft <- fsttable::fst_table("1.fst")


# collect the ft and compares the result to the in-memory data.table
check_interface <- function(ft, dt, i, j) {
  expect_equal(ft[i, j, collect = TRUE], dt[i, j])
}


test_that("empty i and j", {
  ft_copy <- ft[]
  expect_identical(ft, ft_copy)
})


test_that("row selection", {

  # integer selection
  check_interface(ft, x, 1:10)

  # logical selection
  check_interface(ft, x, rep(c(FALSE, TRUE, TRUE, FALSE), 25))

  # equivalent selection results in same structure
  ft1 <- ft[5:1]
  ft2 <- ft[1:5][5:1]
  expect_identical(ft1, ft2)

  # incorrect logical length
  expect_error(ft[c(TRUE, FALSE)], "Recycling of logical i is not allowed with data.table")
})
