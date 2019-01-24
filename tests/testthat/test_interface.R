
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


expect_identical_table_proxy <- function(ft1, ft2) {
  expect_identical(fsttable:::.get_table_proxy(ft1), fsttable:::.get_table_proxy(ft2))
}


test_that("empty i and j", {
  ft_copy <- ft[]
  expect_identical_table_proxy(ft, ft_copy)
})


test_that("row selection", {

  # integer selection
  expect_equal(ft[1:10, collect = TRUE], x[1:10, ])

  # logical selection
  mask <- rep(c(FALSE, TRUE, TRUE, FALSE), 25)
  expect_equal(ft[mask, collect = TRUE], x[mask, ])

  # equivalent selection results in same structure
  ft1 <- ft[5:1]
  ft2 <- ft[1:5][5:1]
  expect_identical(ft1, ft2)

  # incorrect logical length
  expect_error(ft[c(TRUE, FALSE)], "Recycling of logical i is not allowed with data.table")

  # full selection should return identical table
  expect_identical_table_proxy(ft, ft[1:nrow(ft)])
  expect_identical_table_proxy(ft, ft[rep(TRUE, nrow(ft))])
})


test_that("row selection", {

  # dot single column selection
  expect_equal(ft[, .(Z = X), collect = TRUE], x[, .(Z = X)])

  # dot multiple identical column selection
  expect_equal(ft[, .(Z = X, W = X), collect = TRUE], x[, .(Z = X, W = X)])

  # dot multiple identical column selection
  expect_equal(ft[, .(Z = X, W = X), collect = TRUE], x[, .(Z = X, W = X)])

  # column name override
  expect_equal(ft[, .(X = Y), collect = TRUE], x[, .(X = Y)])
})
