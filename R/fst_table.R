#  fsttable - A 'data.table' interface to on-disk fst files.
#
#  Copyright (C) 2017-present, Mark AJ Klik
#
#  This file is part of the fsttable R package.
#
#  The fsttable R package is free software: you can redistribute it and/or modify it
#  under the terms of the GNU Affero General Public License version 3 as
#  published by the Free Software Foundation.
#
#  The fsttable R package is distributed in the hope that it will be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
#  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
#  for more details.
#
#  You should have received a copy of the GNU Affero General Public License along
#  with the fsttable R package. If not, see <http://www.gnu.org/licenses/>.
#
#  You can contact the author at:
#  - fsttable R package source repository : https://github.com/fstpackage/fsttable


#' Access a fst file like a regular data frame
#'
#' Create a fsttable object that can be accessed like a regular data frame. This object
#' is just a reference to the actual data and requires only a small amount of memory.
#' When data is accessed, only the requested subset is read from file. This is possible
#' because the fst file format allows full random access (in columns and rows) to the stored
#' dataset.
#'
#' @param path Path to a fst file
#' @param old_format Use \code{old_format = TRUE} when referencing a fst file that was created
#' with a fst package version lower than 0.8.0
#'
#' @return An object of class \code{fsttable}
#' @export
#' @examples
#' \dontrun{
#' # generate a sample fst file
#' path <- paste0(tempfile(), ".fst")
#' write_fst(iris, path)
#'
#' # create a fsttable object that can be used as a data frame
#' ft <- fst(path)
#'
#' # print head and tail
#' print(ft)
#'
#' # select columns and rows
#' x <- ft[10:14, c("Petal.Width", "Species")]
#'
#' # use the common list interface
#' ft[TRUE]
#' ft[c(TRUE, FALSE)]
#' ft[["Sepal.Length"]]
#' ft$Petal.Length
#'
#' # use data frame generics
#' nrow(ft)
#' ncol(ft)
#' dim(ft)
#' dimnames(ft)
#' colnames(ft)
#' rownames(ft)
#' names(ft)
#' }
fst_table <- function(path, old_format = FALSE) {

  # The fsttable object is wrapped in a data.table to keep code completion working in RStudio
  # Each column corresponds to an actual column in the underlying fst file

  meta <- fst::metadata_fst(path, old_format)

  dt <- data.table::as.data.table(matrix(rep(0, 1 + length(meta$columnBaseTypes)), nrow = 1))
  colnames(dt) <- c(meta$columnNames, ".FstData")

  fsttable_data <- list(
    meta = meta,
    col_selection = NULL,
    row_selection = NULL,
    old_format = old_format
  )

  dt[, .FstData := list(list(fsttable_data))]

  # class attribute
  class(dt) <- c("fsttable", "data.table", "data.frame")

  dt
}


.column_indexes_fst <- function(meta_info, j) {

  # test correct column names
  if (is.character(j) ) {
    wrong <- !(j %in% meta_info$columnNames)

    if (any(wrong)) {
      names <- j[wrong]
      stop(sprintf("Undefined columns: %s", paste(names, collapse = ", ")))
    }
  } else if (is.logical(j)) {

    if (any(is.na(j))) {
      stop("Subscript out of bounds.", call. = FALSE)
    }

    j <- meta_info$columnNames[j]
  } else if (is.numeric(j)) {
    if (any(is.na(j))) {
      stop("Subscript out of bounds.", call. = FALSE)
    }

    if (any(j <= 0)) {
      stop("Only positive column indexes supported.", call. = FALSE)
    }

    if (any(j > length(meta_info$columnBaseTypes))) {
      stop("Subscript out of bounds.", call. = FALSE)
    }

    j <- meta_info$columnNames[j]
  }

  j
}


#' @export
`[.fsttable` <- function(x, i, j, by, keyby, with = TRUE,
  nomatch = getOption("datatable.nomatch"), mult = "all", roll = FALSE,
  rollends = if (roll == "nearest") c(TRUE,TRUE) else if (roll >= 0) c(FALSE,TRUE) else c(TRUE,FALSE),
  which = FALSE, .SDcols, verbose = getOption("datatable.verbose"),
  allow.cartesian = getOption("datatable.allow.cartesian"),
  drop = NULL, on = NULL) {
  
  if (!is.null(drop)) {
    warning("drop ignored", call. = FALSE)
  }

  meta_info <- .subset2(x, "meta")

  # read the full table and return the result
  # when only i is present, we do a column subsetting

  if (missing(i) && missing(j)) {
    return(read_fst(meta_info$path, old_format = .subset2(x, "old_format")))
  }

  if (nargs() <= 2) {

    # return full table
    if (missing(i)) {
      # we have a j
      j <- .column_indexes_fst(meta_info, j)
      return(read_fst(meta_info$path, j, old_format = .subset2(x, "old_format")))
    }

    # i is interpreted as j
    j <- .column_indexes_fst(meta_info, i)
    return(read_fst(meta_info$path, j, old_format = .subset2(x, "old_format")))
  }

  # return all rows

  # special case where i is interpreted as j: select all rows
  if (nargs() == 3 && !missing(drop) && !missing(i)) {
    j <- .column_indexes_fst(meta_info, i)
    return(read_fst(meta_info$path, j, old_format = .subset2(x, "old_format")))
  }

  # i and j not reversed

  # full columns
  if (missing(i)) {
    j <- .column_indexes_fst(meta_info, j)
    return(read_fst(meta_info$path, j, old_format = .subset2(x, "old_format")))
  }


  # determine integer vector from i
  if (!(is.numeric(i) || is.logical(i))) {
    stop("Row selection should be done using a numeric or logical vector", call. = FALSE)
  }

  if (is.logical(i)) {
    i <- which(i)
  }

  # cast to integer and determine row range
  i <- as.integer(i)
  min_row <- min(i)
  max_row <- max(i)

  # boundary check
  if (min_row < 0) {
    stop("Row selection out of range")
  }

  if (max_row > meta_info$nrOfRows) {
    stop("Row selection out of range")
  }

  # column subset

  # select all columns
  if (missing(j)) {
    fst_data <- read_fst(meta_info$path, from = min_row, to = max_row, old_format = .subset2(x, "old_format"))

    return(fst_data[1 + i - min_row, ])
  }

  j <- .column_indexes_fst(meta_info, j)
  fst_data <- read_fst(meta_info$path, j, from = min_row, to = max_row, old_format = .subset2(x, "old_format"))

  fst_data[1 + i - min_row, ]
}
