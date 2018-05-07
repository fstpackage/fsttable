#  fsttable - A 'data.table' interface to on-disk fst files.
#
#  Copyright (C) 2017-present, Mark AJ Klik and Martin Blostein
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


table_proxy_nrow <- function(tbl_proxy) {
  tbl_proxy$remotetablestate$nrow
}


table_proxy_ncol <- function(tbl_proxy) {
  tbl_proxy$remotetablestate$ncol
}


table_proxy_colnames <- function(tbl_proxy) {
  tbl_proxy$remotetablestate$colnames
}


table_proxy_column_types <- function(tbl_proxy, column_index = NULL) {

  # TODO: test type and range of column_index here

  if (!is.null(column_index)) {
    return(tbl_proxy$remotetablestate$coltypes[column_index])
  }

  tbl_proxy$remotetablestate$coltypes
}


#' Read a subset of the proxy table to memory
#'
#' @param tbl_proxy a `table_proxy` object
#' @param from_row start row (of table_proxy)
#' @param to_row end row (of table_proxy)
#' @param col_names names of the columns that you want to retrieve
#'
#' @return a data.frame object with the requested data
#' @export
#' @md
table_proxy_read_range <- function(tbl_proxy, from_row, to_row, col_names = NULL) {

  rtable <- tbl_proxy$remotetable
  rtable_state <- tbl_proxy$remotetablestate

  # determine columns to read
  on_disk <- sapply(rtable_state$colexps, function(expr) {
    is.name(expr) && (as.character(expr) %in% rtable_colnames(rtable))
  })
  cols_to_read <- as.character(sapply(rtable_state$colexps[on_disk], deparse))

  # determine rows to read
  slice_map <- rtable_state$slice_map

  if (is.null(slice_map)) {
    # no previous slice map, use arguments for row subset
    result <- rtable_read_range(rtable, from_row, to_row, cols_to_read)
  } else {
    # calculate new slice map
    slice_map <- slice_map[from_row:to_row]

    # order slice map and read row subset
    min_row <- min(slice_map)
    max_row <- max(slice_map)

    # very inefficient partial read
    # TODO: create a read mask
    row_range <- rtable_read_range(rtable, min_row, max_row, cols)

    # read contiguous extent of selected rows, then filter & order
    row_range[1 + slice_map - min_row, , drop = FALSE]

  }

  if (ncol(result) > 0) {
    setnames(result, rtable_state$colnames[on_disk])
  }

  result <- .add_virtual_cols(result, rtable_state, on_disk, to_row - from_row + 1)

  return(result)
}


#' Read the all data selected by the table_proxy into memory, and put into the specified order
#'
#' For now, all rows in the full range are read in, and the uneeded rows are discarded.
#'
#' @param tbl_proxy a `table_proxy` object
#' @param col_names names of the columns to retrieve. If NULL, all columns in the tbl_proxy will be returned
#'
#' @return a data.frame object with the requested data
#' @export
#' @md
table_proxy_read_full <- function(tbl_proxy, col_names = NULL) {

  rtable <- tbl_proxy$remotetable
  rtable_state <- tbl_proxy$remotetablestate

  # determine columns to read
  on_disk <- sapply(rtable_state$colexps, function(expr) {
    is.name(expr) && (as.character(expr) %in% rtable_colnames(rtable))
  })
  cols_to_read <- as.character(sapply(rtable_state$colexps[on_disk], deparse))

  # determine rows to read
  slice_map <- rtable_state$slice_map

  if (is.null(slice_map)) {
    # read all rows
    result <- rtable_read_full(rtable, cols_to_read)
  } else if (length(slice_map) == 0) {
    # empty table
    result <- rtable_read_range(rtable, 1, 2, cols_to_read)
  } else {
    # order slice map and read row subset
    min_row <- min(slice_map)
    max_row <- max(slice_map)

    # very inefficient partial read
    # TODO: create a read mask
    result <- rtable_read_range(rtable, min_row, max_row, cols_to_read)

    # read contiguous extent of selected rows, then filter & order
    result <- result[1 + slice_map - min_row, , drop = FALSE]
  }

  if (ncol(result) > 0) {
    setnames(result, rtable_state$colnames[on_disk])
  }

  result <- .add_virtual_cols(result, rtable_state, on_disk, rtable_state$nrow)

  return(result)
}


#' Apply a binary row-selection operation on the current table_proxy state.
#' This operation will not change the slice map ordering

#'
#' @param tbl_proxy a table proxy object
#' @param i a logical vector with the required rows set to TRUE
#'
#' @return a table proxy object with the new state
#' @export
table_proxy_select_row_mask <- function(tbl_proxy, i) {

  # In the current implementation, the table proxy state can contain only a single
  # row selection filter. This method will apply filter i to the existing slice map.

  # if full selection, don't alter table state
  if (all(i)) {
    return(tbl_proxy)
  }

  # current slice map
  slice_map <- tbl_proxy$remotetablestate$slice_map

  # set equal to selected slice map
  if (is.null(slice_map)) {
    row_selection <- which(i)
    tbl_proxy$remotetablestate$slice_map <- row_selection
    tbl_proxy$remotetablestate$nrow <- length(row_selection)
    return(tbl_proxy)
  }

  # slice the current clice map
  tbl_proxy$remotetablestate$slice_map <- slice_map[i]

  # update nrow
  tbl_proxy$remotetablestate$nrow <- length(i)

  tbl_proxy
}


#' Apply a row-selection operation on the current table_proxy state
#' This operation could change the slice map ordering
#'
#' @param tbl_proxy a table proxy object
#' @param i an integer vector with the selected rows
#'
#' @return a table proxy object with the new state
#' @export
table_proxy_select_rows <- function(tbl_proxy, i) {

  # In the current implementation, the table proxy state can contain only a single
  # row selection filter. This method will apply filter i to the existing slice map.

  # current slice map
  slice_map <- tbl_proxy$remotetablestate$slice_map

  # if full ordered selection, don't alter table state
  if (identical(i, seq(1, tbl_proxy$remotetablestate$nrow))) {
    return(tbl_proxy)
  }

  # set equal to selected slice map
  # slice map ordering does not change

  if (is.null(slice_map)) {
    # set slice map equal to new selection
    tbl_proxy$remotetablestate$slice_map <- i
  } else {
    # apply new selection to existing slice map
    tbl_proxy$remotetablestate$slice_map <- slice_map[i]
  }

  # update nrow
  tbl_proxy$remotetablestate$nrow <- length(i)

  # determine if the ordering has changed
  # method is.unsorted (base) is fast and needs no real sorting
  if (tbl_proxy$remotetablestate$slice_map_ordered) {
    if (is.unsorted(i)) {
      tbl_proxy$remotetablestate$slice_map_ordered <- FALSE
    }
  }

  tbl_proxy
}

#' Apply a column transformation operation on the current table_proxy state
#'
#' Currently this is intended to be used with the j argument to a datatableinterface
#' object, but it could just as well be used to implement mutate or transmute for a
#' dplyr interface.
#'
#' @param tbl_proxy a table proxy object
#' @param j column expressions
#'
#' @return a table proxy object with the new state
#' @export
table_proxy_transform <- function(tbl_proxy, colexps) {

  # update nrow
  tbl_proxy$remotetablestate$ncol <- length(colexps)
  tbl_proxy$remotetablestate$colnames <- names(colexps)
  tbl_proxy$remotetablestate$colexps <- lapply(colexps, .resubstitute,
                                               sub = tbl_proxy$remotetablestate$colexps)
  tbl_proxy$remotetablestate$
    coltypes <- rtable_column_types(tbl_proxy$remotetable)[match(tbl_proxy$remotetablestate$colexps,
                                                                 rtable_colnames(tbl_proxy$remotetable))]
  tbl_proxy
}


# Like substitute, except it can substitute one expression into an expression stored in a variable
.resubstitute <- function(expr, sub) {
  eval(substitute(substitute(.expr, sub), list(.expr = expr)))
}


# This is just a temporary hack for printing the unevaluated virtual column expressions:
# Perhaps this is the point where the actual evaluating code would be called.
.add_virtual_cols <- function(tbl, rtable_state, on_disk, n_row) {

  if (all(on_disk)) {
    return(tbl)
  }

  virt_cols <- as.data.frame(lapply(rtable_state$colexps[which(!on_disk)], function(expr) {
    rep(deparse(expr), n_row)
  }))

  if (any(on_disk)) {
    tbl <- cbind(tbl, virt_cols)[, rtable_state$colnames, drop = FALSE]
  } else {
    tbl <- virt_cols[, rtable_state$colnames, drop = FALSE]
  }

  return(tbl)
}
