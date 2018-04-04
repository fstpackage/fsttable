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


#' @export
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
#' @param colnames names of the columns that you want to retrieve
#'
#' @return a data.frame object with the requested data
#' @export
#' @md
table_proxy_read_range <- function(tbl_proxy, from_row, to_row, col_names = NULL) {

  rtable <- tbl_proxy$remotetable
  rtable_state <- tbl_proxy$remotetablestate

  # determine columns to read
  cols <- rtable_state$colnames

  if (!is.null(col_names)) {
    if (sum(!(col_names %in% cols) != 0)) {
      stop("Unknown columns requested")
    }

    cols <- col_names
  }

  # determine rows to read
  slice_map <- rtable_state$slice_map
  
  # no previous slice map, use arguments for row subset
  if (is.null(slice_map)) {
    return(rtable_read_range(rtable, from_row, to_row, cols))
  }

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
  cols <- rtable_state$colnames

  if (!is.null(col_names)) {
    if (sum(!(col_names %in% cols) != 0)) {
      stop("Unknown columns requested")
    }

    cols <- col_names
  }

  # determine rows to read
  slice_map <- rtable_state$slice_map

  # read all rows  
  if (is.null(slice_map)) {
    return(rtable_read_full(rtable, cols))
  }

  # order slice map and read row subset
  min_row <- min(slice_map)
  max_row <- max(slice_map)

  # very inefficient partial read
  # TODO: create a read mask
  row_range <- rtable_read_range(rtable, min_row, max_row, cols)

  # read contiguous extent of selected rows, then filter & order
  row_range[1 + slice_map - min_row, , drop = FALSE]
}



#' Apply a row-selection operation on the current table_proxy state
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

  # set equal to selected slice map
  if (is.null(slice_map)) {
    tbl_proxy$remotetablestate$slice_map <- i
    tbl_proxy$remotetablestate$nrow <- length(i)
    return(tbl_proxy)
  }

  # slice the current clice map  
  tbl_proxy$remotetablestate$slice_map <- slice_map[i]

  # update nrow
  tbl_proxy$remotetablestate$nrow <- length(i)

  tbl_proxy
}
