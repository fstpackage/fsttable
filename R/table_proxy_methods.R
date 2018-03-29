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
#' @param from_row start row
#' @param to_row end row
#' @param colnames names of the columns that you want to retrieve
#'
#' @return a data.frame object with the requested data
#' @export
#' @md
table_proxy_read_range <- function(tbl_proxy, from_row, to_row, col_names = NULL) {

  # use the remotetablestate to get a subset of the data in memory
  rtable_state <- tbl_proxy$remotetablestate

  cols <- rtable_state$colnames

  if (!is.null(col_names)) {
    if (sum(!(col_names %in% cols) != 0)) {
      stop("Unknown columns requested")
    }

    cols <- col_names
  }

  # read only required columns and rows
  rtable <- tbl_proxy$remotetable

  rtable_read_range(rtable, from_row, to_row, cols)
}


table_proxy_read_full <- function(tbl_proxy, colnames = NULL) {

  # use the remotetablestate to get a subset of the data in memory
  rtable_state <- tbl_proxy$remotetablestate

  cols <- rtable_state$colnames

  if (!is.null(col_names)) {
    if (sum(!(col_names %in% cols) != 0)) {
      stop("Unknown columns requested")
    }

    cols <- col_names
  }

  # read only required columns and rows
  rtable <- tbl_proxy$remotetable

  rtable_read_full(rtable, cols)
}
