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


#' Proxy object to a fst file
#'
#' This method creates a proxy object to a _fst_ file. With this object and the accompanying
#' methods, it's possible to retrieve data from a _fst_ file using complex filters, selections
#' and orderings. The `fst_proxy` class serves as the interface between the `fst` package and
#' the `data.table` external interface. In the future, it can be re-used as the proxy between
#' `fst` and other interfaces (e.g. `fstplyr`, `SQL`)
#'
#' @param path Path to a _fst_ file
#' @param old_format Use `old_format = TRUE` when referencing a _fst_ file that was created
#' with a `fst`` package version lower than 0.8.0
#'
#' @md
#' @return An object of class `remote_table_fst`
remote_table_fst <- function(path, old_format = FALSE) {

  # get metadata from the fst file
  meta <- fst::metadata_fst(path, old_format)

  remote_table_fst_data <- list(
    meta = meta,
    old_format = old_format
  )

  # class attribute
  class(remote_table_fst_data) <- c("rtablefst")

  remote_table_fst_data
}


rtable_ncol.rtablefst <- function(remote_table_fst) {
  length(remote_table_fst$meta$columnBaseTypes)
}


rtable_nrow.rtablefst <- function(remote_table_fst) {
  remote_table_fst$meta$nrOfRows
}


rtable_colnames.rtablefst <- function(remote_table_fst) {
  remote_table_fst$meta$columnNames
}


rtable_read_range.rtablefst <- function(remote_table_fst, from_row, to_row, colnames = NULL) {
  fst::read_fst(remote_table_fst$meta$path, colnames, from_row, to_row,
    old_format = remote_table_fst$old_format)
}


rtable_read_full.rtablefst <- function(remote_table_fst, colnames = NULL) {
  fst::read_fst(remote_table_fst$meta$path, colnames, old_format = remote_table_fst$old_format)
}


rtable_column_types.rtablefst <- function(remote_table_fst, column_index = NULL) {

  # return type labels (for displaying purposes)
  types <- c("unknown", "chr", "fact", "ord fact", "int", "POSIXct", "difftime",
   "IDate", "ITime", "dbl", "Date", "POSIXct", "difftime", "ITime", "lgl", "int64",
   "nanotime", "raw")

  if (is.null(column_index)) {
    return(types[remote_table_fst$meta$columnTypes])
  }

  types[remote_table_fst$meta$columnTypes][column_index]
}
