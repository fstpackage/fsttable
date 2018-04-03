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



#' A proxy for a remote table
#'
#' This method defines a `table_proxy` object that encapsulates a `remote_table`.
#' A `table_proxy` is a virtual construct that emulates a full table. The actual
#' column data is kept in the physical table as long as possible to keep interaction
#' with the `table_proxy` as fast as possible. Operations on a `table_proxy` object
#' yield a new `table_proxy` object with (possibly) modified meta-data to emulate the
#' result.
#'
#'
#' @param remote_table A object with a custom class. That class should implement the generics
#' required for a data.table proxy
#'
#' @return A `table_proxy` object that can be used a a virtual table.
#' @export
#' @md
table_proxy <- function(remote_table) {

  # get column names from proxy object
  proxy_colnames <- rtable_colnames(remote_table)

  # table proxy state
  remote_table_state <- list(
    colnames        = proxy_colnames,
    coltypes        = rtable_column_types(remote_table),
    nrow            = rtable_nrow(remote_table),
    ncol            = length(proxy_colnames),
    slice_map       = NULL,  # all rows initialy selected
    slice_map_order = NULL   # order of row index
  )

  .table_proxy(remote_table, remote_table_state)
}


# create a remote_table object and encapsulate a remote_table_state
.table_proxy <- function(remote_table, remote_table_state) {

  # create a table_proxy object
  tproxy <- list(
    remotetable = remote_table,
    remotetablestate = remote_table_state
  )

  # class attribute
  class(tproxy) <- c("tableproxy")

  tproxy
}
