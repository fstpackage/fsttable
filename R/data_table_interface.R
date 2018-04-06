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



#' Create a `data.table` interface around a proxy object
#'
#' This method defines a `data.table` interface around a specific object. That object should
#' define all the generics needed for supporting the interface.
#'
#' @param table_proxy Object generated with method `table_proxy()` that provides an interface
#' to an encapsulated remote table.
#'
#' @return A `datatableinterface` object that exposes a `data.table` interface to the encapsulated
#' `table_proxy` object.
#' @export
#' @md
data_table_interface <- function(table_proxy) {

  # get column names from proxy object
  proxy_colnames <- table_proxy_colnames(table_proxy)

  # maximum number of autocomplete columns
  nr_of_display_cols <- min(length(proxy_colnames), 50)

  dt <- data.table::as.data.table(matrix(rep(0, 1 + nr_of_display_cols), nrow = 1))
  data.table::setnames(dt, c(".table_proxy", proxy_colnames[1:nr_of_display_cols]))

  # store remote proxy object
  dt[, .table_proxy := list(list(table_proxy))]

  # class attribute
  class(dt) <- c("datatableinterface", "data.table", "data.frame")

  dt
}


.get_table_proxy <- function(x) {
  .subset2(x, ".table_proxy")[[1]]
}


#' a data.table interface
#'
#' @param x a data.table interface object
#'
#' @param i data.table's 'i' argument
#' @param j data.table's 'j' argument
#' @param by not implemented at this point
#' @param keyby not implemented at this point 
#' @param with not implemented at this point
#' @param nomatch not implemented at this point
#' @param mult not implemented at this point
#' @param roll not implemented at this point
#' @param rollends not implemented at this point
#' @param which not implemented at this point
#' @param .SDcols not implemented at this point
#' @param verbose If TRUE, status and information messages are returned to the console.
#' @param collect If TRUE, the data.table interface operation will return a data.table
#' to memory
#' @param allow.cartesian not implemented at this point
#' @param drop not implemented at this point
#' @param on not implemented at this point
#'
#' @export
`[.datatableinterface` <- function(x, i, j, by, keyby, with, nomatch, mult, roll, rollends,
  which, .SDcols, verbose = FALSE, collect = FALSE, allow.cartesian, drop, on) {

  if (!missing(on) | !missing(drop) | !missing(allow.cartesian) | !missing(.SDcols) |
      !missing(which) | !missing(rollends) | !missing(roll) | !missing(mult) |
      !missing(nomatch) | !missing(by) | !missing(keyby) | !missing(with)) {
    stop("At this point only i and j arguments are implemented")
  }

  dt_interface <- .datatable_interface(x, i, j, verbose)

  # return an interface object
  if (!collect) return(dt_interface)

  # load the data.table into memory
  tbl_proxy <- .get_table_proxy(dt_interface)
  dt <- table_proxy_read_full(tbl_proxy)
  setDT(dt)

  dt
}


.datatable_interface <- function(x, i, j, verbose) {

  tbl_proxy <- .get_table_proxy(x)

  if (verbose) print(paste("number of arguments to []:", nargs()))

  # Scenario 1: i and j are missing
  # In this case we just copy the proxy object and return a new datatableinterface object
  if (missing(i) && missing(j)) {

    if (verbose) print("i and j missing")

    return(data_table_interface(tbl_proxy))
  }

  # the case ft[c(1, 10, 5), A := 3 * B] is not supported yet.
  # At this point row selection is always performed before column selection
  # The following holds:
  #
  # ft[1:10, .(ColB)] is equivalent to ft[1:10][, .(ColB)]
  #
  # Therefore we can treat the call [i, j] as [i][, j]

  if (!missing(i)) {
    if (verbose) print("i used")

    if (is.logical(i)) {

      if (nrow(x) != length(i)) {
        stop(paste("i evaluates to a logical vector of length", length(i),
          "but there are", nrow(x) , "rows.",
          "Recycling of logical i is not allowed with data.table's."))
      }

      # select rows with a logical row index
      tbl_proxy <- table_proxy_select_row_mask(tbl_proxy, i)

      # return a copy of the interface with new table proxy
      return(data_table_interface(tbl_proxy))
    }

    # double's are converted to integers
    if (is.double(i)) {
      i <- as.integer(i)
    }

    # at this point, only integers are allowed
    if (!is.integer(i)) {
      stop(paste("i could not be evaulated as an integer or logical vector."))
    }

    # select rows with an integer row index
    tbl_proxy <- table_proxy_select_rows(tbl_proxy, i)

    # return a copy of the interface with new table proxy
    return(data_table_interface(tbl_proxy))
  }

  if (!missing(j)) {
    if (verbose) print("j used")

    jsub <- parse_j(substitute(j))

    # return full table, implement later
    return(data_table_interface(tbl_proxy))
  }
}
