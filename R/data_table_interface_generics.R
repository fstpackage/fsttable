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
row.names.datatableinterface <- function(x) {
  tproxy <- .table_proxy(x)
  as.character(seq_len(table_proxy_nrow(tproxy)))
}


#' @export
dim.datatableinterface <- function(x) {
  tproxy <- .table_proxy(x)
  c(rproxy_state$nrow, table_proxy_nrow(tproxy))
}


#' @export
dimnames.datatableinterface <- function(x) {
  tproxy <- .table_proxy(x)
  list(as.character(seq_len(table_proxy_nrow(tproxy))), table_proxy_colnames(tproxy))
}


#' @export
names.datatableinterface <- function(x) {
  tproxy <- .table_proxy(x)
  table_proxy_colnames(tproxy)
}


#' @export
`[[.datatableinterface` <- function(x, j, exact = TRUE) {
  if (!exact) {
    warning("exact ignored", call. = FALSE)
  }

  tproxy <- .table_proxy(x)

  if (length(j) != 1) {

    # select at least one column number
    if (length(j) == 0) {
      stop("Please use a length one integer or character vector to specify the column", call. = FALSE)
    }

    # recursive indexing (up to level 1)

    if (length(j) != 2) {
      stop("Recursive indexing is currently only supported up to level 1.", call. = FALSE)
    }

    if (is.character(j)) {
      stop("Subscript out of bounds.", call. = FALSE)
    }

    # check row number

    if (j[2] < 1 || j[2] > table_proxy_nrow(tproxy)) {
      stop("Second index out of bounds.", call. = FALSE)
    }

    col_name <- table_proxy_colnames(tproxy)[as.integer(j[1])]

    # remote_proxy <- .remote_proxy(x)
    # 
    # return(rproxy_read_range(remote_proxy, from = j[2], to = j[2],  colnames = col_name))
  }

  if (!(is.numeric(j) || is.character(j))) {
    stop("Please use a length 1 integer of character vector to specify the column", call. = FALSE)
  }

  # length one integer column selection
  if (is.numeric(j)) {
    if (j < 1 || j > table_proxy_ncol(tproxy)) {
      stop("Invalid column index ", j, call. = FALSE)
    }

    j <- table_proxy_colnames(tproxy)[as.integer(j)]
  } else {
    if (!(j %in% table_proxy_colnames(tproxy))) {
      return(NULL)
    }
  }

  # determine row selection here from metadata

  # rproxy_read_full(remote_proxy, j)
}


# override needed to avoid the [[ operator messing up the str output

#' @export
str.datatableinterface <- function(object, ...) {
  str(unclass(object))
}


#' @export
`$.datatableinterface` <- function(x, j) {
  x[[j]]
}


#' @export
print.datatableinterface <- function(x, number_of_rows = 50, ...) {

  tproxy <- .table_proxy(x)

  nr_of_rows <- table_proxy_nrow(tproxy)
  nr_of_cols <- table_proxy_ncol(tproxy)
  col_names <- table_proxy_colnames(tproxy)
  
  # perhaps custom info here as header of output

  cat("<fst file>\n")
  cat(nr_of_rows, " rows, ", nr_of_cols, " columns\n\n", sep = "")

  if (!is.numeric(number_of_rows)) number_of_rows <- 100L
  if (!is.infinite(number_of_rows)) number_of_rows <- as.integer(number_of_rows)
  if (number_of_rows <= 0L) return(invisible())   # ability to turn off printing

  table_splitted <- (nr_of_rows > number_of_rows) && (nr_of_rows > 10)

  if (table_splitted) {
    sample_data_head <- rproxy_read_range(remote_proxy, 1, 5)
    sample_data_tail <- rproxy_read_range(remote_proxy, rproxy_state$nrow - 4, rproxy_state$nrow)
    sample_data <- rbind.data.frame(sample_data_head, sample_data_tail)
  } else {
    sample_data <- rproxy_read_full(remote_proxy)
  }

  # use color in terminal output
  color_on <- TRUE

  if (!"crayon" %in% loadedNamespaces()) {
    if (!requireNamespace("crayon", quietly = TRUE)) {
      color_on <- FALSE
    } else {
      if (!crayon::has_color()) {
        color_on <- FALSE
      }
    }
  }

  type_row <- matrix(paste("<", rproxy_column_types(remote_proxy), ">", sep = ""), nrow = 1)
  colnames(type_row) <- col_names

  # convert to aligned character columns
  sample_data_print <- format(sample_data)

  if (table_splitted) {
    dot_row <- matrix(rep("--", nr_of_cols), nrow = 1)
    colnames(dot_row) <- col_names

    sample_data_print <- rbind(
      type_row,
      sample_data_print[1:5, , drop = FALSE],
      dot_row,
      sample_data_print[6:10, , drop = FALSE])

    rownames(sample_data_print) <- c(" ", 1:5, "--", (nr_of_rows - 4):nr_of_rows)

    y <- capture.output(print(sample_data_print))

    # the length of y must be a multiple of 13 and color must be permitted
    # if not, take defensive action; skip coloring and solve later
    if (!color_on || (length(y) %% 13 != 0)) {
      print(sample_data_print)
      return(invisible(x))
    }

    type_rows <- seq(2, length(y), 13)
    gray_rows <- seq(8, length(y), 13)

    y[gray_rows] <- paste0("\033[38;5;248m", y[gray_rows], "\033[39m")

    gray_rows <- c(type_rows, gray_rows)
  } else {
    # table is not splitted along the row axis
    sample_data_print <- rbind(
      type_row,
      sample_data_print)

    rownames(sample_data_print) <- c(" ", 1:nr_of_rows)

    # no color terminal available
    if (!color_on) {
      print(sample_data_print)
      return(invisible(x))
    }

    y <- capture.output(print(sample_data_print))

    gray_rows <- type_rows <- seq(2, length(y), 2 + nr_of_rows)
  }

  # type rows are set to italic light grey
  y[type_rows] <- paste0("\033[3m\033[38;5;248m", y[type_rows], "\033[39m\033[23m")

  # use light grey color up to width of row name column
  row_text_size <- regexpr("^[0-9-]*", tail(y, 1))
  row_text_size <- attr(row_text_size, "match.length")

  row_text <- substr(y[-gray_rows], 1, row_text_size)

  y[-gray_rows] <- paste0("\033[38;5;248m", substr(y[-gray_rows], 1, row_text_size),
    "\033[39m", substr(y[-gray_rows], row_text_size + 1, nchar(y[-gray_rows])))

  cat(y, sep = "\n")
  return(invisible(x))
}


#' @export
as.data.frame.datatableinterface <- function(x, row.names = NULL, optional = FALSE, ...) {

  remote_proxy <- .remote_proxy(x)

  as.data.frame(rproxy_read_full(remote_proxy), row.names, optional, ...)
}


#' @export
as.list.datatableinterface <- function(x, ...) {
  as.list(as.data.frame(x))
}
