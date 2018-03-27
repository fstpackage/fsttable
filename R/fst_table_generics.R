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
row.names.fsttable <- function(x) {
  fstproxy <- .get_fstproxy(x)
  as.character(seq_len(fp_nrow(fstproxy)))
}


#' @export
dim.fsttable <- function(x) {
  fstproxy <- .get_fstproxy(x)
  c(fp_nrow(fstproxy), fp_nrow(fstproxy))
}


#' @export
dimnames.fsttable <- function(x) {
  fstproxy <- .get_fstproxy(x)
  list(as.character(seq_len(fp_nrow(fstproxy))), fp_colnames(fstproxy))
}


#' @export
names.fsttable <- function(x) {
  fstproxy <- .get_fstproxy(x)
  fp_colnames(fstproxy)
}


#' @export
`[[.fsttable` <- function(x, j, exact = TRUE) {
  if (!exact) {
    warning("exact ignored", call. = FALSE)
  }

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

    if (j[2] < 1 || j[2] > fp_nrow(fstproxy)) {
      stop("Second index out of bounds.", call. = FALSE)
    }

    col_name <- fp_colnames(fstproxy)[as.integer(j[1])]

    return(fp_read_range(fstproxy, from = j[2], to = j[2],  colnames = col_name))
  }

  if (!(is.numeric(j) || is.character(j))) {
    stop("Please use a length 1 integer of character vector to specify the column", call. = FALSE)
  }

  # length one integer column selection
  if (is.numeric(j)) {
    if (j < 1 || j > length(fp_colnames(fstproxy))) {
      stop("Invalid column index ", j, call. = FALSE)
    }

    j <- fp_colnames(fstproxy)[as.integer(j)]
  } else {
    if (!(j %in% fp_colnames(fstproxy))) {
      return(NULL)
    }
  }

  # determine row selection here from metadata

  fp_read_full(fstproxy, j)
}


# override needed to avoid the [[ operator messing up the str output

#' @export
str.fsttable <- function(object, ...) {
  str(unclass(object))
}


#' @export
`$.fsttable` <- function(x, j) {
  x[[j]]
}


require_bit64 <- function() {
  # called in print when they see integer64 columns are present
  if (!requireNamespace("bit64", quietly = TRUE))
    warning(paste0("Some columns are type 'integer64' but package bit64 is not installed. ",
      "Those columns will print as strange looking floating point data. ",
      "There is no need to reload the data. Simply install.packages('bit64') to obtain ",
      "the integer64 print method and print the data again."))
}


require_data_table <- function() {
  # called in print when they see ITime columns are present
  if (!requireNamespace("data.table", quietly = TRUE))
    warning(paste0("Some columns are type 'ITime' but package data.table is not installed. ",
                   "Those columns will print incorrectly. There is no need to ",
                   "reload the data. Simply install.packages('data.table') to obtain the data.table print ",
                   "method and print the data again."))
}

require_nanotime <- function() {
  # called in print when they see nanotime columns are present
  if (!requireNamespace("nanotime", quietly = TRUE))
    warning(paste0("Some columns are type 'nanotime' but package nanotime is not installed. ",
      "Those columns will print as strange looking floating point data. There is no need to ",
      "reload the data. Simply install.packages('nanotime') to obtain the nanotime print ",
      "method and print the data again."))
}

#' @export
print.fsttable <- function(x, number_of_rows = 50, ...) {
  
  fstproxy <- .get_fstproxy(x)
  
  cat("<fst file>\n")
  cat(fp_nrow(fstproxy), " rows, ", length(fp_colnames(fstproxy)),
      " columns (", basename(fp_path(fstproxy)), ")\n\n", sep = "")

  if (!is.numeric(number_of_rows)) number_of_rows <- 100L
  if (!is.infinite(number_of_rows)) number_of_rows <- as.integer(number_of_rows)
  if (number_of_rows <= 0L) return(invisible())   # ability to turn off printing

  table_splitted <- (fp_nrow(fstproxy) > number_of_rows) && (fp_nrow(fstproxy) > 10)

  if (table_splitted) {
    sample_data_head <- fp_read_range(fstproxy, 1, 5)
    sample_data_tail <- fp_read_range(fstproxy, fp_nrow(fstproxy) - 4, fp_nrow(fstproxy))
    sample_data <- rbind.data.frame(sample_data_head, sample_data_tail)
  } else {
    sample_data <- fp_read_full(fstproxy)
  }

  # use bit64 package if available for correct printing
  if ( (!"bit64"      %in% loadedNamespaces()) && any(sapply(sample_data, inherits, "integer64" ))) require_bit64()
  if ( (!"nanotime"   %in% loadedNamespaces()) && any(sapply(sample_data, inherits, "nanotime"  ))) require_nanotime()
  if ( (!"data.table" %in% loadedNamespaces()) && any(sapply(sample_data, inherits, "ITime"))) require_data_table()

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

  type_row <- matrix(paste("<", fp_column_types(fstproxy), ">", sep = ""), nrow = 1)
  colnames(type_row) <- fp_colnames(fstproxy)

  # convert to aligned character columns
  sample_data_print <- format(sample_data)

  if (table_splitted) {
    dot_row <- matrix(rep("--", length(fp_colnames(fstproxy))), nrow = 1)
    colnames(dot_row) <- fp_colnames(fstproxy)

    sample_data_print <- rbind(
      type_row,
      sample_data_print[1:5, , drop = FALSE],
      dot_row,
      sample_data_print[6:10, , drop = FALSE])

    rownames(sample_data_print) <- c(" ", 1:5, "--", (fp_nrow(fstproxy) - 4):fp_nrow(fstproxy))

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

    rownames(sample_data_print) <- c(" ", 1:fp_nrow(fstproxy))

    # no color terminal available
    if (!color_on) {
      print(sample_data_print)
      return(invisible(x))
    }

    y <- capture.output(print(sample_data_print))

    gray_rows <- type_rows <- seq(2, length(y), 2 + fp_nrow(fstproxy))
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
as.data.frame.fsttable <- function(x, row.names = NULL, optional = FALSE, ...) {
  
  fstproxy <- .get_fstproxy(x)

  as.data.frame(fp_read_full(fstproxy), row.names, optional, ...)
}


#' @export
as.list.fsttable <- function(x, ...) {
  as.list(as.data.frame(x))
}
