

replace_dot_alias <- function(e) {
  # we don't just simply alias .=list because i) list is a primitive (faster to iterate) and ii) we test for use
  # of "list" in several places so it saves having to remember to write "." || "list" in those places
  if (is.call(e)) {
    if (e[[1L]] == ".") e[[1L]] <- quote(list)
    for (i in seq_along(e)[-1L]) if (!is.null(e[[i]])) e[[i]] <- replace_dot_alias(e[[i]])
  }
  e
}


indent <- function(indentation) {
  paste0(rep(" ", indentation), collapse = "")
}


call_parser <- function(jsub, parent_frame, table_columns, indentation = 0) {

  indentation <- indentation + 1
  print(paste0(indent(indentation), "method '", as.character(jsub[[1]]), "' is called:"))

  result <- list(rep(0, length(jsub)))
  result[[1]] <- as.character(jsub[[1]])

  # some arguments
  if (length(jsub) > 1) {

    # get result column names
    var_names <- names(jsub)
    col_count <- FALSE
    indentation <- indentation + 1

    for (pos in 2:length(jsub)) {

      if (is.call(jsub[[pos]])) {

        # call registration here
        call_res <- call_parser(jsub[[pos]], parent_frame, table_columns, indentation)
        result[[pos]] <- call_res[[1]]
        if (call_res[[2]]) col_count <- TRUE
      } else {
        is_symbol <- typeof(jsub[[pos]]) == "symbol"
        name <- as.character(jsub[[pos]])

        # search for column names first
        if (is_symbol && name %in% table_columns) {
          col_count <- TRUE
        }
        # add primitive to list of primitives

        print(paste0(indent(indentation), "argument '", var_names[pos], " = ", jsub[[pos]],
          "', exists in parent frame: ", exists(name, where = parent_frame),
          " is symbol: ", is_symbol,
          if (is_symbol) paste(" is column:", name %in% table_columns) else ""))

        result[[pos]] <- as.character(jsub[[pos]])
      }
    }
  }

  print(paste0(indent(indentation), "end method '", as.character(jsub[[1]]), "' (col_count: ", col_count))

  list(result, col_count)
}


parse_j <- function(j, table_columns) {
  jsub <- substitute(j)
  if (is.call(jsub)) {
    result <- call_parser(jsub, parent.frame(), table_columns)
    return(result)
  }
}
