

replace_dot_alias <- function(e) {
  # we don't just simply alias .=list because i) list is a primitive (faster to iterate) and ii) we test for use
  # of "list" in several places so it saves having to remember to write "." || "list" in those places
  if (is.call(e)) {
    if (e[[1L]] == ".") e[[1L]] = quote(list)
    for (i in seq_along(e)[-1L]) if (!is.null(e[[i]])) e[[i]] = replace_dot_alias(e[[i]])
  }
  e
}


parse_j <- function(jsub) {
  av = NULL

  jsub = replace_dot_alias(jsub)
  root = if (is.call(jsub)) as.character(jsub[[1L]])[1L] else ""
  if (root == ":" ||
      (root %chin% c("-","!") && is.call(jsub[[2L]]) && jsub[[2L]][[1L]]=="(" && is.call(jsub[[2L]][[2L]]) && jsub[[2L]][[2L]][[1L]]==":") ||
      ( (!length(av<-all.vars(jsub)) || all(substring(av,1L,2L)=="..")) &&
        root %chin% c("","c","paste","paste0","-","!") &&
        missing(by) )) {   # test 763. TODO: likely that !missing(by) iff with==TRUE (so, with can be removed)
    # When no variable names (i.e. symbols) occur in j, scope doesn't matter because there are no symbols to find.
    # If variable names do occur, but they are all prefixed with .., then that means look up in calling scope.
    # Automatically set with=FALSE in this case so that DT[,1], DT[,2:3], DT[,"someCol"] and DT[,c("colB","colD")]
    # work as expected.  As before, a vector will never be returned, but a single column data.table
    # for type consistency with >1 cases. To return a single vector use DT[["someCol"]] or DT[[3]].
    # The root==":" is to allow DT[,colC:colH] even though that contains two variable names.
    # root == "-" or "!" is for tests 1504.11 and 1504.13 (a : with a ! or - modifier root)
    # We don't want to evaluate j at all in making this decision because i) evaluating could itself
    # increment some variable and not intended to be evaluated a 2nd time later on and ii) we don't
    # want decisions like this to depend on the data or vector lengths since that can introduce
    # inconistency reminiscent of drop=TRUE in [.data.frame that we seek to avoid.
    with=FALSE
    if (length(av)) {
      for (..name in av) {
        name = substring(..name, 3L)
        if (name=="") stop("The symbol .. is invalid. The .. prefix must be followed by at least one character.")
        if (!exists(name, where=parent.frame())) {
          stop("Variable '",name,"' is not found in calling scope. Looking in calling scope because you used the .. prefix.",
            if (exists(..name, where=parent.frame()))
              paste0(" Variable '..",name,"' does exist in calling scope though, so please just removed the .. prefix from that variable name in calling scope.")
              # We have recommended 'manual' .. prefix in the past, so try to be helpful
            else
              ""
          )
        } else if (exists(..name, where=parent.frame())) {
          warning("Both '",name,"' and '..", name, "' exist in calling scope. Please remove the '..", name,"' variable in calling scope for clarity.")
        }
      }
      ..syms = av
    }
  } else if (is.name(jsub)) {
    if (substring(jsub, 1L, 2L) == "..") stop("Internal error:  DT[, ..var] should be dealt with by the branch above now.")
    if (!with && !exists(as.character(jsub), where=parent.frame()))
      stop("Variable '",jsub,"' is not found in calling scope. Looking in calling scope because you set with=FALSE. Also, please use .. symbol prefix and remove with=FALSE.")
  }
  if (root=="{") {
    if (length(jsub) == 2L) {
      jsub = jsub[[2L]]  # to allow {} wrapping of := e.g. [,{`:=`(...)},] [#376]
      root = if (is.call(jsub)) as.character(jsub[[1L]])[1L] else ""
    } else if (length(jsub) > 2L && is.call(jsub[[2L]]) && jsub[[2L]][[1L]] == ":=") {
      #2142 -- j can be {} and have length 1
      stop("You have wrapped := with {} which is ok but then := must be the only thing inside {}. You have something else inside {} as well. Consider placing the {} on the RHS of := instead; e.g. DT[,someCol:={tmpVar1<-...;tmpVar2<-...;tmpVar1*tmpVar2}")
    }
  }
  if (root=="eval" && !any(all.vars(jsub[[2L]]) %chin% names(x))) {
    # TODO: this && !any depends on data. Can we remove it?
    # Grab the dynamic expression from calling scope now to give the optimizer a chance to optimize it
    # Only when top level is eval call.  Not nested like x:=eval(...) or `:=`(x=eval(...), y=eval(...))
    jsub = eval(jsub[[2L]], parent.frame(), parent.frame())  # this evals the symbol to return the dynamic expression
    if (is.expression(jsub)) jsub = jsub[[1L]]    # if expression, convert it to call
    # Note that the dynamic expression could now be := (new in v1.9.7)
    root = if (is.call(jsub)) as.character(jsub[[1L]])[1L] else ""
  }
  if (root == ":=") {
    allow.cartesian=TRUE   # (see #800)
    if (!missing(i) && !missing(keyby))
      stop(":= with keyby is only possible when i is not supplied since you can't setkey on a subset of rows. Either change keyby to by or remove i")
    if (!missingnomatch) {
      warning("nomatch isn't relevant together with :=, ignoring nomatch")
      nomatch=0L
    }
  }

  res <- jsub

  res
}

