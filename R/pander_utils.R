
table_x <- function (..., exclude = if (useNA == "no") c(NA, NaN), useNA = c("no", 
                                                                  "ifany", "always"), dnn = list.names(...), deparse.level = 1) {
  list.names <- function(...) {
    l <- as.list(substitute(list(...)))[-1L]
    nm <- as.character(l) #names(l)
    fixup <- if (is.null(nm)) 
      seq_along(l)
    else nm == ""
    dep <- vapply(l[fixup], function(x) switch(deparse.level + 
                                                 1, "", if (is.symbol(x)) as.character(x) else "", 
                                               deparse(x, nlines = 1)[1L]), "")
    if (is.null(nm)) 
      dep
    else {
      nm[fixup] <- dep
      nm
    }
  }
  if (!missing(exclude) && is.null(exclude)) 
    useNA <- "always"
  useNA <- match.arg(useNA)
  args <- list(...)
  if (!length(args)) 
    stop("nothing to tabulate")
  if (length(args) == 1L && is.list(args[[1L]])) {
    args <- args[[1L]]
    if (length(dnn) != length(args)) 
      dnn <- if (!is.null(argn <- names(args))) 
        argn
    else paste(dnn[1L], seq_along(args), sep = ".")
  }
  bin <- 0L
  lens <- NULL
  dims <- integer()
  pd <- 1L
  dn <- NULL
  for (a in args) {
    if (is.null(lens)) 
      lens <- length(a)
    else if (length(a) != lens) 
      stop("all arguments must have the same length")
    cat <- if (is.factor(a)) {
      if (any(is.na(levels(a)))) 
        a
      else {
        if (is.null(exclude) && useNA != "no") 
          addNA(a, ifany = (useNA == "ifany"))
        else {
          if (useNA != "no") 
            a <- addNA(a, ifany = (useNA == "ifany"))
          ll <- levels(a)
          a <- factor(a, levels = ll[!(ll %in% exclude)], 
                      exclude = if (useNA == "no") 
                        NA)
        }
      }
    }
    else {
      a <- factor(a, exclude = exclude)
      if (useNA != "no") 
        addNA(a, ifany = (useNA == "ifany"))
      else a
    }
    nl <- length(ll <- levels(cat))
    dims <- c(dims, nl)
    if (prod(dims) > .Machine$integer.max) 
      stop("attempt to make a table with >= 2^31 elements")
    dn <- c(dn, list(ll))
    bin <- bin + pd * (as.integer(cat) - 1L)
    pd <- pd * nl
  }
  names(dn) <- dnn
  bin <- bin[!is.na(bin)]
  if (length(bin)) 
    bin <- bin + 1L
  y <- array(tabulate(bin, pd), dims, dimnames = dn)
  class(y) <- "table"
  y
}

#' @noRd
#' @param x ftable object
#' @param ... optional parameters passed to raw pandoc.table function
#t1 = table(iris$Species, iris$test)
#pander:::pander.ftable(t1)
#pander.ftable_x(t1, ez = iris)
#debug(pander.ftable_x)
#debug(setValueLabels)
pander.ftable_x <- function(x, ez = NULL, ...) {

  getVarName <- function(s) {
    s1 = unlist(strsplit(s, split = '$', fixed=T))
    if (is.na(s1[2])) s else s1[2]
  }
  # return a named list of character vectors, 
  # name = var labels, value = value labels
  getmeta<-function(x) {
    nm = sapply(names(x), getVarName)
    names(x) = nm
    ret = lapply(seq_along(x), getMetaValueLabels, x, dt = ez)
    names(ret) = getVarLabels(ez, nm)
    ret
  }
  
  if (!is.null(ez) & inherits(ez, 'ez.data.frame')) {
    t1 = x
    a1 = attr(t1, 'row.vars')
    attr(t1, 'row.vars')<- getmeta(a1)  #lapply(seq_along(a1), setValueLabels, a1, dt = ez)
    a1 = attr(t1, 'col.vars')
    attr(t1, 'col.vars')<- getmeta(a1)  #lapply(seq_along(a1), setValueLabels, a1, dt = ez)
    
  }
  pander.ftable_y = get('pander.ftable_y', ez_globals)
  pander.ftable_y(t1, ...)
}

pander.summary.lm_x <- function(x, caption = attr(x, 'caption'), covariate.labels,
                  omit, summary = TRUE, add.significance.stars = FALSE, ...){

  if (missing(covariate.labels) ) {
    model_data = as.character(x$call)[3]
    model_data = if (is.na(model_data)) model_data else get(model_data)
    if (inherits(model_data, 'ez.data.frame')) {
      #meta = attr(model_data, 'meta')
      covariate.labels = attr(model_data, 'meta')
    }
  }
  #  covariate.labels may be a character vector or data.frame ( meta from ez.data.frame or provided as is)
  if (!is.null(covariate.labels)) {
    if (!is.null(dim(covariate.labels))) {
      #covariate.labels = match_var_labels(rownames(x$coefficients), covariate.labels)[[2]]
      covariate.labels = getVarLabelsFromMeta(covariate.labels, rownames(x$coefficients))
    }
  }
  pander.summary.lm_y = get('pander.summary.lm_y', ez_globals)
  pander.summary.lm_y(x, caption, covariate.labels, omit, summary, add.significance.stars, ...)
}

#' @noRd
init_hooks <- function(){
  if (!exists('pander.summary.lm_y', ez_globals)) {
    assign("pander.summary.lm_y", pander:::pander.summary.lm, ez_globals)
    set_hook('pander', 'pander.summary.lm', pander.summary.lm_x)
  }

  if (!exists('pander.ftable_y', ez_globals)) {
    assign("pander.ftable_y", pander:::pander.ftable, ez_globals)
    set_hook('pander', 'pander.ftable', pander.ftable_x)
  }
  set_hook('base', 'table', table_x)
}

#' @noRd
set_hook <- function (pkgname, func, newfunc) {
  ns = asNamespace(pkgname)
  unlockBinding(func, ns)
  assign(func, newfunc, ns)
  lockBinding(func, ns)
}


