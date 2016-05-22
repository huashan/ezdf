#
# table utilities
#

#' multivariate table
#' 
#' @param DT ez.data.frame object
#' @param expr formula object
#' @param func statistics applied on the data subset 
#' @examples 
#' tbl(dat, a66 ~ s5a)
#' tbl(dat, a66 ~ s5a + s41, 'sum')
#' tbl(dat, a66 ~ s5a + s41, 'mean', N = T)
#' pander(tbl(dat, ~ s41 + s5a + a66, 'sum', sort = T))
#' tbl(dat, ~ s5a + a66)
tbl<- function(ez, expr, func = 'mean', N = FALSE, sort = TRUE, ...){
  UseMethod("tbl", ez)
}

tbl.ez.data.frame<-function(ez, expr, func = 'mean', N = FALSE, sort = TRUE, ...){
  tt = terms(expr)
  if (length(tt) == 2) {
    # x only, output frequency
    x = as.character(attr(tt, 'variables'))
    if (length(x) > 1) x = x[-1]
    y = NULL
  }
  if (length(tt) == 3) {
    y = as.character(attr(tt, 'variables')[2])
    if (length(y) > 1) y = y[-1]
    x = as.character(attr(tt, 'variables')[-c(1:2)])
  }
  
  dots = list(...)  
  dotsArg = if (length(dots) > 0) paste0(', ', paste(names(dots), dots, sep='=', collapse = ',')) else ''
  expr = parse(text=paste0('lapply(.SD, ', func, dotsArg, ')'))
  
  grp = paste0(x, collapse = ',')
  if (is.null(y)) {
    dat = ez[, .N, by = grp]
    warning(sprintf("No Y variables in expr, output frequencies. Function '%s' is omitted", func))
  } else {
    vars = y
    
    #todo：替换掉 “, ”?
    isMultiGrp = grepl(',', grp) # 分组变量有可能多个
    
    if (isMultiGrp){
      dat = ez[, eval(expr), by = grp, .SDcols = vars]
    }else{
      #dat = dt[!is.na(eval(parse(text=grp))), eval(expr), by = eval(parse(text = sprintf('list(%s)', x))), .SDcols = vars]
      dat = ez[!is.na(eval(parse(text=grp))), eval(expr), by = eval(parse(text =grp)), .SDcols = vars]
      if (names(dat)[[1L]] == 'parse') setnames(dat, 1L, grp)
    }
    
    if (N) {
      if (isMultiGrp){
        dt.n = ez[, .N, by = grp]
      } else {
        dt.n = ez[!is.na(eval(grp)), .N, by = eval(grp)]
      }
      dat[, N:=dt.n$N]
    }
  }
  
  if (sort) setkeyv(dat, x)
  
  setVarLabels<-function() {
    meta = attr(ez, 'meta')
    if (!is.null(meta)) {
      meta = meta[nzchar(meta[, 2, with = F]), ]
      ns = names(dat)
      #ns = ns[ns %in% meta[,1]]
      #ret = meta[match(ns, meta[, 1, with = F], nomatch = 0), ]
      ret = meta[ns, nomatch=0]
      keepVarName = getOptKeepVarname()
      if (keepVarName) {
        ret[[2]] = paste(ret[[1]], ret[[2]], sep = '\t')
      }
      setnames(dat, ret[[1]], ret[[2]])
    }
  }
  # 当 by 只有一个变量时，dat 的字段才会保留 `labels` 属性，否则将不带 labels 属性
  setValueLabels <- function(x, lbl) {
    #lbl = attr(x, 'labels')
    if (!is.null(lbl)) {
      fetchValueLabels(x, lbl, withValue = getOptKeepVal())
    } else {
      x
    }
  }
  # set value labels for grouping variables
  # 如果x是factor，则无需再设label
  x = x[sapply(ez[, x, with = F], is.numeric)]
  lbls = lapply(ez[, x, with = F], attr, which = 'labels')
  #dat[, `:=`(x, lapply(.SD, setValueLabels)), .SDcols = x, with = F]
  dat[, `:=`(x, mapply(setValueLabels, .SD, lbls, SIMPLIFY = F)), .SDcols = x, with = F]
  
  # set var labels
  setVarLabels()
  class(dat) = setdiff(class(dat), 'ez.data.frame')
  dat
}

#' flat table using ftable()
#' 
#' @param style 1=frequency, 2=percentage, 3=percentage with Sum
#' @param ... parameters for ftable.formula(), must have both left and right hand sides.
#debug(ftable.ez.data.frame)
ftable.ez.data.frame <- function(ez, formula, style = 1, prop_margin = 1, ...) {
  setValueLabels <- function(idx, ftbl.attr) {
    lbl = attr(ez[[names(ftbl.attr)[idx]]], 'labels')
    if (!is.null(lbl)) {
      fetchValueLabels(ftbl.attr[[idx]], lbl, withValue = getOptKeepVal())
    } else {
      ftbl.attr[[idx]]
    }
  }
  t1 = stats:::ftable.formula(formula, data = ez, ...)
  a1 = attr(t1, 'row.vars')
  attr(t1, 'row.vars')<- lapply(seq_along(a1), setValueLabels, a1)
  a1 = attr(t1, 'col.vars')
  attr(t1, 'col.vars')<- lapply(seq_along(a1), setValueLabels, a1)
  #return(t1)
  # style = 1, frequency
  # style = 2, percentage
  # style = 3, percentage + row_sum(N)
  switch(as.character(style),
         '1' = as.matrix(t1),
         '2' = as.matrix(prop.table(t1, prop_margin)),
         '3' = cbind(as.matrix(prop.table(t1, prop_margin)), N=apply(t1, 1, sum)),
         warning('style value not valid'))
}

ctbl<- function(ez, expr){
  UseMethod("ctbl", ez)
}

ctbl.ez.data.frame<-function(ez, expr){
  tt = terms(expr)
  if (length(tt) == 2) {
    # x only, output frequency
    x = as.character(attr(tt, 'variables'))
    if (length(x) > 1) x = x[-1]
    y = NULL
  }
  if (length(tt) == 3) {
    y = as.character(attr(tt, 'variables')[2])
    if (length(y) > 1) y = y[-1]
    x = as.character(attr(tt, 'variables')[-c(1:2)])
  }
  
  #expr = parse(text=paste0('lapply(.SD, ', func, ', na.rm=T)'))
  vars = c(y, x)
  table(ez[, vars, with=F])
}
