#
#
#

#' Import Stata .dta file using package haven
#' 
#' @param file dta file name.
#' @param encoding Character encoding for labels, default is UTF-8.
#' @return ez.data.frame class object inherited from data.table
#' @export 
#' @family Stata dta
#' @seealso readSPSS
#' @examples 
#' setwd('E:/Huashan/Stat/R/Lectures/北大新传/data')
#' library(ezdf)
#' dat = readStata('CGSS2013（居民问卷） 发布版.dta')
#' class(dat)
#' tbl(dat, a66 ~ s5a)
readStata <- function(file, encoding) {
  require(haven)
  
  dt = read_dta(file)
  # 修正数据出现  “Error: `x` and `labels` must be same type”错误的临时解决办法：
  dt = as.data.table(lapply(dt, unclass), stringsAsFactors = F)

  lbl = sapply(dt, attr, 'label')
  
  setValueLabels <- function(col, encoding) {
    llbl = attr(col, 'labels'); 
    if (!is.null(llbl)) {
      ns=names(llbl);
      Encoding(ns) = encoding;
      names(llbl) = ns;
      setattr(col, 'labels', llbl);
    }
    invisible()
  }

  if (!missing(encoding)) {
    Encoding(lbl) = encoding
    # use data.table:::setattr() in case package bit is loaded
    invisible(mapply(data.table:::setattr, dt, lbl, name = 'label', SIMPLIFY = F))
    lapply(dt, setValueLabels, encoding = encoding)  
  }
  
  meta = data.frame(var = names(dt), lbl = lbl, stringsAsFactors = F)
  as.ez(dt, meta)
}

#' Import SPSS .sav file using package haven or foreign
#' 
#' @param file .sav file name.
#' @param lib library used to import SPSS sav data. Either 'foreign' or 'haven'.
#' @export
#' @return ez.data.frame class object inherited from data.table
#' @family SPSS
#' @seealso readStata
readSPSS<-function(file, lib='foreign', ...) {
  require(lib, character.only = TRUE)
  
  if (lib == 'foreign') {
    dt = as.data.table(read.spss(file, to.data.frame = TRUE, ...))
    varlbl = attr(dat, 'variable.labels')
  } else {
    dt = read_spss(file)
    varlb = sapply(dt, attr, 'label')
  }
  
  meta = data.frame(var = names(dat), lbl = varlbl, stringsAsFactors=F)
  as.ez(dt, meta)
}


#' Convert a data.frame or data.table object into an ez.data.frame.
#' 
#' @param df
#' @param meta data.frame containing meta information with at least two columns.
#' @family ez.data.frame
#' @examples 
#' data("iris")
#' aa = as.ez(iris, meta = data.frame(names(iris), c('花萼长', '花萼宽', '花瓣长', '花瓣宽', '品种')))
#' class(aa)
#' attr(aa, 'meta')
#' table(aa, ~Species)
as.ez <- function(df, meta = NULL) {
  if (!inherits(df, 'data.table')) {
    df = as.data.table(df)
  }
  if (!is.null(meta)) {
    if (!inherits(meta, 'data.frame')) stop('meta must be a data.frame')
    if (ncol(meta) < 2) stop('meta must has at least two columns')
    meta[, 1] = as.character(meta[, 1])
    meta[, 2] = as.character(meta[, 2])
    meta = as.data.table(meta)
    setkeyv(meta, names(meta)[1])
    setattr(df, 'meta', meta)
    # set variable.labels attribute for data imported from spss?
    # for better display in data viewer
  }
  class(df)<-union('ez.data.frame', class(df))
  invisible(df)
}


setmeta <- function(ez, meta) {
  UseMethod('setmeta')
}

setmeta.ez.data.frame <- function(ez, meta) {
  as.ez(ez, meta)
}

#' assign value labels to an ez.data.frame object
#' 
#' @param ez
#' @param col column name, character
#' @param labels a named vector for value labels, e.g. c(a=1, b=2, c=3)
#' @examples 
#' library(ezdf)
#' data(iris)
#' iris = as.ez(iris)
#' class(iris)
#' iris$test = sample(5, size = nrow(iris), replace = T)
#' tbl(iris, ~test)
#' options('ezdfKeepVal' = T)
#' set_value_labels(iris, 'test', c(a=1, b=2, c=3))
#' tbl(iris, ~test, )
set_value_labels <- function(ez, col, labels) {
  UseMethod('set_value_labels')
}

set_value_labels.ez.data.frame <- function(ez, col, labels) {
  setattr(ez[[col]], 'labels', labels)
}

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
tbl<- function(ez, expr, func = 'mean', N = FALSE, sort = TRUE){
  UseMethod("tbl", ez)
}

tbl.ez.data.frame<-function(ez, expr, func = 'mean', N = FALSE, sort = TRUE){
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

  expr = parse(text=paste0('lapply(.SD, ', func, ', na.rm=T)'))

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

  setmeta<-function() {
    meta = attr(ez, 'meta')
    if (!is.null(meta)) {
      meta = meta[nzchar(meta[, 2, with = F]), ]
      ns = names(dat)
      #ns = ns[ns %in% meta[,1]]
      #ret = meta[match(ns, meta[, 1, with = F], nomatch = 0), ]
      ret = meta[ns, nomatch=0]
      keepVarName = getOptKeepVarname()
      if (keepVarName) {
        ret[[2]] = paste(ret[[1]], ret[[2]], sep = '\n')
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
  setmeta()
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
         '3' = cbind(as.matrix(prop.table(t1, prop_margin)), N=apply(t1, 1, sum)))
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

getOptKeepVarname <- function(){
  keepVarName = getOption('ezdfKeepVarName', default= F)
  keepVarName
}

getOptKeepVal <- function(){
  getOption('ezdfKeepVal', default= F)
}

getOptValueLabelSep <- function() {
  getOption('ezdfValueLabelSep', default= ' ')
}

getValueLabels <- function(ez, col) {
  UseMethod('getValueLabels', ez)
}  

getValueLabels.ez.data.frame <- function(ez) {
  attr(ez[[col]], 'labels', exact = T)
}

getVarLabels <- function(ez, varnames) {
  UseMethod('getVarLabels')  
}

getVarLabels.ez.data.frame <- function(ez, varnames) {
  meta = attr(ez, 'meta')
  if (!is.null(meta)) {
    meta = meta[nzchar(meta[, 2, with = F]), ]
    ret = meta[varnames, ] #nomatch=0
    ret[[2]][is.na(ret[[2]])] = ''
    keepVarName = getOptKeepVarname()
    if (keepVarName) {
      ret[[2]] = sprintf('%s\n(%s)', ret[[2]], ret[[1]])
    } else {
      ret[[2]][is.na(ret[[2]])] = ret[is.na(ret[[2]]), 1, with = F]
    }
    ret[[2]]
  } else {
    varnames
  }
}


getVarLabelsFromMeta <- function(meta, varnames) {
  assertthat::noNA(meta)
  ret = meta[varnames, ]
  keepVarName = getOptKeepVarname()
  if (keepVarName) {
    ret[[2]] = sprintf('%s\n(%s)', ret[[2]], ret[[1]])
  } else {
    ret[[2]][is.na(ret[[2]])] = ret[is.na(ret[[2]]), 1, with = F]
  }
  ret
}

getMetaValueLabels <- function(idx, ftbl.attr, dt) {
  #lbl = attr(dt[[names(ftbl.attr)[idx] ]], 'labels')
  lbl = getValueLabels(dt, names(ftbl.attr)[idx])
  if (!is.null(lbl)) {
    fetchValueLabels(ftbl.attr[[idx]], lbl, withValue = getOptKeepVal())
  } else {
    ftbl.attr[[idx]]
  }
}

#' @noRd
#' lbl = getValueLabels(dat, 's5a')
#' fetchValueLabels(11:13, lbl)
#' fetchValueLabels(8:13, lbl, withValue = T)
fetchValueLabels <- function(x, tbl, withValue = FALSE) {
  tbl = c(' '=NA, tbl)
  if (withValue) 
    paste(x, names(tbl)[match(x, tbl, nomatch = 1)], sep = getOptValueLabelSep())
  else
    names(tbl)[match(x, tbl, nomatch = 1)]
}

#' @noRd
#' @param tbl match table with names attribute
#' @param nomatch 'asis': use values from x, else use nomatch for substitution
#' @examples 
#' a = c(1:5, 9, 'NA')
#' b = 5:1
#' names(b) = c(letters[5:1])
#' match_labels(a, b, nomatch = 'asis')
#' match_labels(a, b, nomatch = NA)
#' match_labels(a, b, nomatch = 'YY')
#' # match labels and use kay=value pair
#' tmp = match_labels(a, b, nomatch = NA)
#' tmp[!is.na(tmp)] = paste0(' = ', tmp[!is.na(tmp)])
#' tmp[is.na(tmp)] = ''
#' paste(a, tmp, sep = '')
match_labels <- function(x, tbl, nomatch = 'asis') {
  if (is.null(names(tbl))) stop('tbl MUST be named.')
  tmp = names(tbl)[match(x, tbl)]
  switch(as.character(nomatch),
         'asis' = ifelse(!is.na(tmp), tmp, x),
         ifelse(!is.na(tmp), tmp, nomatch)
  )
}

