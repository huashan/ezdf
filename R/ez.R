#
#
#

#' Import Stata .dta file using package haven
#' 
#' @param file dta file name.
#' @param encoding Encoding for labels, default is UTF-8.
#' @param varNameEncoding Encoding for variable names, default is the same as `encoding`.
#' @param charEncoding Encoding for character variables, default is the same as `encoding`.
#' @return ez.data.frame class object inherited from data.table
#' @export 
#' @family Stata dta
#' @seealso readSPSS
#' @examples 
#' library(ezdf)
#' dat = readStata('CGSS2013（居民问卷） 发布版.dta')
#' class(dat)
#' # set encoding properly
#' dat = readStata('CGSS2013（居民问卷） 发布版.dta', encoding = 'GB2312')
#' tbl(dat, a66 ~ s5a)
readStata <- function (file, encoding = NULL, varNameEncoding = encoding, 
  charEncoding = encoding) {
  require(haven)
  optUTF8 = getOption('ezdfForceUTF8', default = F)
  
  dt = read_dta(file, encoding = encoding)
  # 修正数据出现  “Error: `x` and `labels` must be same type”错误的临时解决办法：
  dt = as.data.table(lapply(dt, unclass), stringsAsFactors = F)

  if (!is.null(varNameEncoding)) {
  	nn = names(dt)
  	Encoding(nn) = varNameEncoding
  	if (optUTF8) nn <- iconv(nn, varNameEncoding, 'UTF-8')
  	setnames(dt, nn)
  }

  lbl = sapply(dt, attr, 'label')
  if (is.list(lbl)) {
    lbl[sapply(lbl, is.null)] = ''
    lbl[sapply(lbl, length) > 1] = ''  
    lbl = unlist(lbl)   
  }
  
  setValueLabels <- function(col, encoding) {
    llbl = attr(col, 'labels'); 
    if (!is.null(llbl)) {
      ns=names(llbl);
      Encoding(ns) = encoding;
	  	if (optUTF8) ns <- iconv(ns, encoding, 'UTF-8')
      names(llbl) = ns;
      setattr(col, 'labels', llbl);
    }
    invisible()
  }

  if (!is.null(encoding)) {
    #Encoding(lbl) = encoding # no need for havan v1.0
    
    # use data.table:::setattr() in case package bit is loaded
    invisible(mapply(data.table:::setattr, dt, lbl, name = 'label', SIMPLIFY = F))
    lapply(dt, setValueLabels, encoding = encoding)  
    
    if (!is.null(charEncoding)) {
      col = names(dt)[sapply(dt, class) == 'character']
      for (x in col) set(dt, NULL, x, `Encoding<-`(dt[[x]], charEncoding))
    }
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
    varlbl = attr(dt, 'variable.labels')
  } else {
    dt = read_spss(file)
    varlb = sapply(dt, attr, 'label')
  }
  
  meta = data.frame(var = names(dt), lbl = varlbl, stringsAsFactors=F)
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

getmeta <- function(ez) {
  UseMethod('getmeta')
}

getmeta.ez.data.frame <- function(ez) {
  attr(ez, 'meta')
}

getOptKeepVarname <- function(){
  keepVarName = getOption('ezdfKeepVarName', default = F)
  keepVarName
}

getOptKeepVal <- function(){
  getOption('ezdfKeepVal', default = F)
}

getOptValueLabelSep <- function() {
  getOption('ezdfValueLabelSep', default = ' ')
}

valueLabels <- function(ez, col) {
  UseMethod('valueLabels')
}

valueLabels.ez.data.frame <- function(ez, col) {
  ret = attr(ez[[col]], 'labels', exact = T)
  ret = structure(ret, class = "value.labels", 'ez'=as.character(substitute(ez)),
                  'col'= as.character(substitute(col)))
  ret
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
#' valueLabels(iris, 'test') = c(a=1, b=2, c=3)
#' tbl(iris, ~test, )
`valueLabels<-` <- function(ez, col, value) {
  if (!is.null(value) & is.null(names(value))) stop('value must be a named vector')
  UseMethod('valueLabels<-', ez)
}

`valueLabels<-.ez.data.frame` <- function(ez, col, value) {
  if (is.null(ez[[col]])) stop(sprintf('column %s not exist', col))
  data.table::setattr(ez[[col]], 'labels', value[seq_len(length(value))])
  ez
}

`+.value.labels` <- function(e1, e2) {
  if (!is.null(e2) & is.null(names(e2))) stop('value must be a named vector')
  idx_dup = match(e2, e1, nomatch = 0)
  ret = c(unlist(e1[-idx_dup]), e2)
  
  #dt = get(attr(e1, 'ez'))
  #data.table::setattr(dt[[attr(e1, 'col')]], 'labels', ret)
  
  structure(ret, class = "value.labels",
                  'ez' = attr(e1, 'ez'),
                  'col' = attr(e1, 'col')  )
}

`-.value.labels` <- function(e1, e2) {
  #if (!is.null(e2) & is.null(names(e2))) stop('value must be a named vector')
  idx_dup = match(e2, e1, nomatch = 0)
  ret = unlist(e1[-idx_dup])
  
  #dt = get(attr(e1, 'ez'))
  #data.table::setattr(dt[[attr(e1, 'col')]], 'labels', ret)
  
  structure(ret, class = "value.labels",
            'ez' = attr(e1, 'ez'),
            'col' = attr(e1, 'col')  )
}

#' Get variable labels
#' 
#' @param ez
#' @param varnames
#' @param default default value for when `varnames` exists or labels not 
#'     set. Value of c("", "var"). Default is "". "var" for copying varnames.
varLabels <- function(ez, varnames, default = '') {
  UseMethod('varLabels')  
}

varLabels.ez.data.frame <- function(ez, varnames, default = '') {
  meta = attr(ez, 'meta')
  if (missing(varnames)) meta else metaVarLabels(meta, varnames, default)
}

`varLabels<-` <- function(ez, varnames, values) {
  UseMethod('varLabels<-')  
}

`varLabels<-.ez.data.frame` <- function(ez, varnames, values) {

  meta = attr(ez, 'meta')
  if (!is.null(meta)) {
    meta[varnames, lbl:=values]
  } else {
    meta = data.table(var=varnames, lbl=values, key = 'var')
  }
  
  setattr(ez, 'meta', meta)
  ez
}

#' internal function
#' @noRd
metaVarLabels <- function(meta, varnames, default = "") {
  #assertthat::noNA(meta)
  if (!is.null(meta)) {
    # meta = meta[nzchar(meta[, 2, with = F]), ]
    # ret = meta[varnames, ] #nomatch=0
    # keepVarName = getOptKeepVarname()
    # if (keepVarName) {
    #   ret[[2]][is.na(ret[[2]])] = ''
    #   ret[[2]] = sprintf('%s\n(%s)', ret[[2]], ret[[1]])
    # } else {
    #   ret[[2]][is.na(ret[[2]])] = ret[[1]][is.na(ret[[2]])]
    # }
    ret = meta[data.table(var=varnames)]
    if (default == "var") ret[is.na(lbl), lbl:=var] else ret[is.na(lbl), lbl:=""]
    ret[[2]]
  } else {
    if (default == "var") varnames else rep("", length(varnames))
  }
}

#' called in pander_utils.r
getMetaValueLabels <- function(idx, ftbl.attr, dt) {
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

