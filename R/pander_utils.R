match_var_labels <- function(expr, meta) {
  assertthat::noNA(meta)
  ret = meta[expr, ]
  keepVarName = getOptKeepVarname()
  if (keepVarName) {
    ret[[2]] = sprintf('%s\n(%s)', ret[[2]], ret[[1]])
  } else {
    ret[[2]][is.na(ret[[2]])] = ret[is.na(ret[[2]]), 1, with = F]
  }
  ret
}

pander.summary.lm_x <- function(x, caption = attr(x, 'caption'), covariate.labels,
                  omit, summary = TRUE, add.significance.stars = FALSE, ...){

  if (missing(covariate.labels) ) {
    model_data = as.character(x$call)[3]
    model_data = if (is.na(model_data)) model_data else get(model_data)
    if (inherits(model_data, 'ez.data.frame')) {
      meta = attr(model_data, 'meta')
      covariate.labels = meta
    }
  }
  #  covariate.labels may be a character vector or data.frame ( meta from ez.data.frame or provided as is)
  if (!is.null(covariate.labels)) {
    if (!is.null(dim(covariate.labels))) {
      covariate.labels = match_var_labels(rownames(x$coefficients), covariate.labels)[[2]]
    }
  }
  pander.summary.lm_y = get('pander.summary.lm_y', ez_globals)
  pander.summary.lm_y(x, caption, covariate.labels, omit, summary, add.significance.stars, ...)
}

init_hooks <- function(){
  #aa = get("FOO", pkg_globals)
  #if (is.null(pander.summary.lm_y)) {
  if (!exists('pander.summary.lm_y', ez_globals)) {
    #pander.summary.lm_y <<- pander:::pander.summary.lm
    assign("pander.summary.lm_y", pander:::pander.summary.lm, ez_globals)
    pkgutils::set_hook('pander', 'pander.summary.lm', pander.summary.lm_x)
  }
}



