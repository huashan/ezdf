freq_bar <- function(df, varname, theme = NULL, useNA = 'no', NAlabel = '缺失') {
  #varname = ifelse(is.character(substitute(varname)), varname, substitute(varname))
  varname = ifelse(is.call(substitute(varname)), varname, substitute(varname))
  if (!varname %in% names(df)) stop(sprintf('%s not exists in the data', varname))
  
  tbl = table(df[[as.character(varname)]], useNA = useNA)
  axislbl = names(tbl)    
  # 有效样本数，不含缺失值
  tbl_n = sum(tbl[!is.na(axislbl)])
  axislbl[which(is.na(axislbl))] = NAlabel
  

  aes <- eval(substitute(aes(as.factor(x), ..count../sum(..count..), ymax = max(..count../sum(..count..)) + 0.05), 
                         list(x = as.name(varname)  )))
  
  # 数值型，但是含有 attr(, 'value.labels')
  if (is.numeric(df[[as.character(varname)]])) {
    lbl = attr(df[[varname]], 'value.labels')
    if (!is.null(lbl)) {
      tt = names(lbl)[match(axislbl, lbl)]
      axislbl = ifelse(!is.na(tt), tt, axislbl)
    }
  }
  
  df2 = if (useNA == 'no') df[!is.na(df[[varname]]), ] else df
  g = ggplot(data = df2, aes) + geom_bar(width = 0.5) + 
    stat_bin(aes(label = paste(format(100 * ..count../sum(..count..)), '%')), geom = 'text', vjust = -1) + 
    labs(x = '', y = '') + scale_x_discrete(labels = axislbl) + theme + #theme_clear + 
    coord_cartesian(ylim = c(0, 0.05 + max(sapply(tbl, `/`, sum(tbl))) )) # 此处计算的样本比例，含缺失值
  
  g
}

options('digits'= 1)

library(ggplot2)
library(ggthemes)

theme_clear <- #theme_bw() + 
  theme(#axis.text = element_blank(),
    axis.text.y=element_blank(),
    #axis.text.x=element_blank(),
    #axis.ticks=element_blank(),
    legend.position = 'none',
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1, 1, 0, 1), "lines"))

debug(freq_bar)
freq_bar(dat, 'c3')
freq_bar(dat, 'a2a04', useNA = 'ifany', theme = theme_wsj(9) + theme_clear )

a = c(1:5, 9, 'NA')
b = 5:1
names(b) = c(letters[5:1])
tt = names(b)[match(a, b)]
paste(a, ifelse(!is.na(tt), tt, a))


