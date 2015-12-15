setwd('E:/Huashan/Stat/R/Lectures/北大新传/data')
file = 'CGSS2013（居民问卷） 发布版.dta'
encoding = 'GB2312'


debug(tbl.ez.data.frame)
tbl(dat, I(a66 - 1) ~ s5a + s41, 'mean')

dat[, .N, by = list(I(a66-1), s41)]

dat[, .N, by = 's41']




debug(pandoc.table.return)
debug(table.expand)
detach('package:pander')
detach('package:ezfm')




table.expand <- function(cells, cols.width, justify, sep.cols, style) {
  enc = any(Encoding(cells) == 'UTF-8', na.rm  = T)
  tmp = as.integer(nchar(do.call(cbind, strsplit(cells, "\n")), type='width'))
  ret = .Call('pander_tableExpand_cpp', PACKAGE = 'pander', cells, cols.width, tmp, justify, sep.cols, style)
  if (enc) Encoding(ret) = 'UTF-8'
  ret
}

aa = c('aa', 'akaka\nkakak', 'a10\n您目前的政治面貌是')
table.expand(aa, c(20,20,24), 'left', ' ', 'simple')
