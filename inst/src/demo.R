setwd('E:/Huashan/Stat/R/Lectures/北大新传/data')

#library(pander)


library(ezdf)
dat = readStata('CGSS2013（居民问卷） 发布版.dta', encoding = 'GB2312')
tbl(dat, a66 ~ s5a)
tbl(dat, a66 ~ s5a + s41, 'sum')
tbl(dat, a66 ~ s5a + s41, 'mean', N = T)
pander(tbl(dat, ~ s41 + s5a + a66, 'sum', sort = T))

aa = tbl(dat, ~ s5a + a66)



table(list(dat$a66, dat$s5a))

ctbl(dat, a66 ~ s5a)
ctbl(dat, ~ s5a + a66)

t1 = table(dat[, list(a66, s5a, s41)])
t1 = ftable(a66 ~ s5a + s41, dat)

ftable(dat, a66 ~ s5a, style = 2)
pander(ftable(dat, a66 ~ s5a, style = 2, prop_margin = 1))

pander(ftable(dat, a66 ~ s5a))


margin.table(t2, 1)
margin.table(t1, 2)
t3=prop.table(t2, 1)

str(t1)
t1[,,1]

attr(t1, 'dimnames')

tt = apply(table(dat[, list(a66, s5a)]), 1, sum)
sweep(table(dat[, list(a66, s5a)]), 1, tt, '/')

options('ezdfKeepVal' = T)
pander(tbl(dat, a66 ~ s5a, 'mean'))

options('ezdfValueLabelSep' = '=')
pander(tbl(dat, a66 ~ s5a, 'mean'))

m1 = lm(a6~a2 + a10, dat)
pander(m1)

panderOptions('keep.line.breaks', F)
options('ezdfKeepVarName' =  T)
pander(m1)





library(ezdf)
data(iris)
iris = as.ez(iris)
class(iris)
iris$test = sample(5, size = nrow(iris), replace = T)
tbl(iris, ~test)
options('ezdfKeepVal' = T)
set_value_labels(iris, 'test', c(a=1, b=2, c=3))
tbl(iris, ~test)

iris = setmeta(iris, data.frame(var= 'test', lbl = 'YYYY'))
attr(iris, 'meta')

t1 = ftable(iris$Species, iris$test)
pander:::pander.ftable(t1, ez = iris)

# error? 
tbl(d1, Species~test, 'length')
