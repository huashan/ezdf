library(testthat)
library(ezdf)

data(iris)
ezdt = as.ez(iris)
ezdt$test = sample(5, size = nrow(iris), replace = T)

test_that('as.ez.data.frame', {
  expect_is(ezdt, 'ez.data.frame')
})


test_that('value labels', {
  vl1 = valueLabels(ezdt, 'test')
  expect_equal(length(vl1), 0)
  expect_equivalent(vl1, list())
  expect_equal(attr(vl1, 'ez'), 'ezdt')
  expect_equal(attr(vl1, 'col'), 'test')
  
  vl2 = vl1 + c(MI=9, MM=8)
  expect_equivalent(unclass(vl2), c(9, 8))
  vl2.1 = vl2 + c('NN'=9)
  expect_named(vl2.1, c('MM', 'NN'))
  
  vl3 = vl1 - c('a'=8)
  expect_equal(vl3, vl1)
  
  vl4 = vl2 - c(9)
  expect_equal(length(vl4), 1)
  expect_equivalent(unclass(vl4), 8)
  
  valueLabels(ezdt, 'test') = vl2
  expect_equal(valueLabels(ezdt, 'test'), vl2)
})

test_that('var labels', {
  expect_null(varLabels(ezdt))

  meta = varLabels(ezdt, c('Species','test'))
  expect_equal(meta, c("", ""))
  
  meta = varLabels(ezdt, c('Species','test'), default = "var")
  expect_equal(meta, c('Species','test'))
  
  expect_equal(varLabels(ezdt, ''), '')
  expect_equal(varLabels(ezdt, 'nonexists'), '')
  expect_equal(varLabels(ezdt, 'nonexists', default = 'var'), 'nonexists')
  
  varLabels(ezdt, 'test') = 'TEST'
  expect_equal(varLabels(ezdt, 'test'), 'TEST')

  expect_equal(varLabels(ezdt, c('Species', 'test')), c('', 'TEST'))
  expect_equal(varLabels(ezdt, c('Species', 'test'), "var"), c('Species', 'TEST'))
  
  varLabels(ezdt, "test") <- "New Label"
  varLabels(ezdt, "test")
})

test_that('tbl', {
  tmp = 1:5
  names(tmp) = LETTERS[1:5]
  valueLabels(ezdt, 'test') = tmp 
  tbl(ezdt, Sepal.Length ~ test)

  options('ezdfKeepVal' = T)
  options('ezdfValueLabelSep' = '=')
  tbl(ezdt, Sepal.Length ~ test)
  
})


