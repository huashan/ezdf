library(testthat)
library(ezdf)

data(iris)
ezdt$test = sample(5, size = nrow(iris), replace = T)
ezdt = as.ez(iris)

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
  meta = varLabels(ezdt, c('Species','test'))
  expect_equal(meta, c('Species','test'))
  
  expect_equal(varLabels(ezdt, ''), '')
  #expect_equal(varLabels(ezdt, ''), character(0))
  expect_equal(varLabels(ezdt, 'nonexists'), 'nonexists')
  
  varLabels(ezdt, 'test') = 'TEST'
  expect_equal(varLabels(ezdt, 'test'), 'TEST')

  expect_equal(varLabels(ezdt, c('Species', 'test')), c('Species', 'TEST'))
  
 # meta = attr(ezdt, 'meta')
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

#debug('varLabels.ez.data.frame')
