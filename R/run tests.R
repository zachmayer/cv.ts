
#Setup
rm(list = ls(all = TRUE))
library(testthat)
library(fpp)
library(doParallel)
source('cv.ts.R')
source('forecast functions.R')
setwd('/..')

#Run Tests sequentially
test_dir('tests', reporter = 'Summary')

#Run Tests in parallel
cl <- makeCluster(4, type='SOCK')
registerDoParallel(cl)
test_dir('tests', reporter = 'Summary')
stopCluster(cl)