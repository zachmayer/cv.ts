
#Setup
rm(list = ls(all = TRUE))
library(testthat)
setwd('~/Dropbox/Projects/R_Packages/ts.cv/')
source('cv.ts.R')

#Run Tests sequentially
library(fpp)
test_dir('tests', reporter = 'Summary')

#Run Tests in parallel
library(fpp)
library(doSMP)
w <- startWorkers(3)
registerDoSMP(w)
test_dir('tests', reporter = 'Summary')
stopWorkers(w)
