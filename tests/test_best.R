library('testthat')

setwd('..')
source('best.R')

context('best hospital')

test_that('it throws an error with an invalid state', {
    expect_that(best("BB", "heart attack"), throws_error('invalid state'))
})

test_that('it throws an error with an invalid outcome', {
    expect_that(best("NY", "hert attack"),  throws_error('invalid outcome'))
})

test_that('it returns the best hospital', {
    expect_that(best("TX", "heart attack"), equals("CYPRESS FAIRBANKS MEDICAL CENTER"))
    expect_that(best("TX", "heart failure"), equals("FORT DUNCAN MEDICAL CENTER"))
    expect_that(best("MD", "heart attack"), equals("JOHNS HOPKINS HOSPITAL, THE"))
    expect_that(best("MD", "pneumonia"), equals("GREATER BALTIMORE MEDICAL CENTER"))
})
