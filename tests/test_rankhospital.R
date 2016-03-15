library("testthat")

setwd("..")
source("rankhospital.R")

context("rank hospital")

test_that("throws an error with an invalid state", {
    expect_that(rankhospital("BB", "heart attack"), throws_error("invalid state"))
})

test_that("throws an error with an invalid outcome", {
    expect_that(rankhospital("NY", "hert attack"),  throws_error("invalid outcome"))
})

test_that("returns the best hospital", {
    expect_that(rankhospital("TX", "heart attack"), equals("CYPRESS FAIRBANKS MEDICAL CENTER"))
    expect_that(rankhospital("TX", "heart failure"), equals("FORT DUNCAN MEDICAL CENTER"))
    expect_that(rankhospital("MD", "heart attack"), equals("JOHNS HOPKINS HOSPITAL, THE"))
    expect_that(rankhospital("MD", "pneumonia"), equals("GREATER BALTIMORE MEDICAL CENTER"))
})

test_that("returns the worst hospital", {
    expect_that(rankhospital("TX", "heart failure", "worst"), equals("METHODIST WEST HOUSTON HOSPITAL"))
    expect_that(rankhospital("MD", "heart attack", "worst"), equals("FORT WASHINGTON HOSPITAL"))
    expect_that(rankhospital("MD", "pneumonia", "worst"), equals("CIVISTA MEDICAL CENTER"))
})

test_that("returns the correct hospital in case of a tie", {
    expect_that(rankhospital("TX", "heart failure", 3), equals("DETAR HOSPITAL NAVARRO"))
    expect_that(rankhospital("TX", "heart failure", 4), equals("CYPRESS FAIRBANKS MEDICAL CENTER"))
})

test_that("returns NA for a index that out of range", {
    expect_that(rankhospital("NY", "pneumonia", 5000), equals(NA_character_))
})
