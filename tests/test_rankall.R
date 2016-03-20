library("testthat")

setwd("..")
source("rankall.R")

context("rank all")

hospital = 1
state = 2

test_that('it works with the best', {
    result <- tail(rankall("heart failure"), 10)

    expect_that(result[[1,hospital]], equals('WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL'))
    expect_that(result[[1,state]], equals('TN'))

    expect_that(result[[2,hospital]], equals('FORT DUNCAN MEDICAL CENTER'))
    expect_that(result[[2,state]], equals('TX'))

    expect_that(result[[3,hospital]], equals('VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER'))
    expect_that(result[[3,state]], equals('UT'))
})
# tail(rankall("heart failure"), 10)
# hospital state
# TN                         WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL    TN
# TX                                        FORT DUNCAN MEDICAL CENTER    TX
# UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER    UT
# VA                                          SENTARA POTOMAC HOSPITAL    VA
# VI                            GOV JUAN F LUIS HOSPITAL & MEDICAL CTR    VI
# VT                                              SPRINGFIELD HOSPITAL    VT
# WA                                         HARBORVIEW MEDICAL CENTER    WA
# WI                                    AURORA ST LUKES MEDICAL CENTER    WI
# WV                                         FAIRMONT GENERAL HOSPITAL    WV
# WY                                        CHEYENNE VA MEDICAL CENTER    WY


test_that('it works with an arbitrary index', {
    result <- head(rankall("heart attack", 20), 10)

    expect_that(result[[1,hospital]], equals(NA_character_))
    expect_that(result[[1,state]], equals('AK'))

    expect_that(result[[2,hospital]], equals('D W MCMILLAN MEMORIAL HOSPITAL'))
    expect_that(result[[2,state]], equals('AL'))

    expect_that(result[[3,hospital]], equals('ARKANSAS METHODIST MEDICAL CENTER'))
    expect_that(result[[3,state]], equals('AR'))
})
# hospital state
# AK                                <NA>    AK
# AL      D W MCMILLAN MEMORIAL HOSPITAL    AL
# AR   ARKANSAS METHODIST MEDICAL CENTER    AR
# AZ JOHN C LINCOLN DEER VALLEY HOSPITAL    AZ
# CA               SHERMAN OAKS HOSPITAL    CA
# CO            SKY RIDGE MEDICAL CENTER    CO
# CT             MIDSTATE MEDICAL CENTER    CT
# DC                                <NA>    DC
# DE                                <NA>    DE
# FL      SOUTH FLORIDA BAPTIST HOSPITAL    FL


test_that('it works with the worst', {
    result <- tail(rankall("pneumonia", "worst"), 3)

    expect_that(result[[1,hospital]], equals('MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC'))
    expect_that(result[[1,state]], equals('WI'))

    expect_that(result[[2,hospital]], equals('PLATEAU MEDICAL CENTER'))
    expect_that(result[[2,state]], equals('WV'))

    expect_that(result[[3,hospital]], equals('NORTH BIG HORN HOSPITAL DISTRICT'))
    expect_that(result[[3,state]], equals('WY'))
})