# This checks the getting of the book cache.
# library(testthat); library(rebook); source("test-book-cache.R")

test_that("getBookCache works as expected", {
    out <- getBookCache("rebook")
    expect_match(out, "rebook.*rebook")
    expect_identical(basename(out), as.character(packageVersion('rebook')))
})

test_that("getBookCache responds to environment variables", {
    old <- Sys.getenv("REBOOK_REBOOK_CACHE", NA)
    Sys.setenv(REBOOK_REBOOK_CACHE="WHEE")

    out <- getBookCache("rebook")
    expect_identical(out, old)

    Sys.setenv(REBOOK_REBOOK_CACHE=old)
})
