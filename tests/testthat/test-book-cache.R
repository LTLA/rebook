# This checks the getting of the book cache.
# library(testthat); library(rebook); source("test-book-cache.R")

test_that("bookCache works as expected", {
    old <- Sys.getenv("REBOOK_CACHE", NA)
    Sys.unsetenv("REBOOK_CACHE")

    out <- bookCache("rebook")
    expect_match(out, "rebook.*rebook")
    expect_identical(basename(out), as.character(packageVersion('rebook')))

    Sys.setenv(REBOOK_CACHE="WHEE")

    out <- bookCache("rebook")
    expect_identical(dirname(out), "WHEE/rebook")

    Sys.setenv(REBOOK_REBOOK_CACHE=old)
})

test_that("bookCacheExpiry responds to environment variables", {
    old <- Sys.getenv("REBOOK_CACHE_EXPIRY", NA)
    Sys.unsetenv("REBOOK_CACHE_EXPIRY")

    expect_identical(bookCacheExpiry(), 7L)

    Sys.setenv(REBOOK_CACHE_EXPIRY=1)
    expect_identical(bookCacheExpiry(), 1L)

    Sys.setenv(REBOOK_CACHE_EXPIRY=old)
}) 

