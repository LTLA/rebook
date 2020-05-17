# This tests the ability of extractCached to do its job.
# library(testthat); library(rebook); source("test-cache.R")

example <- system.file("example", "test.Rmd", package="rebook")
tmp <- tempfile()
tmprmd <- paste0(tmp, ".Rmd")
file.copy(example, tmprmd)

test_that("first attempt will compile the report", {
    env <- new.env()
    out <- capture.output(extractCached(tmprmd, chunk="godzilla-1954", object="godzilla", envir=env))
    expect_identical(env$godzilla, "RAWR!")
    
    expect_true(any(grepl("godzilla-1954", out)))
    expect_false(any(grepl("ghidorah-1964", out)))
})

test_that("multiple objects can be retrieved", {
    env <- new.env()
    out <- capture.output(extractCached(tmprmd, chunk="ghidorah-1964", object=c("godzilla", "ghidorah"), envir=env))
    expect_identical(env$godzilla, "GAO GAO")
    expect_identical(env$ghidorah, "pew pew")

    expect_true(any(grepl("godzilla-1954", out)))
    expect_true(any(grepl("ghidorah-1964", out)))
    
    # It doesn't pick up the other chunks.
    expect_false(any(grepl("msg <-", out)))
    expect_false(any(grepl("plot\\(", out)))
})

test_that("later objects can be successfully retrieved", {
    env <- new.env()
    out <- capture.output(extractCached(tmprmd, chunk="godzilla-2014", object="godzilla", envir=env))
    expect_identical(env$godzilla, "I'm back.")
    
    expect_true(any(grepl("godzilla-1954", out)))
    expect_true(any(grepl("ghidorah-1964", out)))
    expect_true(any(grepl("godzilla-1978", out)))
    expect_true(any(grepl("godzilla-2014", out)))
})

test_that("objects from earlier chunks can be successfully retrieved", {
    env <- new.env()
    out <- capture.output(extractCached(tmprmd, chunk="godzilla-2014", object=c("mechagodzilla", "godzilla"), envir=env))
    expect_identical(env$godzilla, "I'm back.")
    expect_identical(env$mechagodzilla, "beep beep")

    expect_true(any(grepl("godzilla-1954", out)))
    expect_true(any(grepl("ghidorah-1964", out)))
    expect_true(any(grepl("godzilla-1978", out)))
    expect_true(any(grepl("godzilla-2014", out)))
})

test_that("stuff is created in the global environment on request", {
    capture.output(extractCached(tmprmd, chunk="godzilla-1954", object=c("godzilla")))
    expect_identical(godzilla, "RAWR!")
})

