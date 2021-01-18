# This tests the behavior of extractFromPackage.
# library(testthat); library(rebook); source("test-extract-package.R")

tmp <- tempfile()
envir <- new.env()
extractFromPackage("test.Rmd", chunk="ghidorah-1964", inst.dir="example",
    envir=envir, objects="godzilla", package="rebook", work.dir=tmp)

test_that("extractFromPackage works as expected", {
    expect_identical(envir$godzilla, "GAO GAO")

    cache.dir <- file.path(tmp, "rebook", as.character(packageVersion("rebook")))
    expect_true(file.exists(cache.dir))
    expect_true(file.exists(file.path(cache.dir, "test_cache")))

    # Repeated extractions work.
    extractFromPackage("test.Rmd", chunk="godzilla-2014", inst.dir="example",
        envir=envir, objects="godzilla", package="rebook", work.dir=tmp)
    expect_identical(envir$godzilla, "I'm back.")

    # Extractions clear out previous entities.
    old.dir <- file.path(tmp, "rebook", "0.0.1")
    file.rename(cache.dir, old.dir)
    extractFromPackage("test.Rmd", chunk="godzilla-2014", inst.dir="example",
        envir=envir, objects="godzilla", package="rebook", work.dir=tmp)
    expect_false(file.exists(old.dir))
})
