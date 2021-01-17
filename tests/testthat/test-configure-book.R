# This tests the configuration of the book.
# library(testthat); library(rebook); source("test-configure-book.R")

test_that("configureBook works correctly", {
    dir <- tempfile() 
    dir.create(dir)
    old <- setwd(dir)

    dir.create("inst")
    file.copy(system.file("example", package="rebook"), "inst/", recursive=TRUE)
    file.rename("inst/example", "inst/book")
    write("Package: dummy", file="DESCRIPTION")

    expect_error(configureBook(prefix="YAY"), NA)

    lines <- readLines("vignettes/Makefile")
    expect_true(any(grepl("docs", lines)))

    lines <- readLines("vignettes/stub.Rmd")
    expect_true(any(grepl("dummy", lines)))
    
    expect_true(file.exists("inst/rebook/references.csv"))
    expect_identical(readLines("inst/rebook/prefix.csv"), "YAY")

    # Deletes the prefix file.
    unlink("vignettes", recursive=TRUE)
    expect_error(configureBook(), NA)
    expect_false(file.exists("inst/rebook/prefix.csv"))

    # Check that the stub file can be compiled.
    expect_error(rmarkdown::render("vignettes/stub.Rmd"), NA)
    expect_true(file.exists('vignettes/stub.html'))

    setwd(old)
})
