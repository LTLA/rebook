# This tests the various other miscellaneous functions.
# library(testthat); library(rebook); source("test-other.R")

test_that("setupHTML works as expected", {
    expect_output(setupHTML(), "aaron-collapse")
})

test_that("chapterPreamble works as expected", {
    expect_output(chapterPreamble(), "aaron-collapse")
    expect_identical(knitr::opts_chunk$get("error"), FALSE)
    expect_identical(knitr::opts_chunk$get("message"), FALSE)
    expect_identical(knitr::opts_chunk$get("warning"), FALSE)
})

test_that("prettySessionInfo works as expected", {
    expect_output(prettySessionInfo(), "session info")
})
