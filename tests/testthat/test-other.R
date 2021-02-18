# This tests the various other miscellaneous functions.
# library(testthat); library(rebook); source("test-other.R")

test_that("setupHTML works as expected", {
    expect_output(setupHTML(), "rebook-collapse")
})

test_that("chapterPreamble works as expected", {
    expect_output(chapterPreamble(), "rebook-collapse")
    expect_identical(knitr::opts_chunk$get("error"), FALSE)
    expect_identical(knitr::opts_chunk$get("message"), FALSE)
    expect_identical(knitr::opts_chunk$get("warning"), FALSE)
})

test_that("prettySessionInfo works as expected", {
    expect_output(prettySessionInfo(), "session info")
})

test_that("YAML extraction works as expected", {
    dir <- tempfile()
    dir.create(dir, showWarnings=FALSE)
    write(character(0), file=file.path(dir, "cat.Rmd"))
    write(character(0), file=file.path(dir, "bird.Rmd"))
    write(file=file.path(dir, "_bookdown.yml"), character(0))

    dir.create(file.path(dir, "fish"))
    write(character(0), file=file.path(dir, "fish", "koi.Rmd"))
    write(character(0), file=file.path(dir, "fish", "salmon.Rmd"))

    out <- rebook:::.get_book_chapters(dir)
    expect_identical(sort(out), c("bird.Rmd", "cat.Rmd"))
    expect_identical(rebook:::.find_output_directory(dir), "_book")

    write(file=file.path(dir, "_bookdown.yml"), "rmd_subdir: ['fish']")
    out <- rebook:::.get_book_chapters(dir)
    expect_identical(sort(out), c("bird.Rmd", "cat.Rmd", "fish/koi.Rmd", "fish/salmon.Rmd"))
    
    write(file=file.path(dir, "_bookdown.yml"),
        "rmd_files: ['rabbit.Rmd', 'dog.Rmd']")
    out <- rebook:::.get_book_chapters(dir)
    expect_identical(out, c("rabbit.Rmd", "dog.Rmd"))

    write(file=file.path(dir, "_bookdown.yml"),
        "output_dir: docs")
    expect_identical(rebook:::.find_output_directory(dir), "docs")
})
