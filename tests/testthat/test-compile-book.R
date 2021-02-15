# This tests the book compilation process.
# library(testthat); library(rebook); source("setup.R"); source("test-compile-book.R")

example <- system.file("example", package="rebook")

tmp <- tempfile()
dir.create(tmp)
file.copy(example, tmp, recursive=TRUE)
src.dir <- file.path(tmp, "example")
write(file=file.path(src.dir, "_bookdown.yml"),
'book_filename: "Test book"
pagetitle: "I am a test book!"
delete_merged_file: true
new_session: true
language:
ui:
chapter_name: "Chapter "
output_dir: "docs"
rmd_files: ["test.Rmd"]')

test_that("compileBook works as expected", {
    work.dir <- tempfile()
    final.dir <- tempfile()

    compileBook(src.dir, work.dir, final.dir, input="test.Rmd")
    expect_true(file.exists(file.path(work.dir, "test.Rmd"))) 
    expect_true(file.exists(file.path(work.dir, "test_cache"))) # cache is promoted to the right place.
    expect_true(file.exists(file.path(final.dir, "test-chapter.html"))) # HTMLs are moved to the right place.
})

test_that("compileBook defaults to the right place for the compiled book", {
    write(file=file.path(src.dir, "_bookdown.yml"),
'book_filename: "Test book"
pagetitle: "I am a test book!"
delete_merged_file: true
new_session: true
language:
  ui:
    chapter_name: "Chapter "
rmd_files: ["test.Rmd"]')

    work.dir <- tempfile()
    final.dir <- tempfile()

    compileBook(src.dir, work.dir, final.dir, input="test.Rmd")
    expect_true(file.exists(file.path(work.dir, "test.Rmd"))) 
    expect_true(file.exists(file.path(work.dir, "test_cache"))) # cache is promoted to the right place.
    expect_true(file.exists(file.path(final.dir, "test-chapter.html"))) # HTMLs are moved to the right place.
})
