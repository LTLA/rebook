# This tests the behavior of extractFromPackage.
# library(testthat); library(rebook); source("setup.R"); source("test-extract-package.R")

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

test_that("extractFromPackage works sensibly from an existing workspace", {
    work.dir <- tempfile()
    final.dir <- tempfile()

    compileBook(src.dir, work.dir, final.dir, input="test.Rmd")
    envir <- new.env()
    extractFromPackage("test.Rmd", chunk="ghidorah-1964", work.dir=work.dir, package="rebook", objects="godzilla", envir=envir)
    expect_identical(envir$godzilla, "GAO GAO")
    expect_false(file.exists(file.path(work.dir, "test.html")))

    unlink(file.path(work.dir, "test_cache"), recursive=TRUE)
    envir <- new.env()
    extractFromPackage("test.Rmd", chunk="ghidorah-1964", work.dir=work.dir, package="rebook", objects="godzilla", envir=envir)
    expect_identical(envir$godzilla, "GAO GAO")
    expect_true(file.exists(file.path(work.dir, "test_cache")))
    expect_true(file.exists(file.path(work.dir, "test-chapter.html"))) # triggers recompilation to build the cache.
})

test_that("extractFromPackage works sensibly in the absence of any workspace", {
    work.dir <- tempfile()
    final.dir <- tempfile()

    envir <- new.env()
    extractFromPackage("test.Rmd", chunk="ghidorah-1964", work.dir=work.dir, package="rebook", objects="godzilla", src.name="example", envir=envir)
    expect_identical(envir$godzilla, "GAO GAO")
    expect_true(file.exists(file.path(work.dir, "test_cache")))
})

test_that("extractFromPackage is thread-safe", {
    skip_on_os("windows")

    work.dir <- tempfile()
    final.dir <- tempfile()

    library(BiocParallel)
    out <- bplapply(1:2, function(i) {
        env <- new.env()
        capture.output(rebook::extractFromPackage("test.Rmd", chunk="godzilla-1954", work.dir=work.dir, package="rebook", src.name="example", object="godzilla", envir=env))
        env$godzilla
    }, BPPARAM=MulticoreParam(2))

    expect_identical(out[[1]], "RAWR!")
    expect_identical(out[[2]], "RAWR!")
})
