# This tests the creation of the redirection page.
# library(testthat); library(rebook); source("test-redirects.R")

test_that("createRedirects works as expected", {
    tmp <- tempfile()
    dir.create(tmp)
    createRedirects(c("BLAH.html", "WHEE.html"),
        pkg=c("OSCA.intro", "OSCA.basic"),
        page=c("installation.html", "index.html"),
        dir=tmp)

    bpath <- file.path(tmp, "BLAH.html")
    expect_true(file.exists(bpath))
    wpath <- file.path(tmp, "WHEE.html")
    expect_true(file.exists(wpath))

    expect_true(any(grepl("OSCA.intro/installation.html", readLines(bpath))))
    expect_true(any(grepl("OSCA.basic/index.html", readLines(wpath))))
})

test_that("createRedirects works with a file", {
    tmp <- tempfile()
    dir.create(tmp)
    opath <- file.path(tmp, "redirect.txt")

    write.table(data.frame(path=c("BLAH.html", "WHEE.html"),
        pkg=c("OSCA.intro", "OSCA.basic"),
        page=c("installation.html", "index.html")),
        file=opath, sep=",", col.names=FALSE, row.names=FALSE, quote=FALSE)

    createRedirects(file=opath, dir=tmp)

    bpath <- file.path(tmp, "BLAH.html")
    expect_true(file.exists(bpath))
    wpath <- file.path(tmp, "WHEE.html")
    expect_true(file.exists(wpath))

    expect_true(any(grepl("OSCA.intro/installation.html", readLines(bpath))))
    expect_true(any(grepl("OSCA.basic/index.html", readLines(wpath))))

    # Also works without gifs... but why?
    expect_error(createRedirects(file=opath, dir=tmp, include.gif=FALSE), NA)
})
