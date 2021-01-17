# This tests the scrapeReferences function.
# library(testthat); library(rebook); source("test-scrape-refs.R")

expected <- "id                   file    text
looking-for-plots looking-for-plots.html       3
fig:test-plot looking-for-plots.html     3.1
fig:more-plot looking-for-plots.html     3.2
fig:more-plot2 looking-for-plots.html     3.3
fig:more-plot3 looking-for-plots.html     3.4
fig:blah-plot looking-for-plots.html     3.5
section-tests     section-tests.html       2
subsection-is-here     section-tests.html     2.1
subsubsection-is-over-here     section-tests.html   2.1.1
aaron-special-id     section-tests.html     2.2
even-more-random-stuff     section-tests.html   2.2.1
deep-nested-sub     section-tests.html 2.2.1.1
test-chapter      test-chapter.html       1
fig:unref-figure      test-chapter.html     1.1"

test_that("scrapeReferences works correctly", {
    book.dir <- system.file("example", package="rebook")
    df <- scrapeReferences(book.dir)
    ref <- read.table(text=expected, header=TRUE)
    expect_identical(df, ref)
})

test_that("link works correctly", {
    df <- read.table(text=expected, header=TRUE)

    out <- link("fig:test-plot", package="stuff", df=df)
    expect_match(out, "[**stuff** Figure 3.1]", fixed=TRUE)

    out <- link("fig:test-plot", package="stuff", df=df, prefix=NA)
    expect_match(out, "[Figure 3.1]", fixed=TRUE)

    out <- link("fig:test-plot", package="stuff", df=df, type=NA)
    expect_match(out, "[3.1]", fixed=TRUE)

    out <- link("test-chapter", package="stuff", df=df)
    expect_match(out, "[**stuff** Chapter 1]", fixed=TRUE)

    out <- link("deep-nested-sub", package="stuff", df=df)
    expect_match(out, "[**stuff** Section 2.2.1.1]", fixed=TRUE)
})
