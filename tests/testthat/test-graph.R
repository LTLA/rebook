# Testing buildChapterGraph.
# library(testthat); library(rebook); source("test-graph.R")

dir <- tempfile()
dir.create(dir)
                                                     
tmp1 <- file.path(dir, "alpha.Rmd")
write(file=tmp1, "```{r, echo=FALSE, results='asis'}
rebook::chapterPreamble()
```

```{r}
rodan <- 1
```")
                                                     
tmp2 <- file.path(dir, "bravo.Rmd")
write(file=tmp2, "```{r, echo=FALSE, results='asis'}
rebook::chapterPreamble()
```

```{r}
extractCached('alpha.Rmd')
```")

test_that("building chapter graph works", {
    g <- buildChapterGraph(dir)
    expect_identical(names(igraph::V(g)), c("alpha.Rmd", "bravo.Rmd"))
    expect_true(igraph::are_adjacent(g, "alpha.Rmd", "bravo.Rmd"))
})

test_that("Makefile construction works", {
    createMakefile(dir)
    expect_true(file.exists(file.path(dir, "Makefile")))
})
