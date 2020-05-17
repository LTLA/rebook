# Testing scrapeDependencies.
# library(testthat); library(rebook); source("test-scrape.R")

testdir <- tempfile()
dir.create(testdir)
fail <- file.path(testdir, paste0(1:3, ".Rmd"))

write(file=fail[1], "```{r}
library(CDE)
library(\"CDE2\", character.only)
library('CDE3', character.only)
```")

write(file=fail[2], "```{r}
require(EFGH)
require(\"EFGH2\", character.only)
require('EFGH3', character.only)
```")

write(file=fail[3], "```{r}
x <- IJKLM::whee(asdasd)
x=N::whee(asdasd)
(O::whee(asdasd))
1-P::whee(asdasd)
1+Q::whee(asdasd)
1/R::whee(asdasd)
1*S::whee(asdasd)
other(2,T::whee(asdasd))
2%*%U::whee(asdasd) + V::blurhg
```")

test_that("scrapeDependencies works as expected", {
    collected <- scrapeDependencies(testdir)
    expect_identical(collected, c("CDE", "CDE2", "CDE3",
        "EFGH", "EFGH2", "EFGH3",
        "IJKLM", "N", "O", "P", "Q", "R", "S", "T", "U", "V"))
})

