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


test_that("updateDependencies works as expected", {
    write(file=file.path(testdir, "DESCRIPTION"),
"Package: son.of.godzilla
Version: 0.0.1
Description: Like godzilla, but smaller.")
    
    updateDependencies(testdir)
    out <- read.dcf(file.path(testdir, "DESCRIPTION"))
    imports <- strsplit(out[,"Imports"], ",\\s+")[[1]]
    expect_true("O" %in% imports)
    expect_true("IJKLM" %in% imports)

    write(file=file.path(testdir, "DESCRIPTION"),
"Package: son.of.godzilla
Version: 0.0.1
Description: Like godzilla, but smaller.
Imports: nothing")

    updateDependencies(testdir)
    out <- read.dcf(file.path(testdir, "DESCRIPTION"))
    imports <- strsplit(out[,"Imports"], ",\\s+")[[1]]
    expect_true("O" %in% imports)
    expect_true("IJKLM" %in% imports)
})

