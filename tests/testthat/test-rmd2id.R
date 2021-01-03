# This tests that rmd2id works correctly.
# library(testthat); library(rebook); source("test-rmd2id.R")

test_that("rmd2id works in vanilla scenarios", {
    tmp <- tempfile(fileext='.Rmd')
    write('# some chapter name
                                    
blah', file=tmp)
    expect_identical(rmd2id(tmp), 'some-chapter-name')
})

test_that("rmd2id works with custom identifiers", {
    tmp <- tempfile(fileext='.Rmd')
    write('# some chapter name {#custom-id}
                                    
blah', file=tmp)
    expect_identical(rmd2id(tmp), 'custom-id')
})

test_that("rmd2id works with annoying code before it", {
    tmp <- tempfile(fileext='.Rmd')
    write('```{r}
# some comments
```

# the actual chapter name 

blah', file=tmp)
    expect_identical(rmd2id(tmp), 'the-actual-chapter-name')

    tmp <- tempfile(fileext='.Rmd')
    write('```{r}
# some comments
```

blah', file=tmp)
    expect_null(rmd2id(tmp))
})

test_that("rmd2id works with PART", {
    tmp <- tempfile(fileext='.Rmd')
    write('# (PART) BLAH

# the actual chapter name 

blah', file=tmp)
    expect_identical(rmd2id(tmp), 'the-actual-chapter-name')
})


