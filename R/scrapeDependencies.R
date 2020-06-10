#' Scrape dependencies
#' 
#' Scrape Rmarkdown reports in the book for all required dependencies.
#'
#' @param dir String containing the path to the directory containing Rmarkdown reports.
#' This is searched recursively for all files ending in \code{".Rmd"}.
#' @param recursive,pattern Further arguments to pass to \code{\link{list.files}} 
#' when searching for Rmarkdown reports.
#'
#' @return Character vector of required packages.
#'
#' @details
#' The output of this should be added to the \code{Suggests} field of the book's \code{DESCRIPTION}, 
#' to make it easier to simply install all of its required dependencies.
#'
#' Note that dependencies in inline code sections are not detected,
#' so these should be explicitly mentioned in a standalone code chunk to be captured.
#'
#' @author Aaron Lun
#' tmp <- tempfile(fileext=".Rmd")
#' write(file=tmp, "```{r}
#' A::a()
#' ```
#'
#' ```{r}
#' library(B)
#' require(C)
#' ```")
#'
#' scrapeDependencies(tempdir())
#'
#' @export
#' @importFrom CodeDepends readScript scriptInfo
#' @importFrom methods slot
scrapeDependencies <- function(dir, recursive=TRUE, pattern="\\.Rmd$") {
    all.rmds <- list.files(dir, recursive=recursive, full.names=TRUE, pattern=pattern)
    collated <- character(0)
    
    for (i in seq_along(all.rmds)) {
        txt <- readScript(all.rmds[i], type="Stangled")
        all.info <- scriptInfo(txt)
        collated <- c(collated, unlist(lapply(all.info, slot, "libraries")))
    }
    
    sort(unique(collated))
}
