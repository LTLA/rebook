#' Scrape dependencies
#' 
#' Scrape Rmarkdown reports in the book for all required dependencies.
#'
#' @param dir String containing the path to the directory containing Rmarkdown reports.
#' This is searched recursively for all files ending in \code{".Rmd"}.
#'
#' @return Character vector of required packages.
#'
#' @details
#' The output of this should be added to the \code{Suggests} field of the book's \code{DESCRIPTION}, 
#' to make it easier to simply install all of its required dependencies.
#'
#' @author Aaron Lun
#'
#' @export
scrapeDependencies <- function(dir) {
    all.rmds <- list.files(dir, recursive=TRUE, full.names=TRUE, pattern="\\.Rmd$")
    collated <- character(0)
    
    for (i in seq_along(all.rmds)) {
        txt <- readLines(all.rmds[i])
        collated <- c(collated, 
            .extract_pkgname("library\\(([^\\)]+)\\)", txt),
            .extract_pkgname("require\\(([^\\)]+)\\)", txt),
            .extract_pkgname("([^\\( -,=+/:`]+)::", txt))
    }
    
    sort(unique(collated))
}

.extract_pkgname <- function(pattern, txt) {
    keep <- grep(pattern, txt)
    matching <- regmatches(txt, regexpr(pattern, txt))
    sub(pattern, "\\1", matching) 
}

