#' Helper configuration function for books
#'
#' Helper function to run in the \code{configure} of Bioconductor books,
#' to set up for \code{\link{link}}ing from other books.
#'
#' @param prefix String containing the prefix to be used when \code{\link{link}}ing from other books.
#' @param input See \code{\link{scrapeReferences}}.
#'
#' @return \code{NULL}, invisibly.
#' A \code{"references.csv"} file is created in the \code{inst/rebook} directory.
#' If \code{prefix} is specified, a \code{"prefix.csv"} file is also created.
#' 
#' @details 
#' This assumes that the \pkg{bookdown}-formatted book is located at \code{inst/book}.
#' 
#' @author Aaron Lun
#' 
#' @seealso
#' \code{\link{scrapeReferences}}, which is called by this function to create the reference table.
#'
#' \code{\link{link}}, which is used by other books to link to the \code{configure}d book.
#'
#' @export
#' @importFrom utils write.csv
configureBook <- function(prefix=NULL, input="index.Rmd") {
    df <- scrapeReferences(file.path("inst", "book"), input=input)
    outdir <- file.path("inst", "rebook")
    dir.create(outdir, showWarnings=FALSE)
    write.csv(file=file.path(outdir, "references.csv"), df, col.names=TRUE, quote=FALSE, row.names=FALSE)
    if (!is.null(prefix)) {
        write(file=file.path(outdir, "prefix.csv"), prefix)
    }
}
