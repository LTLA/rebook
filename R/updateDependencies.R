#' Update the dependencies
#'
#' Update the \code{Imports} field in the book's \code{DESCRIPTION} file with the latest dependencies.
#'
#' @param dir String containing the path to the directory containing the book \code{DESCRIPTION} file.
#' @param extra Character vector of extra packages to be added to imports,
#' usually from packages that are in \code{Suggests} and thus not caught directly by \code{\link{scrapeDependencies}}.
#' @param ... Further arguments to pass to \code{\link{scrapeDependencies}}.
#'
#' @return The \code{DESCRIPTION} file in \code{dir} is updated.
#'
#' @author Aaron Lun
#' @export
updateDependencies <- function(dir, extra, ...) {
    scraped <- scrapeDependencies(dir, ...)
    path <- file.path(dir, "DESCRIPTION")
    collected <- read.dcf(path, keep.white=TRUE)
    collected[,"Imports"] <- paste(c(scraped, extra), collapse=",\n  ")
    write.dcf(collected, file=path, width=2000, keep.white="Imports") 
    invisible(NULL)
}
