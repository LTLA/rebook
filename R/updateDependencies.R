#' Update the dependencies
#'
#' Update the book package's DESCRIPTION file with the latest dependencies.
#'
#' @param dir String containing the path to the directory containing the book.
#' @param path String containing the path to the DESCRIPTION file.
#' @param extra Character vector of extra packages to be added to imports,
#' usually from packages that are in \code{Suggests} and thus not caught directly by \code{\link{scrapeDependencies}}.
#' @param indent Integer scalar specifying the size of the indent to use when listing packages.
#' @param field String specifying the dependency field to store the packages in.
#' Defaults to \code{"Suggests"} by convention.
#' @param ... Further arguments to pass to \code{\link{scrapeDependencies}}.
#'
#' @details
#' The book \code{DESCRIPTION} is useful for quick installation of all packages required across all chapters.
#' For example, it is used by \url{https://github.com/LTLA/TrojanBookBuilder} to populate a trojan package's dependencies,
#' ensuring that all packages are available when the book itself is compiled.
#'
#' @return The specified \code{field} in the \code{DESCRIPTION} file in \code{dir} is updated.
#' \code{NULL} is invisibly returned.
#'
#' @examples
#' dir <- tempfile()
#' dir.create(dir)
#'
#' write(file=file.path(dir, "DESCRIPTION"), 
#' "Package: son.of.godzilla 
#' Version: 0.0.1
#' Description: Like godzilla, but smaller.")
#'
#' tmp <- file.path(dir, "alpha.Rmd")
#' write(file=tmp, "```{r, echo=FALSE, results='asis'}
#' rebook::chapterPreamble()
#' ```
#'
#' ```{r}
#' A::func
#' library(C)
#' ```")
#' 
#' tmp <- file.path(dir, "bravo.Rmd")
#' write(file=tmp, "```{r, echo=FALSE, results='asis'}
#' rebook::chapterPreamble()
#' ```
#'
#' ```{r}
#' require(D)
#' B::more
#' ```")
#' 
#' updateDependencies(dir)
#' cat(readLines(file.path(dir, "DESCRIPTION")), sep="\n")
#'
#' @author Aaron Lun
#' @export
updateDependencies <- function(dir=".", path=file.path(dir, "DESCRIPTION"), extra=NULL, indent=4, field="Depends", ...) {
    scraped <- scrapeDependencies(dir, ...)

    # Double read is deliberate to ensure that whitespace is preserved.
    collected <- read.dcf(path)
    collected <- read.dcf(path, keep.white=colnames(collected))

    to.add <- paste(sort(union(scraped, extra)), collapse=paste0(",\n", strrep(" ", indent)))
    if (field %in% colnames(collected)) {
        collected[,field] <- to.add
    } else {
        collected <- cbind(collected, PLACEHOLDER=to.add)
        colnames(collected)[ncol(collected)] <- field
    }

    write.dcf(collected, file=path, width=2000, keep.white=colnames(collected))
    invisible(NULL)
}
