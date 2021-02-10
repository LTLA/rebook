#' Extract cached objects from package's Rmarkdown files
#'
#' Extract and compile Rmarkdown files from a \dQuote{donor} package's installation directory,
#' extracting cached objects from the subsequent \pkg{knitr} cache.
#'
#' @param rmd.name String containing the path to the donor Rmarkdown file, relative to \code{work}.
#' @param ...,envir Further arguments to pass to \code{\link{extractCached}}.
#' @param package String containing the name of the donor package.
#' @param src.name String containing the name or relative path of the subdirectory in the donor package's installation directory that contains all the Rmarkdown files.
#' @param work.dir String containing the path to a subdirectory to hold the cache of the donor Rmarkdown file.
#'
#' @details
#' This function assumes that all potential donor Rmarkdown files for \code{package} are present in the directory \code{src.name}.
#' It copies the contents of {src.name} into \code{work} and calls \code{\link{extractCached}} on the \code{rmd.name} inside.
#' The desired objects are then extracted from the subsequent \pkg{knitr} cache.
#' 
#' We perform a copy to respect any uses of \code{extractCached} inside the donor reports themselves (e.g., to reference other reports in the same directory).
#' Note that we do not assume that the cache itself is present in the installation directory.
#' In theory, doing so would avoid the need for a re-compilation but would also increase the disk usage of the donor package, so we favor the recompilation approach.
#'
#' The \code{work} directory should be set to a persistent cache to enable greater re-use of the cache across calls and R sessions.
#' Indeed, the default here is the same as that used by \code{\link{compileBook}}, so we can avoid recopmilation if the donor book has already been compiled via the latter function.
#'
#' @return Depends on the arguments passed to \code{...}; see \code{\link{extractCached}}.
#' 
#' @author Aaron Lun
#'
#' @examples
#' tmp <- tempfile()
#' extractFromPackage("test.Rmd", chunk="ghidorah-1964", src.name="example",
#'     objects="godzilla", package="rebook", work.dir=tmp)
#'
#' list.files(tmp)
#' godzilla
#'
#' @export
#' @importFrom utils packageVersion
extractFromPackage <- function(rmd.name, ..., package, envir = topenv(parent.frame()), src.name="book", work.dir=getBookCache(package)) {
    if (!file.exists(work.dir)) {
        src <- system.file(src.name, package=package, mustWork=TRUE)
        .clean_dir_copy(src, work.dir)
    }

    target <- file.path(work.dir, rmd.name)
    link.id <- rmd2id(target)
    link.text <- link(link.id, package, error=FALSE)
    if (is.null(link.text)) {
        link.text <- NA
    }

    force(envir)
    extractCached(target, ..., envir=envir, link.text=link.text)
}
