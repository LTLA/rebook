#' Extract cached objects from package's Rmarkdown files
#'
#' Extract and compile Rmarkdown files from a \dQuote{donor} package's installation directory,
#' extracting cached objects from the subsequent \pkg{knitr} cache.
#'
#' @param path String containing the path to the donor Rmarkdown file, relative to \code{inst.dir}.
#' @param ...,envir Further arguments to pass to \code{\link{extractCached}}.
#' @param package String containing the name of the donor package.
#' @param inst.dir String containing the name or relative path of the subdirectory in the donor package's installation directory that contains all the Rmarkdown files.
#' @param work.dir String containing the path to a subdirectory to hold the cache of the donor Rmarkdown file.
#'
#' @details
#' This function assumes that all potential donor Rmarkdown files for \code{package} are present in \code{inst.dir}.
#' It copies the contents of {inst.dir} into \code{work.dir} and calls \code{\link{extractCached}} on the \code{path} inside.
#' The desired objects are then extracted from the subsequent \pkg{knitr} cache.
#' 
#' We perform a copy to respect any uses of \code{extractCached} inside the donor reports themselves (e.g., to reference other reports in the same directory).
#' Note that we do not assume that the cache itself is present in the installation directory.
#' In theory, doing so would avoid the need for a re-compilation but would also increase the disk usage of the donor package, so we favor the recompilation approach.
#'
#' The \code{work.dir} is usually set to a local directory to enable greater re-use of the cache across calls and R sessions.
#' To avoid using stale objects, it will be flushed whenever there is a version bump to \code{package}.
#'
#' @return Depends on the arguments passed to \code{...}; see \code{\link{extractCached}}.
#' 
#' @author Aaron Lun
#'
#' @examples
#' tmp <- tempfile()
#' extractFromPackage("test.Rmd", chunk="ghidorah-1964", inst.dir="example",
#'     objects="godzilla", package="rebook", work.dir=tmp)
#'
#' list.files(tmp)
#' godzilla
#'
#' @export
#' @importFrom utils packageVersion
extractFromPackage <- function(path, ..., package, envir = topenv(parent.frame()), inst.dir="book", work.dir="_rebook_cache") {
    pkg.dir <- file.path(work.dir, package)
    version <- as.character(packageVersion(package))
    ver.dir <- file.path(pkg.dir, version)

    if (!file.exists(ver.dir)) {
        unlink(pkg.dir, recursive=TRUE)
        dir.create(pkg.dir, recursive=TRUE)
        file.copy(system.file(inst.dir, package=package, mustWork=TRUE), pkg.dir, recursive=TRUE)
        file.rename(file.path(pkg.dir, basename(inst.dir)), ver.dir)
    }

    link.id <- rmd2id(file.path(ver.dir, path))
    link.text <- link(link.id, package, error=FALSE)
    if (is.null(link.text)) {
        link.text <- NA
    }

    force(envir)
    extractCached(file.path(ver.dir, path), ..., envir=envir, link.text=link.text)
}