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
#' It copies the contents of {src.name} into \code{work.dir} and calls \code{\link{extractCached}} on the \code{rmd.name} inside.
#' The desired objects are then extracted from the subsequent \pkg{knitr} cache.
#' 
#' The \code{work.dir} directory should be set to a persistent cache to enable greater re-use of the cache across calls and R sessions.
#' Indeed, the default here is the same as that used by \code{\link{preCompileBook}}, so we can avoid recopmilation if the donor book has already been compiled via the latter function.
#' This function will respect any global locks imposed by other functions in the process of performing the copy (or other rearrangements).
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
    # Respecting global locks on the directory. Do NOT abbreviate the
    # 'work.dir' existence check into a common variable, as we want to check it
    # again when the lock is acquired, just in case the directory was created
    # in the meantime.
    lck <- .lock_dir(work.dir, exclusive=!file.exists(work.dir))
    on.exit(unlock(lck))

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
