#' Get the local book cache
#'
#' Get the path to the cache directory in which the book will be built.
#' 
#' @param package String containing the name of the book package.
#' @param clear Logical scalar indicating whether old caches should be wiped.
#'
#' @details
#' The output path contains the version of the specified \code{package}.
#' If \code{clear=TRUE}, any caches corresponding to older versions of the package are destroyed.
#'
#' If the environment variable \code{REBOOK_<PKG>_CACHE} is set for some package name \code{PKG} (uppercased with dots replaced by underscores),
#' this is expected to contain the desired path and is returned directly. 
#' This functionality should only be used by experienced developers.
#'
#' @return String containing the path to the cache directory for this book package.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{configureBook}}, where this function is used in the Makefile.
#'
#' \code{\link{extractFromPackage}}, which populates the cache directory if this is not supplied. 
#'
#' @examples
#' \dontrun{
#' getBookCache('OSCA.workflows')
#' }
#'
#' @export
getBookCache <- function(package, clear=TRUE) {
    env.var <- paste0("REBOOK_", toupper(sub("\\.", "_", package)), "_CACHE")
    target <- Sys.getenv(env.var, NA)
    if (!is.na(target)) {
        return(target)
    }

    v <- packageVersion(package)

    # TODO: need some kind of BiocInstallUtils to manage caching and stuff. 
    pdir <- file.path(rappdirs::user_cache_dir("rebook", opinion=FALSE), package)

    others <- setdiff(list.dirs(pdir, recursive=FALSE, full.names=FALSE), as.character(v))
    if (length(others)) {
        unlink(file.path(pdir, others), recursive=TRUE)
    }

    file.path(pdir, v)
}
