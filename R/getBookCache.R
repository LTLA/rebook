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
#' getBookCache()
#'
#' @export
getBookCache <- function(package, clear=TRUE) {
    v <- packageVersion(package)
    # TODO: need some kind of BiocInstallUtils to manage caching and stuff. 
    pdir <- file.path(rappdirs::user_cache_dir("rebook", opinion=FALSE), package)
    vdir <- file.path(pdir, v)
    if (file.exists(pdir) && !file.exists(vdir)) {
        unlink(pdir, recursive=TRUE)    
    }
    vdir
}
