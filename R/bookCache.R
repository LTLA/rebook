#' Get the local book cache
#'
#' Get the path to the cache directory in which the book will be built.
#' 
#' @param package String containing the name of the book package.
#'
#' @details
#' For \code{bookCache}, the last elements of the output path are the package and version, consistent with the expectations of \pkg{dir.expiry} functions.
#' This path is located in a directory determined by \pkg{rappdirs}.
#' If the environment variable \code{REBOOK_CACHE} is set, it is used to obtain the root of the path instead.
#'
#' If the environment variable \code{REBOOK_CACHE_EXPIRY} is set, it is coerced into an integer and returned by \code{bookCacheExpiry}.
#' This allows users to tune the expiry interval for older cached books.
#'
#' @return 
#' For \code{bookCache}, a string containing the path to the cache directory for this book package.
#'
#' For \code{bookCacheExpiry}, an integer specifying the maximum number of days from last access for a book cache.
#' Any unaccessed caches are subject to deletion by various \pkg{rebook} functions.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{configureBook}}, where this function is used in the Makefile.
#'
#' \code{\link{extractFromPackage}}, which populates the cache directory if this is not supplied. 
#'
#' @examples
#' bookCache('OSCA.workflows')
#' bookCacheExpiry()
#'
#' @export
bookCache <- function(package) {
    target <- Sys.getenv("REBOOK_CACHE", NA)
    if (is.na(target)) {
        target <- rappdirs::user_cache_dir("rebook", opinion=FALSE)
    }
    v <- packageVersion(package)
    file.path(target, package, v)
}

#' @export
#' @rdname bookCache
bookCacheExpiry <- function() {
    target <- Sys.getenv("REBOOK_CACHE_EXPIRY", 7)
    as.integer(target)
}
