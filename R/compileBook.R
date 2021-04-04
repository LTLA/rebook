#' Compile the book
#'
#' Copy a \pkg{bookdown} book to a separate workspace prior to compilation,
#' and then copy the compiled book to a final location.
#'
#' @param src.dir String containing the path to the book Rmarkdown sources.
#' @param work.dir String containing the path to the versioned cache directory used to compile the book, see the \pkg{dir.expiry} package for details.
#' @param final.dir String containing the path to the final location for the compiled book's HTMLs.
#' @param desc String containing the path to a \code{DESCRIPTION} file to copy into \code{work.dir}.
#' Typically used when the book is to inherit the \code{DESCRIPTION} of the enclosing package.
#' @param handle The lock handle returned by \code{preCompileBook}.
#'
#' @return 
#' For \code{preCompileBook}, \code{work} is populated with the book sources and intermediate content (e.g., caches).
#' A lock handle is returned.
#' 
#' For \code{postCompileBook}, \code{final} is populated with the HTMLs.
#' Cache directories are moved out of \code{_bookdown_files} into their original location.
#'
#' In both cases, a \code{NULL} is invisibly returned.
#' 
#' @details
#' These two functions should bracket a \code{\link[bookdown]{render_book}} call.
#' We do not make these into a single function as calling \code{render_book} inside another function inside a package does not interact properly with some imports.
#' The offending example is that of \code{cbind}, which fails to be converted into an S4 generic (this would normally happen when \pkg{BiocGenerics} gets attached).
#'
#' \code{preCompileBook} may take some time as it will compile all chapters via \code{\link{compileChapter}}.
#' It does so by locking and unlocking each chapter as it is compiled, thus avoiding problems with concurrent attempts to compile the same chapter via \code{\link{extractFromPackage}}.
#' (Concurrent compilation of different chapters is still supported and allows for parallel package builds.)
#' The actual compilation of the book with \pkg{bookdown} will simply re-use these caches for efficiency.
#'
#' After compilation of the individual chapters, \code{preCompileBook} will lock the entire \code{work.dir}.
#' This ensures that \pkg{bookdown}'s directory shuffling does not break concurrent processes using the \pkg{knitr} cache directories. 
#' The lock can be released by passing the returned handle to \code{handle} in \code{postCompileBook}. 
#' 
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{configureBook}}, where this function is called in the Makefile.
#' 
#' \code{\link{bookCache}}, the default choice for \code{work.dir}.
#' @name compileBook
NULL

#' @export
#' @rdname compileBook
#' @importFrom dir.expiry lockDirectory
preCompileBook <- function(src.dir, work.dir, desc=NULL) {
    .copy_book_sources(src.dir, work.dir, desc)
    .precompile_book(work.dir)
    lockDirectory(work.dir)
}

#' @importFrom dir.expiry lockDirectory unlockDirectory touchDirectory
.copy_book_sources <- function(src.dir, work.dir, desc) {
    lck <- lockDirectory(work.dir)
    on.exit(unlockDirectory(lck, limit=bookCacheExpiry()))

    .clean_dir_copy(src.dir, work.dir)
    if (!is.null(desc)) {
        file.copy(desc, work.dir)
    }

    touchDirectory(work.dir)
}

#' @importFrom dir.expiry lockDirectory unlockDirectory
.precompile_book <- function(work.dir) {
    # This can be shared as the directory exists and won't be deleted here.
    # We only need to apply exclusive locks on the individual chapters.
    lck <- lockDirectory(work.dir, exclusive=FALSE)
    on.exit(unlockDirectory(lck, clear=FALSE))

    # Compiling each report, locking and unlocking as we go.
    all.rmds <- .get_book_chapters(work.dir)
    for (rmd in all.rmds) {
        .locked_compile_chapter(file.path(work.dir, rmd))
    }
}

.locked_compile_chapter <- function(path) {
    lck <- .lock_report(path)
    on.exit(.unlock_report(lck))
    compileChapter(path)
}

#' @export
#' @rdname compileBook
#' @importFrom dir.expiry unlockDirectory
postCompileBook <- function(work.dir, final.dir, handle=NULL) {
    # Promoting caches to the main directory for easier access.
    book.dir <- file.path(work.dir, "_bookdown_files")
    for (x in list.files(book.dir)) {
        file.rename(file.path(book.dir, x), file.path(work.dir, x))  
    }

    outdir <- .find_output_directory(work.dir)
    compiled <- file.path(work.dir, outdir)
    .clean_dir_copy(compiled, final.dir)

    if (!is.null(handle)) {
        unlockDirectory(handle, clear=FALSE)
    }

    invisible(NULL)
}

.clean_dir_copy <- function(from, to) {
    if (file.exists(to)) {
        unlink(to, recursive=TRUE)
    }
    target <- tempfile(tmpdir=dirname(to))
    dir.create(target, recursive=TRUE)
    if (!file.copy(from, target, recursive=TRUE) || !file.rename(file.path(target, basename(from)), to)) { # stupid copying of directories.
        stop("failed to copy '", from, "' to '", to, "'")
    }
    unlink(target, recursive=TRUE)
}
