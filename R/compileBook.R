#' Compile the book
#'
#' Copy a \pkg{bookdown} book to a separate workspace prior to compilation,
#' and then copy the compiled book to a final location.
#'
#' @param src.dir String containing the path to the book Rmarkdown sources.
#' @param work.dir String containing the path to the workspace used to compile the book.
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
#' \code{preCompileBook} may take some time as it will compile all chapters via \code{\link{compileChapters}}.
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
#' \code{\link{getBookCache}}, typically used to generate a good choice for \code{work}.
#' @name compileBook
NULL

#' @export
#' @rdname compileBook
preCompileBook <- function(src.dir, work.dir, desc=NULL) {
    # Locking while we copy everything over.
    lck <- .lock_dir(work.dir)
    on.exit(.unlock_dir(lck))

    .clean_dir_copy(src.dir, work.dir)
    if (!is.null(desc)) {
        file.copy(desc, work.dir)
    }

    # Release the lock now, as everything has been moved to the right place.
    .unlock_dir(lck) 

    # Compiling each report, locking and unlocking as we go.
    all.rmds <- .get_book_chapters(work.dir)
    for (rmd in all.rmds) {
        .locked_compile_chapter(file.path(work.dir, rmd))
    }

    # Returning another lock to protect the bookdown call.
    .lock_dir(work.dir)
}

.locked_compile_chapter <- function(path) {
    lck <- .lock_report(path)
    on.exit(.unlock_report(lck))
    compileChapter(path)
}

#' @export
#' @rdname compileBook
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
        .unlock_dir(handle)
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
