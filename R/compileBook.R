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
#'
#' @return 
#' For \code{preCompileBook}, \code{work} is populated with the book sources and intermediate content (e.g., caches).
#' 
#' For \code{postCompileBook}, \code{final} is populated with the HTMLs.
#'
#' In both cases, a \code{NULL} is invisibly returned.
#' 
#' @details
#' These two functions should bracket a \code{\link[bookdown]{render_book}} call.
#' We do not make these into a single function as calling \code{render_book} inside another function inside a package does not interact properly with some imports.
#' The offending example is that of \code{cbind}, which fails to be converted into an S4 generic (this would normally happen when \pkg{BiocGenerics} gets attached).
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
    .clean_dir_copy(src.dir, work.dir)
    if (!is.null(desc)) {
        file.copy(desc, work.dir)
    }
    invisible(NULL)
}

#' @export
#' @rdname compileBook
postCompileBook <- function(work.dir, final.dir) {
    # Promoting caches to the main directory for easier access.
    book.dir <- file.path(work.dir, "_bookdown_files")
    for (x in list.files(book.dir)) {
        file.rename(file.path(book.dir, x), file.path(work.dir, x))  
    }

    outdir <- .find_output_directory(work.dir)
    compiled <- file.path(work.dir, outdir)
    .clean_dir_copy(compiled, final.dir)
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

.find_output_directory <- function(dir) {
    # We need to figure out the output_dir but I can't be bothered to require
    # yaml, so we'll just do is the old-fashioned way.
    outdir <- "_book"
    all.lines <- readLines(file.path(dir, "_bookdown.yml"))
    outdir.line <- grepl("^output_dir:", all.lines)
    if (any(outdir.line)) {
        outdir <- sub("^output_dir: +", "", all.lines[outdir.line][1])
        outdir <- sub(" +$", "", outdir)
        outdir <- gsub("\"", "", outdir)
    }
    outdir
}
