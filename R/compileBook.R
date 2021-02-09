#' Compile the book
#'
#' Compile a \pkg{bookdown} book in a separate workspace and copy the compiled book to a different location.
#'
#' @param src String containing the path to the book Rmarkdown sources.
#' @param work String containing the path to the workspace used to compile the book.
#' @param final String containing the path to the final location for the compiled book's HTMLs.
#' @param input String containing the name of the file to pass to \code{\link[bookdown]{render_book}}.
#' @param ... Further arguments to pass to \code{\link[bookdown]{render_book}}.
#'
#' @return 
#' \code{work} is populated with the book sources and intermediate content (e.g., caches).
#' \code{final} is populated with the HTMLs.
#' A \code{NULL} is invisibly returned.
#' 
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{configureBook}}, where this function is called in the Makefile.
#' 
#' \code{\link{getBookCache}}, typically used to generate a good choice for \code{work}.
#' @export
compileBook <- function(src.dir, work.dir, final.dir, input="input.Rmd", ...) {
    .clean_dir_copy(src.dir, work.dir)

    RENDER <- function() {
        old <- setwd(work.dir)
        on.exit(setwd(old))
        bookdown::render_book(input, ...)
    }
    RENDER()

    # Promoting caches to the main directory for easier access.
    book.dir <- file.path(work.dir, "_bookdown_files")
    for (x in list.files(book.dir)) {
        file.rename(file.path(book.dir, x), file.path(work.dir, x))  
    }

    outdir <- .find_output_directory(work.dir)
    compiled <- file.path(work.dir, outdir)
    .clean_dir_copy(compiled, final.dir)
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
