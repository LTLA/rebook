#' Set up the book materials
#'
#' Convert Rmarkdown files to numbered chapters for \pkg{bookdown} compilation.
#'
#' @param chapters A character vector of paths to Rmarkdown files to be converted to chapters.
#' Alternatively, a list of such vectors.
#' @param dir String containing the location of the output files.
#' 
#' @return
#' Creates or updates the Rmarkdown files comprising the book's chapters in \code{dir}.
#' Returns \code{NULL} invisibly.
#'
#' @details
#' If \code{chapters} is a character vector with contents \code{c("first", "second", ...)}
#' chapters are created in the manner of \code{01-first.Rmd}, \code{02-second.Rmd} and so on.
#'
#' If \code{chapters} is a list of character vectors,
#' each vector is assumed to represent a \dQuote{Part} of the book and each file is a chapter in that Part.
#' In this case, chapters would look like \code{P1_W01.first.Rmd}, \code{P1_W02.second.Rmd} and so on.
#'
#' All existing files in \code{dir} that follow a similar naming format are destroyed,
#' to avoid any confusion between the old and new files.
#' 
#' @author Aaron Lun
#'
#' @export
setupChapters <- function(chapters, dir) {
    if (is.list(chapters)) {
        for (part in seq_along(chapters)) {
            files <- chapters[[i]]
            newfiles <- sprintf("P%i_W%02d.%s", part, seq_along(files), basename(files))
            unlink(list.files(dir, pattern=sprintf("^P%i_W[0-9]+\\..*\\.Rmd$", part), full.names=TRUE))
            file.copy(files, newfiles)
        }
    } else {
        newchapters <- sprintf("%02d-%s", part, seq_along(chapters), basename(chapters))
        unlink(list.chapters(dir, pattern=sprintf("^[0-9]+-.*\\.Rmd$", part), full.names=TRUE))
        file.copy(chapters, newchapters)
    }
    
    invisible(NULL)
}
