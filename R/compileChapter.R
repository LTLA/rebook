#' Compile a Rmarkdown file
#'
#' Compile a Rmarkdown file - typically a chapter of a book - 
#' so that \code{\link{extractCached}} calls work correctly in other chapters.
#'
#' @param path String containing a path to an Rmarkdown file.
#' @param cache Logical scalar indicating whether the compilation should be cached.
#'
#' @return
#' The specified file is (re)compiled to generate the corresponding \code{*_cache} directories.
#' \code{NULL} is invisibly returned.
#'
#' @details
#' Compilation is performed in a completely fresh R session,
#' to ensure that objects, globals and loaded packages from one chapter do not affect the next chapter.
#'
#' If an error is encountered during compilation of any Rmarkdown file,
#' the standard output of \code{\link{render}} leading up to the error is printed out before the function exists.
#'
#' @author Aaron Lun
#' @seealso
#' \code{\link{extractCached}}, which calls this function.
#'
#' @examples
#' tmp <- tempfile(fileext=".Rmd")
#' write(file=tmp, "```{r, echo=FALSE, results='asis'}
#' rebook::chapterPreamble()
#' ```
#'
#' ```{r}
#' rodan <- 1
#' ```")
#' 
#' compileChapter(tmp)
#' 
#' file.exists(sub(".Rmd$", ".html", tmp)) # output HTML exists.
#' file.exists(sub(".Rmd$", "_cache", tmp)) # output cache exists.
#' exists("rodan") # FALSE
#' @export
#' @importFrom rmarkdown render
#' @importFrom methods is
compileChapter <- function(path, cache=TRUE) {
    logfile <- tempfile(fileext=".log")
    on.exit(unlink(logfile))

    # There's something strange with callr's environment, as it keeps on giving me
    # "long vectors not supported yet" errors with knitr caching when a plain R session is fine.
    # So given that I don't need the return value, I'll just spin up a new session.
    cmd <- sprintf("knitr::opts_chunk$set(cache=%s); rmarkdown::render(commandArgs(TRUE))", deparse(cache))
    status <- system2(R.home("bin/R"), c("--no-save", "--slave", "-e", shQuote(cmd), "--args", shQuote(path)), stdout=logfile, stderr=logfile)

    if (status!=0) {
        message(sprintf("# %s\n", readLines(logfile)))
        stop(sprintf("failed to compile '%s'", path))
    }

    invisible(NULL)
}
