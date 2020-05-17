#' Compile a Rmarkdown report
#'
#' Compile a report, typically so that \code{\link{extractCached}} calls work correctly in other reports.
#'
#' @param path String containing a path to an Rmarkdown file.
#'
#' @return
#' All files are (re)compiled to generate the corresponding \code{*_cache} directories.
#' \code{NULL} is invisibly returned.
#'
#' @details
#' Compilation is performed in an isolated session using \code{\link{r}} from the \pkg{callr} package.
#' This ensures that settings from one chapter do not affect the next chapter.
#'
#' If an error is encountered during compilation of any Rmarkdown file,
#' the standard output of \code{\link{render}} leading up to the error is printed out before the function exists.
#'
#' @author Aaron Lun
#' @seealso
#' \code{\link{extractCached}}, which calls this function.
#'
#' @export
#' @importFrom callr r
#' @importFrom rmarkdown render
compileChapter <- function(path) {
    logfile <- tempfile(fileext=".log")
    on.exit(unlink(logfile))

    E <- try(
        r(function(input) { rmarkdown::render(input) }, 
            args = list(input = path), 
            stdout=logfile, 
            stderr=logfile, 
            spinner=FALSE
        ),
        silent=TRUE
    )

    if (is(E, "try-error")) {
        message(sprintf("# %s\n", readLines(logfile)))
        stop(sprintf("failed to compile '%s'", path))
    }

    invisible(NULL)
}
