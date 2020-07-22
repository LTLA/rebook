#' Create a compilation Makefile
#'
#' Create a Makefile for compiling individual chapters, potentially in parallel.
#' This respects the dependencies between chapter as detected by \code{\link{buildChapterGraph}}.
#'
#' @inheritParams buildChapterGraph
#' @param fname String containing the name of the output Makefile.
#'
#' @return A Makefile is created in \code{dir} with the name \code{fname}
#' and a \code{NULL} is invisibly returned.
#'
#' @details
#' This uses the markdown output file as an indicator of successful \code{\link{knit}}ting of a chapter.
#' Caches are left in the current working directory after the compilation of each report.
#' It is assumed that \pkg{bookdown}'s \code{render_book} is smart enough to find and use these caches.
#' 
#' @author Aaron Lun
#' @export
createMakefile <- function(dir=".", pattern="\\.Rmd$", ..., fname="Makefile") {
    g <- buildChapterGraph(dir, pattern=pattern, ...) 
    reports <- names(igraph::topo_sort(g))

    path <- file.path(dir, fname)
    to.md <- sub(pattern, ".md", reports)
    write(file=path, paste(c("all:", to.md), collapse=" "))

    for (v in seq_along(reports)) {
        parents <- names(igraph::neighbors(g, reports[v], mode="in"))

        write(sprintf("\n%s: %s\n\tR -e \"knitr::knit('%s')\"", 
            to.md[v], 
            paste(c(reports[v], to.md[match(parents, reports)]), collapse=" "),
            reports[v]),
            file=path, append=TRUE)
    }

    invisible(NULL)
}
