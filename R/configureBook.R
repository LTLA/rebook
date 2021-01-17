#' Helper configuration function for books
#'
#' Helper function to run at the top-level directory of Bioconductor book packages,
#' to prepare for book compilation and to set up install-time resources for \code{\link{link}}ing from other books.
#'
#' @param prefix Optional string containing the prefix to be used when \code{\link{link}}ing from other books.
#' @param input See \code{\link{scrapeReferences}}.
#'
#' @return A number of files are created in the package directory.
#' \itemize{
#' \item A \code{"references.csv"} file is created in the \code{inst/rebook} directory,
#' containing the table of references from \code{\link{scrapeReferences}}.
#' \item If \code{prefix} is specified, a \code{"prefix.csv"} file is also created in \code{inst/rebook}.
#' This contains the preferred prefix of the book.
#' \item If \code{skip=FALSE}, a Makefile is created in \code{vignettes/} that triggers book compilation.
#' \item A stub vignette at \code{vignettes/stub.Rmd} is created that redirects to the deployed book location.
#' }
#' 
#' @details 
#' This function assumes that the \pkg{bookdown}-formatted book is located at \code{inst/book}.
#'
#' @author Aaron Lun
#' 
#' @seealso
#' \code{\link{scrapeReferences}}, which is called by this function to create the reference table.
#'
#' \code{\link{link}}, which is used by other books to link to the configured book.
#'
#' @export
#' @importFrom utils write.csv
configureBook <- function(prefix=NULL, input="index.Rmd") {
    hostdir <- file.path('inst', 'book')
    outdir <- file.path("inst", "rebook")
    dir.create(outdir, showWarnings=FALSE)

    # Filling up the configurations.
    df <- scrapeReferences(hostdir, input=input)
    write.csv(file=file.path(outdir, "references.csv"), df, quote=FALSE, row.names=FALSE)
    if (!is.null(prefix)) {
        write(file=file.path(outdir, "prefix.csv"), prefix)
    }

    # Making a copy in the vignettes/ directory.
    dir.create("vignettes", showWarnings=FALSE)
    vigdir <- file.path("vignettes", "book")
    if (file.exists(vigdir)) {
        unlink(vigdir, force=TRUE, recursive=TRUE)
    }
    file.copy(hostdir, "vignettes", recursive=TRUE)

    # Spawning a Makefile. Note the TEMPORARY stuff is just a hack for now,
    # until Bioconductor pulls from inst/docs.
    write("all: compiled

compiled: 
	cd book && \"${R_HOME}/bin/R\" -e \"bookdown::render_book('index.Rmd')\"
	mv book/docs TEMPORARY
	rm -rf book/ && mkdir book/ && mv TEMPORARY book/docs
	mkdir -p ../inst && cp -r book/docs ../inst/", 
        file="vignettes/Makefile")

    # Spawning a stub that just redirects to the deployed book.
    write('---
vignette: >
  %\\VignetteIndexEntry{Link to book}
  %\\VignetteEngine{knitr::rmarkdown}
  %\\VignetteEncoding{UTF-8}
---    

```{r, echo=FALSE}
# Some compileable code, apparently needed for the engine
# to recognize this as Rmarkdown.
a <- 1 + 1
```

<meta charset="utf-8">
<meta http-equiv="refresh" content="`r sprintf("0; URL=http://bioconductor.org/books/%s/OSCA/", BiocManager::version())`">
<link rel="canonical" href="`r sprintf("http://bioconductor.org/books/%s/OSCA/", BiocManager::version())`">',
        file="vignettes/stub.Rmd")
}
