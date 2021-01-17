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
    configdir <- file.path("inst", "rebook")
    dir.create(configdir, showWarnings=FALSE)

    # Filling up the configurations.
    df <- scrapeReferences(hostdir, input=input)
    write.csv(file=file.path(configdir, "references.csv"), df, quote=FALSE, row.names=FALSE)

    prefix.file <- file.path(configdir, "prefix.csv")
    if (!is.null(prefix)) {
        write(file=prefix.file, prefix)
    } else {
        unlink(prefix.file)
    }

    dir.create("vignettes")

    # Spawning a Makefile in the vignettes directory. We need to figure
    # out the output_dir but I can't be bothered to require yaml, so 
    # we'll just do is the old-fashioned way.
    outdir <- "_book"
    all.lines <- readLines(file.path(hostdir, "_bookdown.yml"))
    outdir.line <- grepl("^output_dir:", all.lines)
    if (any(outdir.line)) {
        outdir <- sub("^output_dir: +", "", all.lines[outdir.line][1])
        outdir <- sub(" +$", "", outdir)
    }

    write(sprintf("all: compiled

compiled: 
	rm -rf book/ && cp -r ../inst/book book
	cd book && \"${R_HOME}/bin/R\" -e \"bookdown::render_book('index.Rmd')\"
	mv book/%s inst/doc
	rm -rf book/", outdir),
        file="vignettes/Makefile")

    # Spawning a stub that just redirects to the deployed book.
    # This requires us to figure out the name of our current package.     
    pkg.name <- read.dcf("DESCRIPTION")[,"Package"]

    write(paste0('---
vignette: >
  %\\VignetteIndexEntry{Link to book}
  %\\VignetteEngine{knitr::rmarkdown}
  %\\VignetteEncoding{UTF-8}
---    

```{r, echo=FALSE}
# Some compileable code, apparently needed for the engine
# to recognize this as Rmarkdown.
link <- BiocStyle::Biocbook("', pkg.name, '", label="link")
URL <- sub(".*\\\\((.+))", "\\\\1", link)
```

<meta charset="utf-8">
<meta http-equiv="refresh" content="`r sprintf("0; URL=%s", URL)`">
<link rel="canonical" href="`r URL`">'),
        file="vignettes/stub.Rmd")
}
