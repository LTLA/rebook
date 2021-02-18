#' Helper configuration function for books
#'
#' Helper function to run at the top-level directory of Bioconductor book packages,
#' to prepare for book compilation and to set up install-time resources for \code{\link{link}}ing from other books.
#'
#' @param prefix Optional string containing the prefix to be used when \code{\link{link}}ing from other books.
#' @param input Name of the index file for the book, see \code{\link{scrapeReferences}}.
#' @param redirect Optional name of the file containing redirection information, to be passed to \code{\link{createRedirects}}.
#'
#' @return A number of files are created in the package directory.
#' \itemize{
#' \item A \code{"references.csv"} file is created in the \code{inst/rebook} directory,
#' containing the table of references from \code{\link{scrapeReferences}}.
#' \item If \code{prefix} is specified, a \code{"prefix.csv"} file is also created in \code{inst/rebook}.
#' This contains the preferred prefix of the book.
#' \item A Makefile is created in \code{vignettes/} that triggers book compilation.
#' This will also generate HTMLs for redirection via \code{\link{createRedirects}} if \code{redirect} is provided.
#' \item A stub vignette at \code{vignettes/stub.Rmd} is created that redirects to the deployed book location.
#' }
#' 
#' @details 
#' This function assumes that the \pkg{bookdown}-formatted book is located at \code{inst/book} inside the package.
#' \code{input} is interpreted relative to this location, e.g., if \code{input="index.Rmd"}, the file should be located at \code{inst/book/index.Rmd}.
#'
#' Similarly, \code{redirect} is provided, the file should already be present in \code{vignettes/}.
#' For example, if \code{redirect="redirect.txt"}, the file should be located at \code{vignettes/redirect.txt}.
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
configureBook <- function(prefix=NULL, input="index.Rmd", redirect=NULL) {
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

    dir.create("vignettes", showWarnings=FALSE)
    pkg.name <- gsub("\\s", "", read.dcf("DESCRIPTION")[,"Package"])

    make.path <- "vignettes/Makefile"
    cmds <- .makeCommandString(
        src.dir=file.path('..', hostdir), 
        work.expr=sprintf("rebook::getBookCache('%s')", pkg.name), 
        final.dir='../inst/doc/book',
        desc.expr="'../DESCRIPTION'",
        input=input
    )

    write(sprintf("all: compiled

compiled: 
	\"${R_HOME}/bin/R\" -e \"%s\"", paste(cmds, collapse="; ")),
        file=make.path)

    if (!is.null(redirect)) {
        write(file=make.path, sprintf("\t\"${R_HOME}/bin/R\" -e \"rebook::createRedirects(file='%s')\"", redirect), append=TRUE)
    }

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

.makeCommandString <- function(src.dir, work.expr, final.dir, input='input.Rmd', desc.expr='NULL') {
    c(
        sprintf("work.dir <- %s", work.expr),
        sprintf("handle <- rebook::preCompileBook('%s', work.dir=work.dir, desc=%s)", src.dir, desc.expr),
        "old.dir <- setwd(work.dir)",
        sprintf("bookdown::render_book('%s')", input),
        "setwd(old.dir)",
        sprintf("rebook::postCompileBook(work.dir=work.dir, final.dir='%s', handle=handle)", final.dir)
    )
}
