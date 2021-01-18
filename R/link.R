#' Create a link to a different book
#'
#' From another Rmarkdown file, create a link to a section or figure of a \pkg{rebook}-configured book.
#'
#' @param id String containing an identifier for a section or figure.
#' @param package String containing the name of the package containing the target book.
#' @param type String containing the type of the link, e.g., \code{"Section"} or \code{"Figure"}, to be added to the link text.
#' This is automatically determined if not provided.
#' If \code{NA}, the type is not added to the link text.
#' @param prefix String specifying the prefix to use on \code{type}.
#' This is automatically determined from \code{package}'s chosen prefix or, if that is not provided, using the package name itself.
#' If \code{NA}, no prefix is added.
#' Only used if \code{type} is not \code{NA}. 
#' @param df A data.frame containing all links for \code{package}.
#' Only used for testing.
#' @param error Logical scalar indicating whether an error should be raised if the link cannot be found.
#' 
#' @details
#' We expect that the target book is set up as a Bioconductor package with a \code{configure} file that runs \code{\link{configureBook}}.
#' This function will then retrieve install-time information from that package to create necessary hyperlinks to the Bioconductor-hosted book content.
#'
#' @return String containing a markdown-formatted link to the relevant part of the target book.
#' If the link cannot be constructed and \code{error=FALSE}, a \code{NULL} is instead returned.
#'
#' @author Aaron Lun
#' @examples
#' # Only using 'df=' here because 'testpackage' doesn't actually exist.
#' link("fig:xxx", package="testpackage", 
#'     df=data.frame(id='fig:xxx', file='whee.html', text='3.1'))
#'
#' link("fig:xxx", package="testpackage", type=NA,
#'     df=data.frame(id='fig:xxx', file='whee.html', text='3.1'))
#'
#' link("fig:xxx", package="testpackage", prefix=NA,
#'     df=data.frame(id='fig:xxx', file='whee.html', text='3.1'))
#' @seealso
#' \code{\link{configureBook}}, which should be run by the authors of \code{package}.
#'
#' \code{\link{scrapeReferences}}, to generate a \code{df} for testing.
#'
#' @export
#' @importFrom BiocStyle Biocbook
link <- function(id, package, type=NULL, prefix=NULL, df=NULL, error=TRUE) {
    if (is.null(df)) {
        df <- .load_package_references(package, error=error)
    }

    m <- match(id, df$id)
    if (is.na(m)) {
        if (error) {
            stop("'", id, "' not a recognized reference for '", package, "'") 
        } else {
            return(NULL)
        }
    }

    text <- df$text[m]
    if (is.null(type)) {
        if (grepl("fig:", id)) {
            type <- "Figure"
        } else if (grepl("\\.", text)) {
            type <- "Section"
        } else {
            type <- "Chapter"
        }
    } 

    if (!is.null(type) && !is.na(type)) {
        if (is.null(prefix)) {
            prefix <- .load_package_prefix(package)
        }
        if (!is.null(prefix) && !is.na(prefix)) {
            type <- paste(prefix, type)
        }
        text <- paste(type, text)
    }

    Biocbook(paste0(package, "/", df$file[m], "#", id), label=text)
}

link.env <- new.env()
link.env$df.list <- list()
link.env$prefix.list <- list()

#' @importFrom utils read.csv
.load_package_references <- function(package, error) {
    df <- link.env$df.list[[package]]
    if (is.null(df)) {
        path <- system.file("rebook", "references.csv", package=package, mustWork=error)
        if (path!="") {
            df <- read.csv(path)
            link.env$df.list[[package]] <- df
        }
    }
    df
}

.load_package_prefix <- function(package) {
    prefix <- link.env$prefix.list[[package]]

    if (is.null(prefix)) {
        # This is install-time information, so we can cache this safely.
        attempt <- system.file("rebook", "prefix.csv", package=package)
        if (attempt=="") {
            prefix <- paste0("**", package, "**")
        } else {
            prefix <- readLines(attempt)[1]
        }
        link.env$prefix.list[[package]] <- prefix
    }

    prefix
}
