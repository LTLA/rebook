#' Report opening details about the book 
#'
#' Report opening details about the book, to be executed as an R expression in the \code{Date:} field.
#' 
#' @param Copyright String containing copyright information; defaults to \code{"Bioconductor, <current year>"}.
#' @param ... Further named strings to be included in the opening details.
#'
#' @details
#' It is usually sufficient to set something like
#' \preformatted{date: "`r rebook::openingDetails()`"
#' }
#' in the YAML header of the book, thereby ensuring that the book details are printed after the title but before any contents.
#' This assumes that none of the details have problematic characters, particularly double quotes.
#'
#' Details are extracted from a \code{DESCRIPTION} file in the current or any parent directory.
#' This assumes that authors are formatted as \code{Authors@R} and the \code{License} and \code{Date} fields are specified.
#'
#' @return A string containing the formatted details for inclusion into a YAML header.
#'
#' @author Aaron Lun
#' @examples
#' wd <- getwd()
#' setwd(system.file(package="rebook"))
#' cat(openingDetails(), '\n')
#' setwd(wd)
#' @export
openingDetails <- function(..., Copyright=NULL) {
    # Hunt for a DESCRIPTION file!
    curpath <- getwd()
    while (curpath != (nextpath <- dirname(curpath))) {
        target <- file.path(curpath, "DESCRIPTION")
        if (file.exists(target)) {
            break
        }
        curpath <- nextpath
    }
    if (!file.exists(target)) {
        stop("failed to find a DESCRIPTION file")
    }

    info <- read.dcf(target)
    authors <- eval(parse(text=info[,"Authors@R"]))

    # Defaulting the copyright to Bioconductor, current year.
    if (is.null(Copyright)) {
        Copyright <- paste("Bioconductor,", format(Sys.Date(), "%Y"))
    }

    # Getting the license for the book.
    stash <- c(
        Authors = paste(format(authors, include=c("given", "family", "role")), collapse=", "),
        Version = unname(info[,"Version"]),
        Modified = unname(info[,"Date"]),
        Compiled = as.character(Sys.Date()),
        Environment = paste0(R.version.string, ", Bioconductor ", BiocManager::version()),
        License = unname(info[,"License"]),
        Copyright = Copyright,
        ...
    )

    combined <- sprintf("**%s:** %s", names(stash), stash)
    paste(combined, collapse="<br/>\n  ")
}
