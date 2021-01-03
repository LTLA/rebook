#' Get the chapter identifier
#'
#' Get the identifier for a book chapter given the Rmarkdown source code.
#' This is usually derived from the chapter title but can also be explicitly specified.
#'
#' @param path String containing the path to the Rmarkdown file for a chapter.
#'
#' @return String containing the identifier for this chapter.
#' If no identifier can be determined, \code{NULL} is returned.
#' 
#' @author Aaron Lun
#'
#' @examples
#' tmp <- tempfile(fileext='.Rmd')
#' write('# some chapter name
#'
#' blah', file=tmp)
#' rmd2id(tmp)
#'
#' tmp2 <- tempfile(fileext='.Rmd')
#' write('# some chapter name {#chapter-id}
#'
#' blah', file=tmp2)
#' rmd2id(tmp2)
#'
#' 
#' @export
rmd2id <- function(path) {
    contents <- readLines(path)
    candidates <- grepl("^# ", contents) & !grepl("# \\(PART\\)", contents)
    candidates <- which(candidates)

    # A quick-and-dirty parser that assumes chunks are paired.
    chunk.boundaries <- "^ *```"
    chunk.lines <- grep(chunk.boundaries, contents)  
    exclude <- findInterval(candidates, chunk.lines) %% 2 == 1
    candidates <- candidates[!exclude]

    if (!any(candidates)) {
        return(NULL)
    }
    title <- contents[candidates][1]
    html.name <- sub("^# ", "", sub("\\s*$", "", title))

    anchor.pattern <- ".*\\{#(.*)\\}$"
    if (grepl(anchor.pattern, html.name)) {
        stub <- sub(anchor.pattern, "\\1", html.name)
    } else {
        stub <- gsub("[()]", "", html.name)
        stub <- gsub(" +", "-", stub)
    }

    tolower(stub)
}
