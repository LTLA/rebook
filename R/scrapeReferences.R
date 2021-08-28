#' Scrape references from a \pkg{bookdown} directory
#'
#' Scrape references to sections and figures from all Rmarkdown files in a \pkg{bookdown} directory.
#'
#' @param dir String containing a path to a \pkg{bookdown}-containing directory.
#' @param input String containing the name of the file to use in the \code{\link[bookdown]{render_book}} statement.
#' @param workdir String containing a path to a working directory to use to store bits and pieces.
#' @param clean Logical scalar indicating whether the working directory should be removed upon function completion.
#' 
#' @return A data.frame where each row corresponds to a reference.
#' It has \code{id}, the name of the reference;
#' \code{file}, the compiled HTML file that the reference comes from;
#' and \code{text}, the text to be associated with that reference.
#'
#' @details
#' This function works by performing a quick dummy compilation of the book, turning off all evaluations with a global \code{eval=FALSE}.
#' It then trawls the set of newly created HTML files, pulling out the section/figure identifiers and collating them into a data.frame.
#'
#' The goal is to facilitate convenient linking between books by automatically filling in the file and text for a given link.
#' Packages that deploy books should run this in their \code{configure} scripts to obtain a reference mapping that they can serve to other packages via \code{\link{link}}.
#' 
#' Extraction of the figure text assumes that the figure prefix ends with a non-numeric character, e.g., \code{"Figure "} or \code{"Figure S"}.
#'
#' @seealso
#' \code{\link{link}}, to create links given a package name and identifier.
#' @examples
#' book.dir <- system.file("example", package="rebook")
#' df <- scrapeReferences(book.dir)
#' df
#' @author Aaron Lun
#' @export
scrapeReferences <- function(dir, input="index.Rmd", workdir=tempfile(), clean=TRUE) {
    dir.create(workdir, showWarnings=FALSE)
    file.copy(dir, workdir, recursive=TRUE, overwrite=TRUE)
    if (clean) { on.exit(unlink(workdir, recursive=TRUE)) }

    ###################################################
    # Doing a dummy compilation to get the references.
    # This requires removal of lines that break with eval=FALSE.

    all.files <- list.files(workdir, recursive=TRUE, full.names=TRUE, pattern=".Rmd$")
    for (f in all.files) {
        lines <- readLines(f)

        # Removing all lines with inline references.
        starts.inline <- startsWith(lines, "`r ") 
        lines[starts.inline] <- sub("^`r [^`]+`", "PLACEHOLDER", lines[starts.inline])
        has.inline <- grepl("[^`]`r ", lines)
        lines[has.inline] <- gsub("([^`])`r [^`]+`", "\\1PLACEHOLDER", lines[has.inline])

        # Preventing any actual evaluation of code.
        lines <- c("```{r}\nknitr::opts_chunk$set(eval=FALSE)\n```\n\n", lines)

        # Forcibly stuffing in a plot for any fig.cap=-containing chunk.
        leads.plot <- grepl("```{r ", lines, fixed=TRUE) & grepl("fig.cap=", lines, fixed=TRUE)
        plot.label <- sub(".*```\\{r ([^,]+),.*", "\\1", lines[leads.plot])
        lines[leads.plot] <- sprintf("```{r %s, eval=TRUE, fig.cap='PLACEHOLDER'}\nplot(1,1)\n```\n\n```{r}", plot.label)

        write(lines, file=f)
    }

    target.dir <- file.path(workdir, basename(dir))
    old <- setwd(target.dir)
    on.exit(setwd(old), add=TRUE, after=FALSE) # change directory before deletion.

    bookdown::render_book(input, output_dir="docs", quiet=TRUE)

    ###################################################
    # Running through the HTMLs and scraping the linking information. 
    # This is rather fragile as it assumes a particular HTML structure.

    all.compiled <- list.files("docs", pattern=".html$", full.names=TRUE)
    link.info <- list(data.frame(id=character(0), file=character(0), text=character(0)))
    counter <- 1L

    for (html in all.compiled) {
        out <- XML::htmlParse(html)

        # Pulling out all numbered sections.
        sec.divs <- XML::getNodeSet(out, "//div[matches(@class, 'section')]")
        for (i in seq_along(sec.divs)) {
            cur.div <- sec.divs[[i]]
            if (grepl("unnumbered", XML::xmlGetAttr(cur.div, name="class"))) {
                next
            }

            # Find the number based on the first header section number after it.
            num.node <- XML::getNodeSet(cur.div, "(.//span[@class='header-section-number'])[1]")
            if (length(num.node) != 1) {
                next
            }

            sec.text <- XML::xmlValue(num.node)
            matches <- regexpr("[0-9][0-9\\.]*$", sec.text) # stripping out non-numeric prefixes.
            sec.num <- regmatches(sec.text, matches)

            link.info[[counter]] <- data.frame(
                id=XML::xmlGetAttr(cur.div, name="id"),
                file=basename(html),
                text=sec.num
            )
            counter <- counter + 1L
        }

        # Pulling out all numbered figures.
        fig.divs <- XML::getNodeSet(out, "//div[@class='figure']")
        for (i in seq_along(fig.divs)) {
            id.thing <- XML::getNodeSet(fig.divs[[i]], "*[@id]")
            stopifnot(length(id.thing)==1L)
            caption.thing <- XML::getNodeSet(fig.divs[[i]], "*[@class='caption']")
            stopifnot(length(caption.thing)==1L)

            # Fishing out the number from the placeholder.
            cap.text <- XML::xmlValue(caption.thing[[1]])
            matches <- regexpr("[0-9][0-9\\.]+: ", cap.text)
            fig.num <- regmatches(cap.text, matches)
            fig.num <- sub(": ", "", fig.num)
            stopifnot(length(fig.num)==1L)

            link.info[[counter]] <- data.frame(
                id=XML::xmlGetAttr(id.thing[[1]], name="id"),
                file=basename(html),
                text=fig.num
            )
            counter <- counter + 1L
        }
    }

    do.call(rbind, link.info)
}
