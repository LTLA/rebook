#' Deploy a custom CSS
#'
#' Deploy a custom CSS to change the colors of the book's section headers, mostly to add some flavor to the book.
#'
#' @param path String containing the path to the output CSS file.
#' @param h2.col String containing the color to use for the section headers.
#' @param h3.col String containing the color to use for the subsection headers.
#' 
#' @return The CSS file is overwritten at \code{path}.
#' A \code{NULL} is invisibly returned.
#'
#' @author Aaron Lun,
#' based on work by Rob Amezquita and Kevin Rue-Albrecht
#'
#' @details
#' We quickly learned that it was unwise to be too adventurous with the colors.
#' In particular, changing the colors of the table of contents was quite distracting.
#' Altering the colors of the section headers provides a tasteful level of customization,
#' with the default colors set (almost) to the Bioconductor color palette.
#'
#' @examples
#' fname <- tempfile(fileext=".css")
#' deployCustomCSS(fname)
#' cat(readLines(fname), sep="\n")
#'
#' @export
deployCustomCSS <- function(path="style.css", h2.col="#87b13f", h3.col="#1a81c2") {
    write(sprintf(.css_template, h2.col, h3.col), file=path)  
    invisible(NULL)
}

.css_template <- "/*
Based on work by Rob Amezquita and Kevin Rue-Albrecht
*/

p.caption {
  color: #777;
  margin-top: 10px;
}
p code {
  white-space: inherit;
}
pre {
  word-break: normal;
  word-wrap: normal;
}
pre code {
  white-space: inherit;
}

.book .book-summary {
  background-color: #f0f0f0;
}

.book .book-summary ul.summary li span {
  color: black;
}

.book .book-body .page-wrapper .page-inner section.normal h1 {
  color: black;
}

.book .book-body .page-wrapper .page-inner section.normal h2 {
  color: %s;
}

.book .book-body .page-wrapper .page-inner section.normal h3 {
  color: %s;
}"
