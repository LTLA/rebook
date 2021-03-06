% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createRedirects.R
\name{createRedirects}
\alias{createRedirects}
\title{Create redirection pages}
\usage{
createRedirects(
  name,
  pkg,
  page,
  dir = "../inst/doc/book",
  file = NULL,
  check = FALSE,
  include.gif = TRUE
)
}
\arguments{
\item{name}{Character vector containing the name of each HTML.}

\item{pkg}{Character vector containing the name of the Bioconductor book package to redirect to.}

\item{page}{Character vector containing the name of the new chapter to redirect to.}

\item{dir}{String containing the path to the output directory for the HTMLs.}

\item{file}{String containing the name of a comma-separate file with three unnamed columns, from which to derive \code{name}, \code{pkg} and \code{page}.}

\item{check}{Logical scalar indicating whether to check if the destination URL exists.}

\item{include.gif}{Logical scalar indicating whether a GIF should be included in the redirection notice.}
}
\value{
HTMLs of the specified \code{name} are created in \code{dir}, redirected to the sites defined by their respective \code{pkg} and \code{page} entries.
A \code{NULL} is invisibly returned.
}
\description{
Create HTML pages to redirect users to the latest version of the relevant Bioconductor book.
This is useful for preserving compatibility with old links when reorganizing the contents of a book.
}
\details{
This function is intended to be called inside the Makefile generated by \code{\link{configureBook}},
which will create the necessary HTMLs at package build time.
The expectation is that there is a file like \code{redirect.txt} that can be passed in as the \code{file} argument.
The default \code{dir} is the same as the final destination for all HTMLs that is defined in the Makefile.

In \code{file}, the last column can be left empty for any row.
This will instruct \code{createRedirects} to re-use \code{name} as \code{page},
which is convenient when a chapter is simply moved to another package without a change in the HTML file name.

It is probably a good idea to run with \code{check=TRUE} on occasion, to verify that the redirections are working.
This is not done by default to avoid a chicken-and-egg situation where two books cannot build because they redirect to each other.
}
\examples{
tmp <- tempfile()
dir.create(tmp)
createRedirects("BLAH.html", pkg="OSCA.intro", page="installation.html", dir=tmp)

if (interactive()) {
    browseURL(file.path(tmp, "BLAH.html"))
}

}
\author{
Aaron Lun
}
