\name{rebookNews}
\title{rebook news}
\encoding{UTF-8}

\section{Version 1.2.0}{\itemize{
\item
Added the \code{rmd2id()} function to easily determine the ID for each chapter.

\item
Link to the originating chapter for the set-up code in \code{extractCached()}.

\item 
Expose \code{collapseStart()} and \code{collapseEnd()} for manual creation of collapsible chunks.

\item 
Added \code{scrapeReferences()} to scrape a \pkg{bookdown} book for references for external use.

\item 
Added \code{configureBook()} to configure a Bioconductor package as a book deployment.

\item 
Added \code{link()} to rapidly link to references in a configured Bioconductor book package.

\item 
Added \code{extractFromPackage()} to extract objects from Rmarkdown files in installed packages.

\item 
Added \code{createRedirects()} to redirect from old, deprecated pages to their new locations.
}}

\section{Version 1.0.0}{\itemize{
\item
New package rebook, providing utilities for creating Bioconductor books. 
}}
