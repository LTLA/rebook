#' Extract cached objects
#' 
#' Extract specific R objects from the knitr cache of a previously compiled Rmarkdown file (the \dQuote{donor})
#' so that it can be used in the compilation process of another Rmarkdown file (the \dQuote{acceptor}).
#'
#' @param donor String containing the name of the donor Rmarkdown file.
#' @param locations Character vector containing the possible locations to search for the donor file.
#' @param chunk String containing the name of the requested chunk.
#' @param objects Character vector containing variable names for one or more objects to be extracted.
#' @param envir Environment where the loaded objects should be stored.
#'
#' @details
#' Each R object is extracted in its state at the requested \code{chunk} and inserted into \code{envir}.
#' Note that the object does not have to be generated or even referenced in \code{chunk},
#' provided it was generated in a previous chunk.
#'
#' The parser in this function is rather limited,
#' so the donor Rmarkdown file is subject to several constraints:
#' \itemize{
#' \item All chunks involved in generating the requested objects (indirectly or otherwise) should be named.
#' \item All named chunks should be executed, i.e., no \code{eval=FALSE}.
#' \item All relevant code occurs within triple backticks, i.e., any inline code should be read-only.
#' \item All triple backticks occur at the start of the line, i.e., no code nested in list elements.
#' \item Any assignment or modifications to variables are done \emph{correctly} with \code{<-}.
#' }
#' 
#' Unnamed chunks are allowed but cannot be referenced and will not be shown in the output of this function.
#' This should not be used for code that might affect variables in the named chunks,
#' i.e., code in unnamed chunks should be \dQuote{read-only} with respect to variables in the named chunks.
#' Chunks with names starting with \code{unref-} are considered to be the same as unnamed chunks and will be ignored;
#' this is useful for figure-generating chunks that need to be referenced inside the donor report.
#'
#' The donor report is found by searching successive \code{locations} for a file that matches \code{donor}.
#' It will use the first file that is found in this manner.
#'
#' Obviously, this entire process assumes that donor report has already been compiled with \code{cache=TRUE}.
#' If not, \code{extractCached} will compile it (and thus generate the cache) using \code{\link{compileCached}}.
#'
#' @return Variables with names \code{objects} are created in \code{envir}.
#' A markdown chunk (wrapped in a collapsible element) is printed that contains all commands needed to generate those objects, 
#' based on the code in the named chunks of the donor Rmarkdown file.
#' 
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{setupHTML}} and \code{\link{chapterPreamble}}, to set up the code for the collapsible element.
#'
#' \code{\link{compileChapter}}, to compile a Rmarkdown report to generate the cache.
#' 
#' @export
extractCached <- function(donor, chunk, objects, locations=".", envir=topenv(parent.frame())) {
    fname <- .obtain_cache_path(donor, locations)
    prefix <- sub("\\.rmd$", "", fname, ignore.case = TRUE)
    cache_path <- file.path(paste0(prefix, "_cache"), "html/")
    if (!file.exists(cache_path)) {
        compileChapter(fname)
    }

    chunks <- .extract_chunks(fname, chunk)
    .load_objects(cache_path, chunks, objects=objects, envir=envir)

    # Pretty-printing the chunks.
    cat('<button class="aaron-collapse">View history</button>
<div class="aaron-content">
   
```r\n')
    first <- TRUE
    for (x in names(chunks)) {
        if (!first) {
            cat("\n")
        } else {
            first <- FALSE
        }
        cat(sprintf("#--- %s ---#\n", x))
        cat(chunks[[x]], sep="\n")
    }
    cat("```

</div>\n")

    invisible(NULL)
}

.obtain_cache_path <- function(donor, locations) {
    for (l in locations) {
        possible <- file.path(l, donor)
        if (file.exists(possible)) {
            return(possible)
        }
    }
    stop("'donor' file not found")
}

.extract_chunks <- function(fname, chunk) {
    # Extracting chunks until we get to the one with 'chunk'.
    all.lines <- readLines(fname)
    named.pattern <- "^```\\{r ([^,]+).*\\}"
    opens <- grep(named.pattern, all.lines)

    chunks <- list()
    for (i in seq_along(opens)) {
        if (i < length(opens)) {
            j <- opens[i+1] - 1L
        } else {
            j <- length(all.lines)
        }

        available <- all.lines[(opens[i]+1):j]
        end <- grep("^```\\s*$", available)
        if (length(end)==0L) {
            stop("unterminated chunk")         
        } 

        curname <- sub(named.pattern, "\\1", all.lines[opens[i]])
        if (!grepl("^unref-", curname)) {
            current.chunk <- available[seq_len(end[1]-1L)]
            chunks[[curname]] <- current.chunk
        }
    }

    m <- match(chunk, names(chunks))
    if (is.na(m)) {
        stop(sprintf("could not find chunk '%s'", chunk))
    }
    chunks[seq_len(m)]
}

#' @importFrom knitr opts_knit load_cache
.load_objects <- function(cache_path, chunks, objects, envir) {
    # This is required so that the cache_path is interpreted correctly,
    # as load_cache will 'cd' into 'output.dir' and break relative paths. 
    if (is.null(old <- opts_knit$get("output.dir"))) {
        opts_knit$set(output.dir=".")
        on.exit(opts_knit$set(output.dir=old))
    }

    # Collecting all variable names and loading them into the global namespace.
    for (obj in objects) {
        assign.pattern <- paste0(obj, ".*<-")
        found <- FALSE

        # Setting 'rev' to get the last chunk in which 'obj' was on the left-hand side of assignment.
        for (x in rev(names(chunks))) {
            if (found <- any(grepl(assign.pattern, chunks[[x]]))) {
                assign(obj, envir=envir, value=load_cache(label=x, object=obj, path=cache_path))
                break
            }
        }

        if (!found) {
            stop(sprintf("could not find '%s'", obj))
        }
    }

    invisible(NULL)
}
