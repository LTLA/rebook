#' Print the collapse opening and ending
#'
#' Print HTML tags to open and close the collapsible chunks.
#'
#' @param message String containing a message to insert in the collapsible header.
#'
#' @return
#' Both functions will \code{\link{cat}} HTML tags; one to start and another to end each collapsible chunk.
#'
#' @author Aaron Lun
#'
#' @examples
#' collapseStart("This is collapsible")
#' cat("something inside the chunk\n")
#' collapseEnd()
#'
#' @export
collapseStart <- function(message) {
    # Pretty-printing the chunks.
    cat(sprintf('<button class="rebook-collapse">%s</button>
<div class="rebook-content">\n\n', message))
}

#' @export
#' @rdname collapseStart
collapseEnd <- function() {
    cat("\n</div>\n")
}

