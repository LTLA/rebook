#' Get various Bioconductor images
#'
#' Helper functions to pull down images to use in the book.
#' These aim to provide a sensible default for Bioconductor-related books.
#'
#' @param mode String specifying the type of sticker to show.
#'
#' @return 
#' \code{BiocFavicon} will return a path to a \code{favicon.ico} file.
#'
#' \code{BiocSticker} will return a URL or path to a sticker.
#'
#' @author Aaron Lun
#'
#' @examples
#' BiocFavicon()
#'
#' BiocSticker()
#' @export
#' @name bioc-images
BiocFavicon <- function() {
    system.file('images', 'favicon.ico', package='rebook', mustWork=TRUE)
}

#' @export
#' @name bioc-images
BiocSticker <- function(mode=c("static", "animated")) {
    mode <- match.arg(mode)
    if (mode=="static") {
        "https://github.com/Bioconductor/BiocStickers/blob/master/Bioconductor/Bioconductor-rh.png"
    } else {
        "https://github.com/Bioconductor/BiocStickers/raw/master/Bioconductor/Bioconductor-serial.gif"
    }
}
