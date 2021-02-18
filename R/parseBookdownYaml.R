#' @importFrom yaml read_yaml
.parse_bookdown_yaml <- function(dir) {
    read_yaml(file.path(dir, "_bookdown.yml"))
}

.get_book_chapters <- function(dir) {
    out <- .parse_bookdown_yaml(dir)

    to.use <- out$rmd_files
    if (is.null(to.use)) {
        to.use <- list.files(pattern=".Rmd$", ignore.case=TRUE)
        for (i in out$rmd_subdir) {
            to.use <- c(to.use, file.path(i, list.files(out$rmd_subdir, pattern=".Rmd$", ignore.case=TRUE)))
        }
    }

    to.use
}

.find_output_directory <- function(dir) {
    out <- .parse_bookdown_yaml(dir)
    outdir <- out$output_dir
    if (is.null(outdir)) {
        outdir <- "_book"
    }
    outdir
}
