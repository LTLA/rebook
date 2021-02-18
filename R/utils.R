.parse_bookdown_yaml <- function(dir) {
    yaml::read_yaml(file.path(dir, "_bookdown.yml"))
}

.get_book_chapters <- function(dir) {
    out <- .parse_bookdown_yaml(dir)

    to.use <- out$rmd_files
    if (is.null(to.use)) {
        to.use <- list.files(dir, pattern=".Rmd$", ignore.case=TRUE)
        for (i in out$rmd_subdir) {
            to.use <- c(to.use, file.path(i, list.files(file.path(dir, i), pattern=".Rmd$", ignore.case=TRUE)))
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

#' @importFrom filelock lock
.lock_report <- function(path, ...) {
    lck.path <- paste0(path, "-00LOCK")
    lock(lck.path, ...)
}

#' @importFrom filelock unlock
.unlock_report <- function(lck) {
    unlock(lck)
}

#' @importFrom filelock lock
.lock_dir <- function(dir, ...) {
    host <- dirname(dir)
    if (!dir.exists(host)) {
        dir.create(host, recursive=TRUE, showWarnings=FALSE)
    }

    # TODO: replace with dir.expiry::lockDirectory.
    lck.path <- paste0(sub("/$", "", dir), "-00LOCK")
    lock(lck.path, ...)
}

#' @importFrom filelock unlock
.unlock_dir <- function(lck) {
    # TODO: replace with dir.expiry::unlockDirectory.
    unlock(lck)
}
