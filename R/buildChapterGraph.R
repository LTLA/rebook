#' Build the chapter dependency graph
#'
#' Build the dependency graph between chapter based on their \code{\link{extractCached}} calls to each other.
#'
#' @inheritParams scrapeDependencies
#'
#' @return A directed \link[igraph]{graph} object from the \pkg{igraph} package,
#' where each node is a chapter and is connected to its dependencies by an edge.
#'
#' @author Aaron Lun
#'
#' @export
#' @importFrom CodeDepends readScript scriptInfo
buildChapterGraph <- function(dir, recursive=TRUE, pattern="\\.Rmd$") {
    all.rmds <- list.files(dir, recursive=recursive, pattern=pattern)
    collated <- list()
    
    for (i in seq_along(all.rmds)) {
        txt <- readScript(file.path(dir, all.rmds[i]), type="Stangled")
        all.info <- scriptInfo(txt)
        dependencies <- c()

        for (j in seq_along(all.info)) {
            current <- all.info[[j]]
            if ("extractCached" %in% names(current@functions)) {
                dependencies <- c(dependencies, intersect(current@files, all.rmds))
            }
        }

        collated[[all.rmds[i]]] <- dependencies
    }

    all.upstream <- unlist(collated)
    igraph::make_graph(rbind(all.upstream, rep(names(collated), lengths(collated))), 
        isolates=setdiff(all.rmds, all.upstream))
 }
