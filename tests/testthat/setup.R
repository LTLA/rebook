compileBook <- function(src.dir, work.dir, final.dir, input) {
    if (.Platform$OS.type=="windows") {
        # These checks are not required in the configureBook() function itself,
        # because the paths there are always relative and generated with '/'.
        src.dir <- gsub("\\\\", "/", src.dir)
        final.dir <- gsub("\\\\", "/", final.dir)
        work.dir <- gsub("\\\\", "/", work.dir)
    }
    cmds <- rebook:::.makeCommandString(src.dir, deparse(work.dir), final.dir, input=input)
    eval(parse(text=cmds))
}
