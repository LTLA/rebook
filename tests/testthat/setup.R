compileBook <- function(src.dir, work.dir, final.dir, input) {
    cmds <- rebook:::.makeCommandString(src.dir, deparse(work.dir), final.dir, input=input)
    eval(parse(text=cmds))
}
