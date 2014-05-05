setwd("q:/home/wsp/github/rappster/forked/pathological")
require("formatR")

files <- c(
    "R/paths.R"
)
tidyAndSave <- function(x, ...) {
    code <- tidy.source(source=x, ...)
    write(code$text.tidy, file="c:/temp/pathological.R")
}
sapply(files, tidyAndSave, reindent.spaces=2, width.cutoff=70)