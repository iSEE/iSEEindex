.stop <- function(x) {
    stop(paste(strwrap(x, exdent = 2), collapse = "\n"))
}

.warning <- function(x) {
    warning(paste(strwrap(x, exdent = 2), collapse = "\n"))
}
