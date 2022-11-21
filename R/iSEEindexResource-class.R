### =========================================================================
### iSEEindexResource objects
### -------------------------------------------------------------------------
###

#' The iSEEindexResource class
#' 
#' The iSEEindexResource class represents a resource accessible through
#' an HTTPS link.
#' 
#' @section Methods:
#' The `download()` method returns the URI to the HTTPS link as-is,
#' because \pkg{BiocFileCache} is capable of using those links directly.
#'
#' @name iSEEindexResource-class
#' @rdname iSEEindexResource-class
#' @exportClass iSEEindexResource
#' @aliases download,iSEEindexHttpsResource-method
setClass("iSEEindexResource",
    slots=c(
        uri="character"
    ))

setMethod("show", "iSEEindexResource",
    function(object)
{
    cat("class:", class(object), "\n")
})


#' @export
setGeneric("download", function(x, ...) {
    stopifnot(is(x, "iSEEindexResource"), length(x) == 1L)
    standardGeneric("download")
})

#' @export
setMethod("download", "iSEEindexResource",
    function(x, ...)
{
    msg <- sprintf("no 'download' method defined for object
        of class %s, consider defining your own.",
        sQuote(class(x)))
    stop(paste(strwrap(msg), collapse="\n"))
})

# iSEEindexHttpsResource ----

#' The iSEEindexHttpsResource class
#' 
#' The iSEEindexHttpsResource class represents a resource accessible through
#' an HTTPS link.
#' 
#' @section Methods:
#' The `download()` method returns the URI to the HTTPS link as-is,
#' because \pkg{BiocFileCache} is capable of using those links directly.
#'
#' @name iSEEindexHttpsResource-class
#' @rdname iSEEindexHttpsResource-class
#' @exportClass iSEEindexHttpsResource
#' @aliases download,iSEEindexHttpsResource-method
setClass("iSEEindexHttpsResource", contains="iSEEindexResource")

#' @export
setMethod("download", "iSEEindexHttpsResource",
    function(x, ...)
{
    # Do not actually download anything
    # Simply return the original URI to BiocFileCache
    # which will manage the download automatically.
    return(x@uri)
})
