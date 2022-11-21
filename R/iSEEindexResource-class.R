#' The iSEEindexResource class
#' 
#' The iSEEindexResource class represents an undefined resource.
#' 
#' @section Slot overview:
#' The following slots are relevant to panel organization:
#' \itemize{
#' \item \code{uri}, a character scalar specifying the URI to a resource.
#' }
#' 
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{iSEEindexResource} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#' 
#' \itemize{
#' \item \code{\link{precache}(x, ,..)} throws an error, encouraging users to develop a method for derived classes that are not suported yet.
#' }
#' 
#' @author Kevin Rue-Albrecht
#'
#' @name iSEEindexResource-class
#' @rdname iSEEindexResource-class
#' @aliases 
#' show,iSEEindexResource-method
#' precache,iSEEindexResource-method
NULL

#' @export
setClass("iSEEindexResource",
    slots=c(
        uri="character"
    ))

#' @export
setMethod("show", "iSEEindexResource",
    function(object)
{
    cat("class:", class(object), "\n")
})

#' Download and Cache Resources
#' 
#' An overview of the generics for downloading and caching resources.
#' 
#' @section Defining the parameter interface:
#' `precache(x, ...)` potentially downloads a resource from an URI and returns the path to the local downloaded file:
#' \itemize{
#' \item \code{x}, a character scalar that represents a URI.
#' \item \code{...}, additional arguments passed to and from other methods.
#' }
#' 
#' @docType methods
#' @aliases precache
#' @name iSEEindexResource-generics
#' @author Kevin Rue-Albrecht
NULL
setGeneric("precache", function(x, ...) {
    stopifnot(is(x, "iSEEindexResource"), length(x) == 1L)
    standardGeneric("precache")
})

#' @export
#' @aliases precache,iSEEindexResource-method
setMethod("precache", "iSEEindexResource",
    function(x, ...)
{
    msg <- sprintf("no 'precache' method defined for object
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
#' @section Slot overview:
#' This class inherits all slots from its parent class \linkS4class{iSEEindexResource}.
#' 
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{iSEEindexHttpsResource} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#' 
#' \itemize{
#' \item \code{\link{precache}(x, ,..)} returns the URI to the resource as-is,
#' because The \pkg{BiocFileCache} naturally supports the HTTPS protocol.
#' }
#' 
#' @author Kevin Rue-Albrecht
#'
#' @name iSEEindexHttpsResource-class
#' @rdname iSEEindexHttpsResource-class
#' @aliases 
#' precache,iSEEindexHttpsResource-method
NULL

#' @export
setClass("iSEEindexHttpsResource", contains="iSEEindexResource")

#' @export
setMethod("precache", "iSEEindexHttpsResource",
    function(x, ...)
{
    # Do not download anything
    # Simply return the original URI to BiocFileCache
    # which will manage the download automatically.
    return(x@uri)
})
