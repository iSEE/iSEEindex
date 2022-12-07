#' The iSEEindexResource class
#' 
#' The iSEEindexResource class represents an undefined resource.
#' 
#' @section Slot overview:
#' \itemize{
#' \item \code{uri}, a character scalar specifying the URI to a resource.
#' }
#' 
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{iSEEindexResource} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#' 
#' \itemize{
#' \item \code{\link{precache}(x, bfc, id, ...)} throws an error, encouraging users to develop a method for derived classes that are not suported yet.
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
#' @section Preparing and caching resources:
#' `precache(x, bfc, id, ...)` potentially downloads a resource from an URI, caches it, and returns the path to the cached file:
#' \itemize{
#' \item \code{x}, a character scalar that represents a URI.
#' \item \code{bfc}, a [BiocFileCache()] object..
#' \item \code{id}, a data set identifier as a character scalar..
#' \item \code{...}, additional arguments passed to and from other methods.
#' }
#' 
#' @author Kevin Rue-Albrecht
#' 
#' @docType methods
#' @aliases precache
#' @name iSEEindexResource-generics
NULL

setGeneric("precache", function(x, bfc, id, ...) {
    stopifnot(is(x, "iSEEindexResource"), length(x) == 1L)
    standardGeneric("precache")
})

#' @export
setMethod("precache", "iSEEindexResource",
    function(x, bfc, id, ...)
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
#' A URI for this type of resource uses the prefix \dQuote{https://}.
#' 
#' @section Slot overview:
#' This class inherits all slots from its parent class \linkS4class{iSEEindexResource}.
#' 
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{iSEEindexHttpsResource} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#' 
#' \itemize{
#' \item \code{\link{precache}(x, ...)} returns the URI to the resource as-is,
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
    function(x, bfc, id, ...)
{
    # Pass the URI as-is.
    # Let the BiocFileCache manage the download automatically.
    fpath <- x@uri
    object_path <- bfcadd(x = bfc, rname = id, fpath = fpath, action = "copy", ...)
    return(object_path)
})

# iSEEindexLocalhostResource ----

#' The iSEEindexLocalhostResource class
#' 
#' The iSEEindexLocalhostResource class represents a resource accessible through
#' a local filepath.
#' A URI for this type of resource uses the prefix \dQuote{localhost://}.
#' 
#' @details 
#' Absolute and relative paths are both supported.
#' 
#' Absolute paths require an additional `/` (forward slash)
#' following the double forward slash `//` separating the scheme component of the URI.
#' 
#' For instance:
#' 
#' \itemize{
#' \item `localhost://path/to/file` refers to the relative path `path/to/file`
#' (relative to the working directory when the Shiny application is launched).
#' \item `localhost:///path/to/file` refers to the absolute path `/path/to/file`.
#' }
#' 
#' @section Slot overview:
#' This class inherits all slots from its parent class \linkS4class{iSEEindexResource}.
#' 
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{iSEEindexLocalhostResource} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#' 
#' \itemize{
#' \item \code{\link{precache}(x, ...)} trims the `localhost://` prefix,
#' and returns the remainder of the URI as a file path, for use in the \pkg{BiocFileCache}.
#' }
#' 
#' @author Kevin Rue-Albrecht
#'
#' @name iSEEindexLocalhostResource-class
#' @rdname iSEEindexLocalhostResource-class
#' @aliases 
#' precache,iSEEindexLocalhostResource-method
NULL

#' @export
setClass("iSEEindexLocalhostResource", contains="iSEEindexResource")

#' @export
setMethod("precache", "iSEEindexLocalhostResource",
    function(x, bfc, id, ...)
{
    # Trim 'localhost://' from the original URI and pass to BiocFileCache,
    # which will manage the caching.
    # Use action="copy" to leave the original file untouched.
    fpath <- sub("localhost://", "", x@uri)
    stopifnot(file.exists(out))
    object_path <- bfcadd(x = bfc, rname = id, fpath = fpath, action = "copy", ...)
    return(object_path)
})

# iSEEindexRcallResource ----

#' The iSEEindexRcallResource class
#' 
#' The iSEEindexRcallResource class represents a resource accessible through
#' the result of an R call.
#' A URI for this type of resource uses the prefix \dQuote{rcall://}.
#' 
#' @details 
#' The URI must contain valid R code, once the prefix `rcall://` is removed.
#' The code must return the file path to an existing R script
#' that produces an object called `initial` that contains a valid configuration
#' for an \pkg{iSEE} app.
#' 
#' For instance:
#' 
#' ```
#' rcall://system.file(package='iSEEindex','ReprocessedAllenData_config_01.R')
#' ```
#' 
#' @section Slot overview:
#' This class inherits all slots from its parent class \linkS4class{iSEEindexResource}.
#' 
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{iSEEindexRcallResource} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#' 
#' \itemize{
#' \item \code{\link{precache}(x, ...)} trims the `rcall://` prefix,
#' evaluates the remainder of the URI as R code,
#' and returns the resulting file path, for use in the \pkg{BiocFileCache}.
#' }
#' 
#' @author Kevin Rue-Albrecht
#'
#' @name iSEEindexRcallResource-class
#' @rdname iSEEindexRcallResource-class
#' @aliases 
#' precache,iSEEindexRcallResource-method
NULL

#' @export
setClass("iSEEindexRcallResource", contains="iSEEindexResource")

#' @export
setMethod("precache", "iSEEindexRcallResource",
    function(x, bfc, id, ...)
{
    # Trim 'rcall://' from the original URI and evaluate the R call,
    # check that the value is an existing filepath and pass to BiocFileCache,
    # which will manage the caching.
    # Use action="copy" to leave the original file untouched.
    call_string <- sub("rcall://", "", x@uri)
    fpath <- eval(parse(text = call_string))
    stopifnot(file.exists(fpath))
    object_path <- bfcadd(x = bfc, rname = id, fpath = fpath, action = "copy", ...)
    return(object_path)
})

# iSEEindexS3Resource ----

#' The iSEEindexS3Resource class
#' 
#' The iSEEindexS3Resource class represents a cloud storage resource accessible
#' via the 
#' [paws.storage](https://cran.r-project.org/package=paws.storage)
#' R package.
#' A URI for this type of resource uses the prefix \dQuote{s3://}.
#' 
#' @details 
#' The URI must correspond to an existing file in an AWS S3 compatible cloud
#' storage system.
#' 
#' For details about authentication, see section \dQuote{AWS Credentials} below.
#' 
#' For instance:
#' 
#' ```
#' s3://your-bucket/your-prefix/index.rds)
#' ```
#' 
#' @section Slot overview:
#' This class inherits all slots from its parent class \linkS4class{iSEEindexResource}.
#' 
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{iSEEindexS3Resource} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#' 
#' \itemize{
#' \item \code{\link{precache}(x, ..., temp_dir = tempdir())} trims the `s3://` prefix,
#' parses information encoded in the remainder of the URI,
#' downloads the resource from AWS S3 using that information,
#' and returns the local file path to the downloaded resource,
#' for use in the \pkg{BiocFileCache}.
#' }
#' 
#' @section Pre-caching:
#' Additional arguments to the \code{\link{precache}(x, ..., temp_dir = tempdir())}:
#' \describe{
#' \item{`...`}{Additional arguments passed on to the [paws.storage::s3()]}
#' \item{`temp_dir`}{Scalar character, the directory to store the downloaded file
#' in before it is handed over to \pkg{BiocFileCache}. This directory will be created
#' recursively if it doesn't already exist.}
#' }
#' 
#' @section AWS Credentials and region settings:
#' For detailed information, please consult the
#' [paws R package documentation](https://github.com/paws-r/paws/blob/main/docs/credentials.md).
#' 
#' Currently, you must have the [AWS Command Line Interface](https://aws.amazon.com/cli/) installed to use AWS SSO with \pkg{paws.storage}.
#' 
#' The AWS region can be set in the file `~/.aws/config`.
#' For instance:
#' 
#' ```
#' [default]
#' region=eu-west-2
#' ```
#' 
#' Credentials for all services can be set in the AWS shared credentials file `~/.aws/credentials`.
#' For instance:
#' 
#' ```
#' [default]
#' aws_access_key_id=your AWS access key
#' aws_secret_access_key=your AWS secret key
#' ```
#' 
#' @author Thomas Sandmann, Kevin Rue-Albrecht
#'
#' @name iSEEindexS3Resource-class
#' @rdname iSEEindexS3Resource-class
#' @aliases 
#' precache,iSEEindexS3Resource-method
NULL

#' @export
setClass("iSEEindexS3Resource", contains="iSEEindexResource")

#' @export
#' @importFrom urltools url_parse
#' @importFrom paws.storage s3
setMethod("precache", "iSEEindexS3Resource",
    function(x, bfc, id, ..., temp_dir = tempdir())
{
    # Trim 's3://' from the original URI and pass to paws.storage,
    # which will manage the download.
    # Pass the local filepath to BiocFileCache, which will cache the downloaded file.
    # Use action="move" to save time and disk space.
    uri <- urltools::url_parse(x@uri)
    if(uri$scheme != "s3") stop("URI scheme must be `s3`")
    
    # connect to S3 and download the file
    svc <- paws.storage::s3(...)
    dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
    fpath <- file.path(temp_dir, basename(uri$path))
    svc$download_file(Bucket = uri$domain, Key = uri$path,
        Filename = fpath)
    stopifnot(file.exists(fpath))
    object_path <- bfcadd(x = bfc, rname = id, fpath = fpath, action = "move", ...)
    return(object_path)
})
