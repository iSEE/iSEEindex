# iSEEindexResource ----

#' The iSEEindexResource class
#'
#' The iSEEindexResource class is a virtual class from which classes of supported resource must be derived.
#'
#' @section Slot overview:
#' \itemize{
#' \item \code{uri}, a character scalar specifying the URI of a resource.
#' }
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a [`iSEEindexResource-class`] class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' \itemize{
#' \item \code{\link{precache}(x, bfc, id, ...)} throws an error, encouraging users to develop a method for derived classes that are not supported yet.
#' }
#'
#' @author Kevin Rue-Albrecht
#'
#' @name iSEEindexResource-class
#' @rdname iSEEindexResource-class
#' @aliases
#' precache,iSEEindexResource-method
#' show,iSEEindexResource-method
#'
#' @examples
#' showClass("iSEEindexResource")
NULL

#' @export
setClass("iSEEindexResource",
    slots=c(
        uri="character"
    ))

#' @export
#' @rdname iSEEindexResource-class
#'
#' @param object An `iSEEindexResource` object.
#'
#' @return `show()` returns `NULL` after displaying a summary of the object.
setMethod("show", "iSEEindexResource",
    function(object)
{
    cat("class:", class(object), "\n")
    cat("- uri:", object@uri, "\n")
})

#' Generics for iSEEindexResources Objects
#'
#' An overview of the generics for `iSEEindexResources` objects.
#'
#' @param x An [`iSEEindexResource-class`] object.
#' @param bfc A [BiocFileCache()] object.
#' @param id A data set identifier as a character scalar.
#' @param ... additional arguments passed to and from other methods.
#'
#' @section Preparing and caching resources:
#' `precache(x, bfc, id, ...)` retrieves and caches a resource from an URI, caches it, and returns the path to the cached file.
#'
#' @author Kevin Rue-Albrecht
#'
#' @docType methods
#' @aliases precache
#' @name iSEEindexResource-generics
#'
#' @examples
#' library(BiocFileCache)
#' bfc <- BiocFileCache(cache = tempdir())
#'
#' x <- iSEEindexRcallResource(list(
#'   uri = "rcall://system.file(package='iSEEindex','ReprocessedAllenData_config_01.R')"
#' ))
#' precache(x, bfc, "ID0")
NULL

#' @export
#' @rdname iSEEindexResource-generics
#'
#' @return `precache()` returns the file path to the cached copy of a resource fetched from a given URI.
setGeneric("precache", function(x, bfc, id, ...) {
    stopifnot(is(x, "iSEEindexResource"), length(x) == 1L)
    standardGeneric("precache")
})

#' @export
#' @rdname iSEEindexResource-class
#'
#' @param x An [`iSEEindexResource-class`] object.
#' @param bfc A [BiocFileCache()] object.
#' @param id A data set identifier as a character scalar.
#' @param ... additional arguments passed to and from other methods.
#'
#' @return `precache()` throws an error if no method is found for the derived class.
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
#' @details
#' Required metadata:
#'
#' \describe{
#' \item{uri}{Character scalar. URI of the resource.}
#' }
#'
#' @section Slot overview:
#' This class inherits all slots from its parent class \linkS4class{iSEEindexResource}.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{iSEEindexHttpsResource} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' \itemize{
#' \item \code{\link{precache}(x, bfc, id, ...)} caches the resource located at the given URI using \pkg{BiocFileCache} and returns the file path to the cached file.
#' }
#'
#' @author Kevin Rue-Albrecht
#'
#' @name iSEEindexHttpsResource-class
#' @rdname iSEEindexHttpsResource-class
#' @aliases
#' precache,iSEEindexHttpsResource-method
#'
#' @examples
#' iSEEindexHttpsResource(list(uri = "https://example.com"))
NULL

#' @export
setClass("iSEEindexHttpsResource", contains="iSEEindexResource")

#' @export
#' @rdname iSEEindexHttpsResource-class
#'
#' @param x List of metadata. See Details.
#'
#' @return The constructor function `iSEEindexHttpsResource()` returns an object of object of class `iSEEindexHttpsResource`.
iSEEindexHttpsResource <- function(x) {
    new("iSEEindexHttpsResource", uri = x[[.datasets_uri]])
}

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
#' Required metadata:
#'
#' \describe{
#' \item{uri}{Character scalar. URI of the resource.}
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
#' \item \code{\link{precache}(x, ...)} trims the `localhost://` prefix, and caches a copy of the resource located at the resulting file path using \pkg{BiocFileCache}, before returning the file path to the cached file.
#' }
#'
#' @section Absolute and relative paths:
#'
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
#' @author Kevin Rue-Albrecht
#'
#' @name iSEEindexLocalhostResource-class
#' @rdname iSEEindexLocalhostResource-class
#' @aliases
#' precache,iSEEindexLocalhostResource-method
#'
#' @examples
#' iSEEindexLocalhostResource(list(uri = "localhost:///example/absolute/path"))
#' iSEEindexLocalhostResource(list(uri = "localhost://example/relative/path"))
NULL

#' @export
setClass("iSEEindexLocalhostResource", contains="iSEEindexResource")

#' @export
#' @rdname iSEEindexLocalhostResource-class
#'
#' @param x List of metadata. See Details.
#'
#' @return The constructor function `iSEEindexLocalhostResource()` returns an object of object of class `iSEEindexLocalhostResource`.
iSEEindexLocalhostResource <- function(x) {
    new("iSEEindexLocalhostResource", uri = x[[.datasets_uri]])
}

#' @export
setMethod("precache", "iSEEindexLocalhostResource",
    function(x, bfc, id, ...)
{
    # Trim 'localhost://' from the original URI and pass to BiocFileCache,
    # which will manage the caching.
    # Use action="copy" to leave the original file untouched.
    fpath <- sub("localhost://", "", x@uri)
    stopifnot(file.exists(fpath))
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
#' Required metadata:
#'
#' \describe{
#' \item{uri}{Character scalar. R call which, once evaluated, produces a character scalar that is the URI of the resource.}
#' }
#'
#' @section URI format:
#' The URI must contain valid R code, once the prefix `rcall://` is removed.
#' The code must return the path to an existing file on the local filesystem.
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
#' evaluates the remainder of the URI as R code, and caches a copy of the
#' resource located at the resulting file path using \pkg{BiocFileCache},
#' before returning the file path to the cached file.
#' }
#'
#' @author Kevin Rue-Albrecht
#'
#' @name iSEEindexRcallResource-class
#' @rdname iSEEindexRcallResource-class
#' @aliases
#' precache,iSEEindexRcallResource-method
#'
#' @examples
#' iSEEindexRcallResource(list(
#'   uri = "rcall://system.file(package='iSEEindex','ReprocessedAllenData_config_01.R')"
#' ))
NULL

#' @export
setClass("iSEEindexRcallResource", contains="iSEEindexResource")

#' @export
#' @rdname iSEEindexRcallResource-class
#'
#' @param x List of metadata. See Details.
#'
#' @return The constructor function `iSEEindexRcallResource()` returns an object of object of class `iSEEindexRcallResource`.
iSEEindexRcallResource <- function(x) {
    new("iSEEindexRcallResource", uri = x[[.datasets_uri]])
}

#' @export
setMethod("precache", "iSEEindexRcallResource",
    function(x, bfc, id, ...)
{
    # Trim 'rcall://' from the original URI and evaluate the R call,
    # check that the value is an existing filepath and pass to BiocFileCache,
    # which will manage the caching.
    # Use action="copy" to leave the original file untouched.
    call_string <- sub("rcall://", "", x@uri)
    env <- new.env()
    fpath <- eval(parse(text = call_string), envir = env)
    stopifnot(file.exists(fpath))
    object_path <- bfcadd(x = bfc, rname = id, fpath = fpath, action = "copy", ...)
    return(object_path)
})



# iSEEindexRunrResource ----

#' The iSEEindexRunrResource class
#'
#' The iSEEindexRunrResource class represents an SE object, obtained directly
#' through an R call.
#'
#' A URI for this type of resource uses the prefix \dQuote{runr://}.
#'
#' @details
#' Required metadata:
#'
#'
#' @examples
#' iSEEindexRunrResource(list(
#'   uri = "runr://HCATonsilData::HCATonsilData(assayType = 'RNA', cellType = 'epithelial')"
#' ))
NULL

#' @export
setClass("iSEEindexRunrResource", contains="iSEEindexResource")

#' @export
#' @rdname iSEEindexRunrResource-class
#'
#' @param x List of metadata. See Details.
#'
#' @return The constructor function `iSEEindexRunrResource()` returns an object of object of class `iSEEindexRunrResource`.
iSEEindexRunrResource <- function(x) {
  new("iSEEindexRunrResource", uri = x[[.datasets_uri]])
}

#' @export
setMethod("precache", "iSEEindexRunrResource",
          function(x, bfc, id, ...)
          {
            # Trim 'runr://' from the original URI and evaluate the R call,
            # check that the value is an existing filepath and pass to BiocFileCache,
            # which will manage the caching.
            # Use action="copy" to leave the original file untouched.
            call_string <- sub("runr://", "", x@uri)
            env <- new.env()

            # fpath not needed per se, we should have a "valid r call and that is it"

            object_path <- eval(parse(text = call_string))


            # fpath <- eval(parse(text = call_string), envir = env)
            # stopifnot(file.exists(fpath))
            # object_path <- bfcadd(x = bfc, rname = id, fpath = fpath, action = "copy", ...)




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
#' Required metadata:
#'
#' \describe{
#' \item{uri}{Character scalar. URI of the resource.}
#' }
#'
#' @section Slot overview:
#' This class inherits all slots from its parent class \linkS4class{iSEEindexResource}.
#'
#' Furthermore, this class defines the additional slot(s) below:
#' \describe{
#' \item{region}{AWS region.}
#' }
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{iSEEindexS3Resource} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' \itemize{
#' \item \code{\link{precache}(x, ..., temp_dir = tempdir())} trims the `s3://` prefix,
#' parses information encoded in the remainder of the URI,
#' downloads the resource from AWS S3 using that information,
#' and caches a copy of the resource located at the resulting file path using
#' \pkg{BiocFileCache}, before returning the file path to the cached file.
#' }
#'
#' @section URI format:
#' The URI must correspond to an existing file in an AWS S3 compatible cloud
#' storage system.
#'
#' For instance:
#'
#' ```
#' s3://bucket/prefix/index.rds
#' ```
#'
#' For details about authentication, see section \dQuote{AWS Credentials} below.
#'
#' @section Pre-caching:
#' Additional arguments to the \code{\link{precache}(x, ..., temp_dir = tempdir())}:
#'
#' \describe{
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
#' A default AWS region can be set in the file `~/.aws/config`.
#' For instance:
#'
#' ```
#' [default]
#' region=eu-west-2
#' ```
#'
#' Optionally, a field named `region` can be added in the list of resource metadata to set the AWS S3 region for each individual resource, e.g.
#'
#' ```
#' - id: ID1
#'   title: ReprocessedAllenData
#'   uri: s3://example/ReprocessedAllenData.rds
#'   description: |
#'     Reprocessed Allen Data.
#'   region: eu-west-2
#' ```
#'
#' Regions set in individual resource metadata override the default AWS region set in `~/.aws/config` (if any).
#' The region metadata does not need to be set for resources that should use the default region, and resource classes that do not require region information.
#'
#' If a default region is NOT set in `~/.aws/config`, then the region MUST be set in the metadata.
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
#' iSEEindexS3Resource
#' precache,iSEEindexS3Resource-method
#'
#' @examples
#' # Without region metadata
#' metadata <- list(uri = "s3://example/path/to/bucket")
#' x <- iSEEindexS3Resource(metadata)
#' str(x)
#'
#' # With region metadata
#' # NOTE: The @region slot is set to NA pending bugfix (see above).
#' metadata <- list(uri = "s3://example/path/to/bucket", region = "eu-west-2")
#' x <- iSEEindexS3Resource(metadata)
#' str(x)
NULL

#' @export
setClass("iSEEindexS3Resource", contains="iSEEindexResource", slots = c("region" = "character"))

#' @export
#' @rdname iSEEindexS3Resource-class
#'
#' @param x List of metadata. See Details.
#'
#' @return The constructor function `iSEEindexS3Resource()` returns an object of object of class `iSEEindexS3Resource`.
iSEEindexS3Resource <- function(x) {
    region <- x[[.dataset_region]]
    if (is.null(region) || identical(nchar(region), 0L)) {
        region <- NA_character_
    }
    new("iSEEindexS3Resource", uri = x[[.datasets_uri]], region = region)
}

#' @export
#' @importFrom urltools url_parse
#' @importFrom paws.storage s3
setMethod("precache", "iSEEindexS3Resource",
    function(x, bfc, id, ..., temp_dir = tempdir())
{
    # nocov start
    # Trim 's3://' from the original URI and pass to paws.storage,
    # which will manage the download.
    # Pass the local filepath to BiocFileCache, which will cache the downloaded file.
    # Use action="move" to save time and disk space.
    uri <- urltools::url_parse(x@uri)
    if(uri$scheme != "s3") stop("URI scheme must be `s3`")

    # connect to S3 and download the file
    aws_config <- list()
    if (!is.na(x@region)) {
        aws_config$region <- x@region
    }
    svc <- paws.storage::s3(config = aws_config)
    dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
    fpath <- file.path(temp_dir, basename(uri$path))
    svc$download_file(Bucket = uri$domain, Key = uri$path, Filename = fpath)
    stopifnot(file.exists(fpath))
    object_path <- bfcadd(x = bfc, rname = id, fpath = fpath, action = "move", ...)
    return(object_path)
    # nocov end
})
