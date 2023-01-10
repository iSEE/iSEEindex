#' iSEEindex App
#' 
#' @description
#' Generate an \pkg{iSEE} app that includes a landing page enabling
#' users to choose from a custom set of data sets and initial configuration
#' states prepared by the app maintainer.
#' 
#' @section Data Sets:
#' The function passed to the argument `FUN.datasets` must return a `data.frame` that contains the following columns:
#' 
#' \describe{
#' \item{id}{A unique identifier for the data set.}
#' \item{label}{A short human-readable title for the data set, displayed in the 'Info' panel when the data set is selected.}
#' \item{uri}{A Uniform Resource Identifier (URI) that indicates the location of the data file that contains the data set.}
#' \item{description}{A more detailed description of the data set, displayed in the 'Info' panel when the data set is selected.}
#' }
#' 
#' The `id` is used to identify the data set file in the \pkg{BiocFileCache}.
#' Thus, we recommend using a dedicated `BiocFileCache()` for the app, using the `BiocFileCache(cache)` to specify an on-disk location (directory path) for the dedicated cache.
#' 
#' @section Initial Configurations:
#' The function passed to the argument `FUN.initial` must return a `data.frame` that contains the following columns:
#' 
#' \describe{
#' \item{dataset_id}{The unique identifier of a data set.}
#' \item{config_id}{A unique identifier for the initial configuration.}
#' \item{label}{A short human-readable title for the initial configuration, representing the initial configuration in the 'Initial settings' dropdown menu.}
#' \item{uri}{A Uniform Resource Identifier (URI) that indicates the location of the R script that contains the initial configuration.}
#' \item{description}{A more detailed description of the initial configuration, displayed in the 'Configure and launch' panel when the initial configuration is selected.}
#' }
#' 
#' The `dataset_id` must match one of the `id` values in the data set metadata.
#' See section 'Data Sets'.
#' 
#' The same `config_id` may be re-used in combination with different `dataset_id`.
#' The `dataset_id` and `initial_id` are combined to identify the initial configuration script and the associated data set in the \pkg{BiocFileCache}.
#'
#' @param bfc An [BiocFileCache()] object.
#' @param FUN.datasets A function that returns a `data.frame` of metadata for
#' available data sets.
#' @param FUN.initial A function that returns a `data.frame` of metadata for
#' available initial configuration states.
#'
#' @return An [iSEE()] app with a custom landing page using a [BiocFileCache()] to cache a selection of data sets.
#' 
#' @author Kevin Rue-Albrecht
#'
#' @export
#'
#' @importFrom iSEE iSEE
#' @importFrom utils packageVersion
#'
#' @examples
#' library(BiocFileCache)
#' bfc <- BiocFileCache(cache = tempdir())
#' 
#' dataset_fun <- function() {
#'     read.csv(system.file(package="iSEEindex", "datasets.csv"))
#' }
#' 
#' initial_fun <- function() {
#'     read.csv(system.file(package = "iSEEindex", "initial.csv"))
#' }
#' 
#' app <- iSEEindex(bfc, dataset_fun, initial_fun)
#' 
#' if (interactive()) {
#'   shiny::runApp(app, port = 1234)
#' }
iSEEindex <- function(bfc, FUN.datasets, FUN.initial = NULL) {
    stopifnot(is(bfc, "BiocFileCache"))
    if (is.null(FUN.initial)) {
        FUN.initial <- function() NULL
    }
    iSEE(
        landingPage=.landing_page(bfc, FUN.datasets, FUN.initial),
        appTitle = sprintf("iSEEindex - v%s",
            packageVersion("iSEEindex")
            )
        )
}

#' Prepare and Launch the Main App.
#' 
#' Invokes a function that replaces the landing page by the \pkg{iSEE}
#' interactive dashboard.
#'
#' @details
#' This function wraps steps that can be tracked and reported to the
#' user through a progress bar, using [shiny::withProgress()].
#'
#' @section Unit testing:
#' This function cannot be unit tested (yet), as [shiny::withProgress()]
#' requires a functional `ShinySession` object.
#'
#' This might be revisited in the future.
#'
#' @param FUN A function to initialize the \pkg{iSEE} observer
#' architecture. Refer to [iSEE::createLandingPage()] for more details.
#' @param bfc An [BiocFileCache()] object.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the
#' landing page.
#'
#' @return A `NULL` value is invisibly returned.
#' 
#' @author Kevin Rue-Albrecht
#'
#' @importFrom utils capture.output
#' @importFrom shiny incProgress markdown modalDialog p showModal
#' showNotification withProgress
#' @importFrom shinyjs enable
#'
#' @rdname INTERNAL_launch_isee
.launch_isee <- function(FUN, bfc, session, pObjects) {
    # nocov start
    dataset_id <- pObjects[[.dataset_selected_id]]
    which_dataset <- which(pObjects$datasets_table[[.datasets_id]] == dataset_id)
    # TODO: refactor as function that takes data set identifier and returns uri
    dataset_metadata <- as.list(pObjects$datasets_table[which_dataset, , drop=FALSE])
    # TODO: refactor as function that takes data set identifier and returns label
    dataset_label <- pObjects$datasets_table[which_dataset, .datasets_label, drop=TRUE]
    withProgress(message = sprintf("Loading '%s'", dataset_label),
        value = 0, max = 2, {
        incProgress(1, detail = "(Down)loading object")
        se2 <- try(.load_sce(bfc, dataset_id, dataset_metadata))
        incProgress(1, detail = "Launching iSEE app")
        if (is(se2, "try-error")) {
            showNotification("Invalid SummarizedExperiment supplied.", type="error")
        } else {
            if (is.null(pObjects$initial_table)) {
                init <- NULL
            } else {
                initial_id <- pObjects[[.ui_initial]]
                which_initial <- which(pObjects$initial_table[[.initial_config_id]] == initial_id)
                initial_metadata <- as.list(pObjects$initial_table[which_initial, , drop = FALSE])
                initial_message <- capture.output(
                    init <- try(.load_initial(bfc, dataset_id, initial_id, initial_metadata)),
                    type = "message")
            }
            if (is(init, "try-error")) {
                showModal(modalDialog(
                    title = "Invalid initial state",
                    p("An error occured while evaluating the script:"),
                    markdown(paste0(c("```", initial_message, "```"), collapse = "\n")),
                    p("Contact the app maintainer for further help."),
                    footer = NULL,
                    size = "l",
                    easyClose = TRUE
                  ))
                return(NULL)
            }
            FUN(SE=se2, INITIAL=init)
            shinyjs::enable(iSEE:::.generalOrganizePanels) # organize panels
            shinyjs::enable(iSEE:::.generalLinkGraph) # link graph
            shinyjs::enable(iSEE:::.generalExportOutput) # export content
            shinyjs::enable(iSEE:::.generalCodeTracker) # tracked code
            shinyjs::enable(iSEE:::.generalPanelSettings) # panel settings
            shinyjs::enable(iSEE:::.generalVignetteOpen) # open vignette
            shinyjs::enable(iSEE:::.generalSessionInfo) # session info
            shinyjs::enable(iSEE:::.generalCitationInfo) # citation info
        }
    }, session = session)

    invisible(NULL)
    # nocov end
}
