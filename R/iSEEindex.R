#' iSEEindex app
#'
#' @param bfc An [BiocFileCache()] object.
#' @param FUN.datasets A function that returns a `data.frame` of metadata for
#' available data sets.
#' @param FUN.initial A function that returns a `data.frame` of metadata for
#' available initial configuration states.
#'
#' @return An [iSEE()] app with a custom landing page using a [BiocFileCache()] to cache a selection of data sets.
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
    dataset_uri <- pObjects$datasets_table[which_dataset, .datasets_uri, drop=TRUE]
    # TODO: refactor as function that takes data set identifier and returns label
    dataset_label <- pObjects$datasets_table[which_dataset, .datasets_label, drop=TRUE]
    withProgress(message = sprintf("Loading '%s'", dataset_label),
        value = 0, max = 2, {
        incProgress(1, detail = "(Down)loading object")
        se2 <- try(.load_sce(bfc, dataset_id, dataset_uri))
        incProgress(1, detail = "Launching iSEE app")
        if (is(se2, "try-error")) {
            showNotification("Invalid SummarizedExperiment supplied.", type="error")
        } else {
            if (is.null(pObjects$initial_table)) {
                init <- NULL
            } else {
                initial_id <- pObjects[[.ui_initial]]
                which_initial <- which(pObjects$initial_table[[.initial_config_id]] == initial_id)
                initial_uri <- pObjects$initial_table[which_initial, .initial_uri, drop=TRUE]
                initial_message <- capture.output(
                    init <- try(.load_initial(bfc, dataset_id, initial_id, initial_uri)),
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
