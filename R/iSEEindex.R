#' iSEEindex App
#'
#' @description
#' Generate an \pkg{iSEE} app that includes a landing page enabling
#' users to choose from a custom set of data sets and initial configuration
#' states prepared by the app maintainer.
#'
#' @section Data Sets:
#' The function passed to the argument `FUN.datasets` must return either a `data.frame` or a `list` that contains metadata about the available data sets.
#'
#' Required metadata are:
#'
#' \describe{
#' \item{id}{A unique identifier for the data set.}
#' \item{label}{A short human-readable title for the data set, displayed in the 'Info' panel when the data set is selected.}
#' \item{uri}{A Uniform Resource Identifier (URI) that indicates the location of the data file that contains the data set.}
#' \item{description}{A more detailed description of the data set, displayed in the 'Info' panel when the data set is selected.}
#' }
#'
#' **Important:** The `id` value is used to identify the data set file in the \pkg{BiocFileCache}.
#' Thus, we recommend using a dedicated `BiocFileCache()` for the app, using the `BiocFileCache(cache)` argument to specify an on-disk location (directory path) for the dedicated cache.
#'
#' Example `data.frame`:
#'
#' ```
#' data.frame(
#'   id = c("ID1", "ID2"),
#'   label = c("Dataset 01", "Dataset 02"),
#'   uri = c("https://example.com/1.rds", "https://example.com/2.rds"),
#'   description = c("My first data set.", "My second data set.")
#' )
#' ```
#'
#' The `data.frame` may also contain optional columns of metadata specific to individual [`iSEEindexResource-class`] classes (refer to the help page of those classes for details).
#' The value in optional columns can be left empty (`""`) for resource classes that do not require that information.
#'
#' Example `list`:
#'
#' ```
#' list(
#'   list(
#'      id = "ID1",
#'      label = "Dataset 01",
#'      uri = "https://example.com/1.rds",
#'      description = "My first data set."
#'   ),
#'   list(
#'      id = "ID2",
#'      label = "Dataset 02",
#'      uri = "https://example.com/1.rds",
#'      description = "My second data set."
#'   )
#' )
#' ```
#'
#' The individual sub-lists may also contain optional named metadata specific to individual [`iSEEindexResource-class`] classes (refer to the help page of those classes for details).
#'
#' @section Initial Configurations:
#' The function passed to the argument `FUN.initial` must return either a `data.frame` or a `list` that contains metadata about the available initial configurations, or `NULL` in the absence of any custom initial configuration (default settings will be applied to all data sets.).
#'
#' Required metadata are:
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
#'
#' **Important:** The `dataset_id` and `config_id` are combined to identify the initial configuration script and the associated data set in the \pkg{BiocFileCache}.
#'
#' Example `data.frame`:
#'
#' ```
#' data.frame(
#'   dataset_id = c("ID1", "ID1"),
#'   config_id = c("config01", config02"),
#'   label = c("Configuration 01", "Configuration 02"),
#'   uri = c("https://example.com/1.R", "https://example.com/2.R"),
#'   description = c("My first configuration.", "My second configuration.")
#' )
#' ```
#'
#' The `data.frame` may also contain optional columns of metadata specific to individual [`iSEEindexResource-class`] classes (refer to the help page of those classes for details).
#' The value in optional columns can be left empty (`""`) for resource classes that do not require that information.
#'
#' Example `list`:
#'
#' ```
#' list(
#'   list(
#'      dataset_id = "ID1",
#'      config_id = "config01",
#'      label = "Configuration 01",
#'      uri = "https://example.com/1.R",
#'      description = "My first configuration."
#'   ),
#'   list(
#'      dataset_id = "ID1",
#'      config_id = "config02",
#'      label = "Configuration 02",
#'      uri = "https://example.com/2.R",
#'      description = "My second configuration."
#'   )
#' )
#' ```
#'
#' The individual sub-lists may also contain optional named metadata specific to individual [`iSEEindexResource-class`] classes (refer to the help page of those classes for details).
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
#' # Using YAML ----
#'
#' dataset_fun <- function() {
#'     x <- yaml::read_yaml(system.file(package = "iSEEindex", "example.yaml"))
#'     x$datasets
#' }
#'
#' initial_fun <- function() {
#'     x <- yaml::read_yaml(system.file(package = "iSEEindex", "example.yaml"))
#'     x$initial
#' }
#'
#' app <- iSEEindex(bfc, dataset_fun, initial_fun)
#'
#' if (interactive()) {
#'     shiny::runApp(app, port = 1234)
#' }
#'
#' # Using CSV ---
#'
#' dataset_fun <- function() {
#'     x <- read.csv(system.file(package = "iSEEindex", "datasets.csv"))
#'     x$datasets
#' }
#'
#' initial_fun <- function() {
#'     x <- yaml::read.csv(system.file(package = "iSEEindex", "initial.csv"))
#'     x$initial
#' }
#'
#' app <- iSEEindex(bfc, dataset_fun, initial_fun)
#'
#' if (interactive()) {
#'     shiny::runApp(app, port = 1234)
#' }
#'
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
