#' iSEEindex App
#'
#' @description
#' Generate an \pkg{iSEE} app that includes a landing page enabling
#' users to choose from a custom set of data sets and initial configuration
#' states prepared by the app maintainer.
#'
#' @section Data Sets:
#' The function passed to the argument `FUN.datasets` must return a `list` that contains metadata about the available data sets.
#'
#' For each data set, required metadata are:
#'
#' \describe{
#' \item{id}{A unique identifier for the data set.}
#' \item{title}{A short human-readable title for the data set, displayed in the 'Info' panel when the data set is selected.}
#' \item{uri}{A Uniform Resource Identifier (URI) that indicates the location of the data file that contains the data set.}
#' \item{description}{A more detailed description of the data set, displayed in the 'Info' panel when the data set is selected.}
#' }
#'
#' Example:
#'
#' ```
#' list(
#'   list(
#'      id = "dataset01",
#'      title = "Dataset 01",
#'      uri = "https://example.com/1.rds",
#'      description = "My first data set."
#'   ),
#'   list(
#'      id = "dataset02",
#'      title = "Dataset 02",
#'      uri = "https://example.com/1.rds",
#'      description = "My second data set."
#'   )
#' )
#' ```
#'
#' The individual sub-lists may also contain optional named metadata specific to individual [`iSEEindexResource-class`] classes (refer to the help page of those classes for details).
#'
#' **Important**: The `id` value is used to identify the data set file in the \pkg{BiocFileCache}.
#' Thus, we recommend using a dedicated `BiocFileCache()` for the app, using the `BiocFileCache(cache)` argument to specify an on-disk location (directory path) for the dedicated cache.
#'
#' @section Initial Configurations:
#' The function passed to the argument `FUN.initial` must return a `list` that contains metadata about the available initial configurations, or `NULL` in the absence of any custom initial configuration (default settings will be applied to all data sets.).
#'
#' For each initial configuration, required metadata are:
#'
#' \describe{
#' \item{id}{A unique identifier for the initial configuration.}
#' \item{title}{A short human-readable title for the initial configuration, representing the initial configuration in the 'Initial settings' dropdown menu.}
#' \item{uri}{A Uniform Resource Identifier (URI) that indicates the location of the R script that contains the initial configuration.}
#' \item{description}{A more detailed description of the initial configuration, displayed in the 'Configure and launch' panel when the initial configuration is selected.}
#' }
#'
#' For each initial configuration, optional metadata are:
#' \describe{
#' \item{datasets}{A series of data set identifiers for which the configuration should be made available. If missing, the configuration will be available for all data sets.}
#' }
#'
#' Example:
#'
#' ```
#' list(
#'   list(
#'      id = "config01",
#'      datasets = c("dataset01")
#'      title = "Configuration 01",
#'      uri = "https://example.com/1.R",
#'      description = "My first configuration."
#'   ),
#'   list(
#'      id = "config02",
#'      title = "Configuration 02",
#'      uri = "https://example.com/2.R",
#'      description = "My second configuration."
#'   )
#' )
#' ```
#'
#' The individual sub-lists may also contain additional optional named metadata specific to individual [`iSEEindexResource-class`] classes (refer to the help page of those classes for details).
#'
#' @param bfc An [BiocFileCache()] object.
#' @param FUN.datasets A function that returns a `list` of metadata for
#' available data sets.
#' @param FUN.initial A function that returns a `list` of metadata for
#' available initial configuration states.
#' @param default.add Logical scalar indicating whether a default
#' initial configuration should be added as a choice in the Shiny `selectizeInput()`.
#' See [iSEEindex()].
#' @param default.position Character scalar indicating whether the default
#' initial configuration should be added as the `"first"` or `"last"` option
#' in the Shiny `selectizeInput()`.
#' @param app.title Character string to specify the desired title to be displayed
#' in the main window of the dashboard. Defaults to `NULL`, which displays some
#' info on the versions of the `iSEEindex` and `iSEE` packages.
#' @param body.header UI element to display \emph{above} the main landing page body.
#' @param body.footer UI element to display \emph{below} the main landing page body.
#'
#' @return An [iSEE::iSEE()] app with a custom landing page using a [BiocFileCache()] to cache a selection of data sets.
#'
#' @author Kevin Rue-Albrecht
#'
#' @export
#'
#' @importFrom iSEE iSEE
#' @importFrom utils packageVersion
#'
#' @examples
#' ##
#' # BiocFileCache ----
#' ##
#'
#' library(BiocFileCache)
#' bfc <- BiocFileCache(cache = tempdir())
#'
#' ##
#' # iSEEindex ----
#' ##
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
iSEEindex <- function(bfc, FUN.datasets, FUN.initial = NULL, default.add = TRUE, default.position = c("first", "last"), app.title = NULL, body.header = NULL, body.footer = NULL) {
    stopifnot(is(bfc, "BiocFileCache"))
    if (is.null(FUN.initial)) {
        FUN.initial <- function() NULL
    }

    if (is.null(app.title)) {
        app.title <- sprintf("iSEEindex - v%s | powered by iSEE - v%s",
                             packageVersion("iSEEindex"),
                             packageVersion("iSEE"))
    }

    iSEE(
        landingPage=.landing_page(bfc, FUN.datasets, FUN.initial, default.add, default.position, body.header, body.footer),
        appTitle = app.title
        )
}

#' @examples
#' ##
#' # BiocFileCache ----
#' ##
#'
#' library(BiocFileCache)
#' bfc <- BiocFileCache(cache = tempdir())
#'
#' ##
#' # iSEEindex ----
#' ##
#'
#' dataset_fun <- function() {
#'     x <- yaml::read_yaml(
#'       system.file("tonsils_example", "tonsil_package.yml", package = "iSEEindex")
#'     )
#'     x$datasets
#' }
#'
#' initial_fun <- function() {
#'     x <- yaml::read_yaml(
#'       system.file("tonsils_example", "tonsil_package.yml", package = "iSEEindex")
#'     )
#'     x$initial
#' }
#'
#' library("shiny")
#' header_tonsils <- fluidRow(
#'   shinydashboard::box(
#'     width = 12,
#'     collapsible = TRUE,
#'     collapsed = TRUE,
#'     title = "How to explore the Tonsil Atlas datasets",
#'     includeMarkdown(
#'       system.file("tonsils_example", "header_tonsils.md", package = "iSEEindex")
#'     )
#'   )
#' )
#'
#' footer_tonsils <- fluidRow(
#'   shinydashboard::box(
#'     width = 12,
#'     includeMarkdown(
#'       system.file("tonsils_example", "footer_tonsils.md", package = "iSEEindex")
#'     )
#'   )
#' )
#'
#' app <- iSEEindex_runr(bfc, dataset_fun, initial_fun,
#'                       default.add = TRUE,
#'                       default.position = "last",
#'                       app.title = "iSEE ❤️ Tonsil Data Atlas",
#'                       body.header = header_tonsils,
#'                       body.footer = footer_tonsils)
#'
#' if (interactive()) {
#'     shiny::runApp(app, port = 1234)
#' }
iSEEindex_runr <- function(bfc,
                           FUN.datasets,
                           FUN.initial = NULL,
                           default.add = TRUE, default.position = c("first", "last"),
                           app.title = NULL,
                           body.header = NULL,
                           body.footer = NULL) {
  stopifnot(is(bfc, "BiocFileCache"))
  if (is.null(FUN.initial)) {
    FUN.initial <- function() NULL
  }

  if (is.null(app.title)) {
    app.title <- sprintf("iSEEindex - v%s",
                         packageVersion("iSEEindex"))
  } else {
    app.title <- app.title
  }

  iSEE(
    landingPage=.landing_page_runr(bfc,
                                   FUN.datasets,
                                   FUN.initial,
                                   default.add, default.position,
                                   body.header,
                                   body.footer),
    appTitle = app.title
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
    # TODO: refactor as function that takes data set identifier and returns title
    dataset_title <- pObjects$datasets_table[which_dataset, .datasets_title, drop=TRUE]
    withProgress(message = sprintf("Loading '%s'", dataset_title),
        value = 0, max = 2, {
        incProgress(1, detail = "(Down)loading object")
        se2 <- try(.load_sce(bfc, dataset_id, dataset_metadata))
        incProgress(1, detail = "Launching iSEE app")
        if (is(se2, "try-error")) {
            showNotification("Invalid SummarizedExperiment supplied.", type="error")
        } else {
            if (is.null(pObjects$initial_table)) {
              initial <- NULL
              tour <- NULL
            } else {
                initial_id <- pObjects[[.ui_initial]]
                which_initial <- which(
                  pObjects$initial_table[[.initial_config_id]] == initial_id &
                  pObjects$initial_table[[.initial_datasets_id]] == dataset_id
                  )
                initial_metadata <- as.list(pObjects$initial_table[which_initial, , drop = FALSE])
                initial_message <- capture.output(
                    init <- try(.parse_initial(bfc, dataset_id, initial_id, initial_metadata)),
                    type = "message")
                initial <- init$initial
                tour <- init$tour
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
            FUN(SE=se2, INITIAL=initial, TOUR=tour)
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






.launch_isee_runr <- function(FUN, bfc, session, pObjects) {
  # nocov start
  dataset_id <- pObjects[[.dataset_selected_id]]
  which_dataset <- which(pObjects$datasets_table[[.datasets_id]] == dataset_id)
  # TODO: refactor as function that takes data set identifier and returns uri
  dataset_metadata <- as.list(pObjects$datasets_table[which_dataset, , drop=FALSE])
  # TODO: refactor as function that takes data set identifier and returns title
  dataset_title <- pObjects$datasets_table[which_dataset, .datasets_title, drop=TRUE]
  withProgress(message = sprintf("Loading '%s'", dataset_title),
               value = 0, max = 2, {
                 incProgress(1, detail = "(Down)loading object")


                 # se2 <- try(.load_sce_runr(bfc, dataset_id, dataset_metadata))
                 se2 <- try(.load_sce_runr(bfc, dataset_id, dataset_metadata))





                 incProgress(1, detail = "Launching iSEE app")
                 if (is(se2, "try-error")) {
                   showNotification("Invalid SummarizedExperiment supplied.", type="error")
                 } else {
                   if (is.null(pObjects$initial_table)) {
                     initial <- NULL
                     tour <- NULL
                   } else {
                     initial_id <- pObjects[[.ui_initial]]
                     which_initial <- which(
                       pObjects$initial_table[[.initial_config_id]] == initial_id &
                         pObjects$initial_table[[.initial_datasets_id]] == dataset_id
                     )
                     initial_metadata <- as.list(pObjects$initial_table[which_initial, , drop = FALSE])
                     initial_message <- capture.output(
                       init <- try(.parse_initial(bfc, dataset_id, initial_id, initial_metadata)),
                       type = "message")
                     initial <- init$initial
                     tour <- init$tour
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
                   FUN(SE=se2, INITIAL=initial, TOUR=tour)
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
