#' Observers for iSEEindex
#'
#' @description
#'
#' `.create_observers()` initialises observers for the \pkg{iSEEindex} landing page.
#'
#' `.create_launch_observers()` initialises observers for launching the \pkg{iSEE} main app.
#'
#' @param input The Shiny input object from the server function.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the landing page.
#' @param rObjects A reactive list of values generated in the landing page.
#' @param FUN.initial A function that returns available scripts for initial configurations states for a given data set identifier.
#' @param default.add Logical scalar indicating whether a default
#' initial configuration should be added as a choice in the Shiny `selectizeInput()`.
#' See [iSEEindex()].
#' @param default.position Character scalar indicating whether the default
#' initial configuration should be added as the `"first"` or `"last"` option
#' in the Shiny `selectizeInput()`.
#'
#' @return
#' Those functions create observers in the server function in which they are called.
#' In all cases, a \code{NULL} value is invisibly returned.
#'
#' @author Kevin Rue-Albrecht
#'
#' @importFrom shiny isolate observeEvent updateSelectizeInput
#' @importFrom rintrojs introjs
#'
#' @rdname INTERNAL_create_observers
.create_observers <- function(input, session, pObjects, rObjects, FUN.initial, default.add, default.position) {

    # nocov start
    observeEvent(input[[.dataset_selected_row]], {
        dataset_selected_id <- pObjects$datasets_table[[.datasets_id]][input[[.dataset_selected_row]]]
        pObjects[[.dataset_selected_id]] <- dataset_selected_id
        rObjects$rerender_overview <- iSEE:::.increment_counter(isolate(rObjects$rerender_overview))
        initial_choices <- .initial_choices(dataset_selected_id, pObjects$initial_table, default.add, default.position)
        updateSelectizeInput(session, .ui_initial, choices = initial_choices)
    }, ignoreInit = FALSE, ignoreNULL = FALSE)
    # nocov end

    # nocov start
    observeEvent(input[[.ui_dataset_columns]], {
        pObjects[[.ui_dataset_columns]] <- input[[.ui_dataset_columns]]
        rObjects$rerender_datasets <- iSEE:::.increment_counter(isolate(rObjects$rerender_datasets))
    })
    # nocov end

    # nocov start
    observeEvent(input[[iSEE:::.generalTourSteps]], {
        introjs(session, options=list(steps=.landing_page_tour))
    }, ignoreInit=TRUE)
    # nocov end

    # nocov start
    observeEvent(input[[.ui_initial]], {
        pObjects[[.ui_initial]] <- input[[.ui_initial]]
        rObjects$rerender_initial <- iSEE:::.increment_counter(isolate(rObjects$rerender_initial))
    })
    # nocov end

    invisible(NULL)
}

#' @param FUN A function to initialize the \pkg{iSEE} observer
#' architecture. Refer to [iSEE::createLandingPage()] for more details.
#'
#' @param bfc A [BiocFileCache()] object.
#' landing page.
#' @param input TODO
#' @param session TODO
#' @param pObjects TODO
#'
#' @importFrom shiny observeEvent
#'
#' @rdname INTERNAL_create_observers
.create_launch_observers <- function(FUN, bfc, input, session, pObjects) {

    # nocov start
    observeEvent(input[[.ui_launch_button]], {
        .launch_isee(FUN, bfc, session, pObjects)
    }, ignoreNULL=TRUE, ignoreInit=TRUE)
    # nocov end

    invisible(NULL)
}


#
# .create_launch_observers_runr <- function(FUN, bfc, input, session, pObjects) {
#
#   # nocov start
#   observeEvent(input[[.ui_launch_button]], {
#     .launch_isee(FUN, bfc, session, pObjects, already_se_object = TRUE)
#   }, ignoreNULL=TRUE, ignoreInit=TRUE)
#   # nocov end
#
#   invisible(NULL)
# }

