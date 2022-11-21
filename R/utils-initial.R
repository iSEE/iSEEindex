#' Choices of Initial Configurations
#' 
#' `.initial_choices()` identifies choices of predefined initial app states
#' available for a given data set, and returns them as a named character vector
#' meant to populate a Shiny [selectizeInput()].
#'
#' @param x Data set identifier as a character scalar.
#' @param FUN.initial Function that returns a named character vector of files
#' that define initial configuration states for a given data set identifier.
#' 
#' @details 
#' A default option is automatically prepended to the choices for all data sets.
#' That default option launches an initial state that includes one panel of
#' each type compatible with the information present in the data set.
#'
#' @return A named character vector of choices for initial states of the
#' [iSEE()] app for that data set.
#' 
#' @export
#' @rdname INTERNAL_initial_choices
#'
#' @examples
#' .initial_choices("dummy_id")
.initial_choices <- function(x, FUN.initial) {
    # x: URI of the data set
    choices <- c("Default" = .initial_default_choice)
    if (!is.null(FUN.initial)) {
        choices <- c(choices, FUN.initial(x))
    }
    choices
}

#' Load an Initial Application State
#'
#' @param pObjects An environment containing global parameters generated in the
#' landing page.
#' 
#' @details 
#' Currently, this function always return `NULL`, representing the default
#' initial state that includes one panel of each type compatible with the
#' information present in the data set.
#'
#' @return A `list` of [Panel-class] representing an initial app state.
#'
#' @rdname INTERNAL_load_initial
.load_initial <- function(pObjects) {
    initial_choice <- pObjects[[.ui_initial]]
    if (identical(initial_choice, .initial_default_choice)) {
        initial <- NULL
    } else {
        initial <- NULL # TODO: support custom initial states
    }
    initial
}