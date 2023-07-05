#' Choices of Initial Configurations
#'
#' `.initial_choices()` identifies choices of predefined initial app states
#' available for a given data set, and returns them as a named character vector
#' meant to populate a Shiny [selectizeInput()].
#'
#' @param id Data set identifier, as a character scalar.
#' @param available Metadata for available initial configurations, as `data.frame`.
#' @param default.add Logical scalar indicating whether a default
#' initial configuration should be added as a choice in the Shiny `selectizeInput()`.
#' See Details.
#' @param default.position Character scalar indicating whether the default
#' initial configuration should be added as the `"first"` or `"last"` option
#' in the Shiny `selectizeInput()`.
#'
#' @details
#' An option representing a default initial configuration can be automatically
#' added to the choices for all data sets.
#' That default option launches an initial state that includes one panel of
#' each type compatible with the information present in the data set.
#' 
#' The default initial configuration is always added for data sets which are not
#' associated with any custom initial configuration.
#' 
#' The option `default.add` controls whether the default initial configuration
#' is added to the choices for data sets associated with at least one custom
#' initial configuration.
#'
#' @return A named character vector of choices for initial states of the
#' [iSEE()] app for that data set.
#'
#' @author Kevin Rue-Albrecht
#'
#' @rdname INTERNAL_initial_choices
.initial_choices <- function(id, available, default.add = TRUE, default.position = c("first", "last")) {
  default.position <- match.arg(default.position)
  which_initial <- available[[.initial_datasets_id]] == id
  config_subset_table <- available[which_initial, , drop=FALSE]
  initial_choices <- config_subset_table[[.initial_config_id]]
  names(initial_choices) <- config_subset_table[[.initial_title]]
  
  if (default.add) {
    default_choice <- c("Default" = .initial_default_choice)
    if (identical(default.position, "first")) {
      initial_choices <- c(default_choice, initial_choices)
    } else if (identical(default.position, "last")) {
      initial_choices <- c(initial_choices, default_choice)
    }
  }
  
  initial_choices
}

#' Parses an Initial Application State script
#' 
#' @description
#' Parses the selected initial application state script.
#' This can be a custom R script or the default initial state that creates
#' one panel of each class compatible with the contents of the data set.
#' 
#' The same script may define two objects `initial` and `tour` (see 'Value').
#'
#' @param bfc A [BiocFileCache()] object.
#' @param dataset_id Character scalar. Identifier of the data set selected.
#' @param config_id Character scalar. Identifier of the configuration file to load.
#' @param metadata Named list of metadata. See individual resource classes for required and optional metadata.
#'
#' @return A `list` of two elements.
#' \describe{
#' \item{initial}{A list of [Panel-class] objects, representing an initial app state}
#' \item{tour}{A `data.frame` representing an \pkg{rintrojs} interactive tour}
#' }
#'
#' @author Kevin Rue-Albrecht
#'
#' @rdname INTERNAL_parse_initial
.parse_initial <- function(bfc, dataset_id, config_id, metadata) {
    if (identical(config_id, .initial_default_choice)) {
        initial <- NULL
        tour <- NULL
    } else {
        bfc_config_id <- paste0(dataset_id, "_", config_id)
        # TODO: refactor to a function that is also used by .load_sce
        bfc_result <- bfcquery(bfc, bfc_config_id, field = "rname", exact = TRUE)
        # nocov start
        if (nrow(bfc_result) == 0) {
            uri_object <- .metadata_to_object(metadata)
            script_path <- precache(uri_object, bfc, bfc_config_id)
        } else {
            script_path <- bfc[[bfc_result$rid]]
        }
        # nocov end
        env <- new.env()
        source(script_path, local = env)
        if (!exists("initial", where = env)) {
            stop("No object named 'initial' was found - this needs to be ",
                 "defined in the config script.")
        }
        initial <- get("initial", pos = env)
        if (exists("tour", where = env)) {
          tour <- get("tour", pos = env)
        } else {
          tour <- NULL
        }
    }
    list(
      initial=initial,
      tour=tour
    )
}

#' Check Validity of Initial Configurations Metadata
#'
#' @param x `list` of lists of metadata.
#'
#' @return Invisible `NULL` if the metadata table is valid. Otherwise, throw an error.
#'
#' @author Kevin Rue-Albrecht
#'
#' @rdname INTERNAL_check_initial_table
#'
#' @examples
#' x <- list(
#'   list(
#'     id = "dataset01_config01",
#'     datasets = c("dataset01"),
#'     title = "Initial configuration 01",
#'     uri = "https://example.com/dataset01_config01.R",
#'     description = "Initial configuration 01 for data set 01."
#'   )
#' )
#' iSEEindex:::.check_initial_list(x)
.check_initial_list <- function(x) {
    # If x is NULL, it is valid without further checks.
    if (is.null(x)) {
        return(invisible(NULL))
    }
    # If x is not null it must have at least one row.
    if (identical(length(x), 0L)) {
        txt <- "If not NULL, initial configurations metadata must have at least one item."
        .stop(txt)
    }
    # Check that all required column names are present.
    required_metadata <- c(.initial_config_id,
        .initial_title, .initial_uri, .initial_description)
    for (metadata_name in required_metadata) {
        for (config_index in seq_along(x)) {
            if (!metadata_name %in% names(x[[config_index]])) {
                txt <- sprintf("Required metadata '%s' missing in initial configurations metadata #%i.", metadata_name, config_index)
                .stop(txt)
            }
        }
    }
    # Check that config identifiers are unique
    which_dup <- duplicated(vapply(x, function(x) x[[.initial_config_id]], character(1)))
    if (any(which_dup)) {
        first_dup <- which(which_dup)[1]
        txt <- sprintf("duplicate config_id: %s", x[[first_dup]][[.initial_config_id]])
        .stop(txt)
    }
    invisible(NULL)
}
