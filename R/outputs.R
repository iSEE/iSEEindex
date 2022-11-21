#' Render Table of Available Data Sets
#'
#' @param output The Shiny output object from the server function.
#' @param pObjects An environment containing global parameters generated in the landing page.
#' @param rObjects A reactive list of values generated in the landing page.
#'
#' @return Adds a rendered [DT::datatable()] to `output`.
#' A \code{NULL} value is invisibly returned.
#' 
#' @importFrom DT datatable renderDT
#'
#' @rdname INTERNAL_render_datasets_table
.render_datasets_table <- function(output, pObjects, rObjects) {
    # nocov start
    output[[.ui_dataset_table]] <- DT::renderDT({
        force(rObjects$rerender_datasets)
        keep_columns <- pObjects[[.ui_dataset_columns]]
        datasets_visible <- pObjects$datasets_table[, keep_columns, drop=FALSE]
        DT::datatable(datasets_visible, filter="top", rownames=TRUE,
            options=list(
                search=list(search="", smart=FALSE, regex=TRUE, caseInsensitive=FALSE),
                searchCols=c(list(NULL), list(NULL)), # row names are the first column!
                scrollX=TRUE,
                columnDefs=NULL),
            selection=list(mode = 'single', selected=1L, target = 'row'))
    })
    # nocov end

    invisible(NULL)
}

#' Render Overview of Selected Data Set
#' 
#' `.render_markdown_overview()` renders an overview of the selected data set using Markdown.
#' 
#' `.render_initial_overview()` renders an overview of the selected initial configuration using Markdown.
#'
#' @param output The Shiny output object from the server function.
#' @param pObjects An environment containing global parameters generated in the landing page.
#' @param rObjects A reactive list of values generated in the landing page.
#' 
#' @details
#' Currently, those functions expect a column named `uri` in the metadata table
#' of available data sets, which it uses as the identifier for each data set.
#'
#' @return Adds a rendered [DT::datatable()] to `output`.
#' A \code{NULL} value is invisibly returned.
#'
#' @importFrom shiny markdown renderUI
#'
#' @rdname INTERNAL_render_markdown_overview
.render_markdown_overview <- function(output, pObjects, rObjects) {
    # nocov start
    output[[.ui_markdown_overview]] <- renderUI({
        force(rObjects$rerender_overview)
        dataset_selected_id <- pObjects[[.dataset_selected_id]]
        if (!length(dataset_selected_id)) {
            contents <- markdown("Please select a data set.")
        } else {
            dataset_info <- subset(pObjects$datasets_table, id == dataset_selected_id, drop=FALSE)
            contents <- markdown(paste0(
                "# ", sprintf("%s", dataset_info[[.datasets_label]]), "\n\n",
                sprintf("%s", dataset_info[[.datasets_description]]), "\n\n"
            ))
        }
        contents
    })
    # nocov end

    invisible(NULL)
}

#' @rdname INTERNAL_render_markdown_overview
.render_initial_overview <- function(output, pObjects, rObjects) {
    # nocov start
    output[[.ui_initial_overview]] <- renderUI({
        force(rObjects$rerender_initial)
        dataset_selected_id <- pObjects[[.dataset_selected_id]]
        initial_basename <- pObjects[[.ui_initial]]
        if (identical(initial_basename, .initial_default_choice)) {
            contents <- markdown(paste0(
                "By default, `iSEE()` automatically displays one instance of each built-in panel compatible with the data set.\n\n",
                "This is useful to showcase the functionality of the [_iSEE_](https://bioconductor.org/packages/iSEE/) package, ",
                "sometimes at the cost of a longer loading time."
            ))
        } else {
            contents <- markdown("TODO: display information about the configuration.")
        }
        contents
    })
    # nocov end

    invisible(NULL)
}
