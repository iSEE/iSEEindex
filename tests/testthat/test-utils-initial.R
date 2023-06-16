# .initial_choices ----

test_that(".initial_choices works ", {

    initial_table <- data.frame(
        id = "config01",
        datasets = "ID1",
        title = "Configuration 01")
    out <- iSEEindex:::.initial_choices("dummy_id", initial_table)

    expect_identical(out, c("Default" = "(Default)"))
})

test_that(".initial_choices works ", {

    initial_table <- data.frame(
        id = "config01",
        datasets = "ID1",
        title = "Configuration 01")

    out <- iSEEindex:::.initial_choices("ID1", initial_table)

    expect_identical(out, c(Default = "(Default)", "Configuration 01" = "config01"))
})

# .parse_initial ----

test_that(".parse_initial works for default choice", {

    out <- iSEEindex:::.parse_initial(bfc, dataset_id = "dummy", config_id = "(Default)", metadata = "dummy")
    expect_null(out)

})

test_that(".parse_initial works for non-default choice", {

    config_file <- paste0(
        "localhost://",
        system.file(package = "iSEEindex", "ReprocessedAllenData_config_01.R"))
    config_metadata <- list(uri = config_file)
    out <- iSEEindex:::.parse_initial(bfc, "ID1", "config01", config_metadata)
    expect_type(out, "list")

})

test_that(".parse_initial detects invalid scripts", {

    tf <- tempfile()
    file.create(tf)
    metadata <- list(uri = paste0("localhost://", tf))

    expect_error(
        iSEEindex:::.parse_initial(bfc, "ID0", "config0", metadata),
        "No object named 'initial' was found")

})

# .check_initial_table ----

test_that(".check_initial_list works for NULL metadata", {

    out <- iSEEindex:::.check_initial_list(NULL)
    expect_null(out)

})

test_that(".check_initial_list works for valid metadata", {

    x <- list(list(
        id = "config01",
        datasets = "dataset01",
        title = "Data Set 01",
        uri = "https://example.com/config01.R",
        description = "Long description of configuration 01."
    ))

    out <- iSEEindex:::.check_initial_list(x)
    expect_null(out)

})

test_that(".check_initial_list throws an error for missing required metadata", {

    x <- list(list(
        id = "config01",
        datasets = "dataset01",
        title = "Data Set 01",
        uri = "https://example.com/dataset01_config01.R"
    ))

    txt <- "Required metadata 'description' missing in initial configurations metadata"
    expect_error(
        iSEEindex:::.check_initial_list(x),
        paste(strwrap(txt, exdent = 2), collapse = "\n")
    )

})

test_that(".check_initial_list throws an error for zero item", {

    x <- list()

    txt <- "If not NULL, initial configurations metadata must have at least one item."
    expect_error(
        iSEEindex:::.check_initial_list(x),
        paste(strwrap(txt, exdent = 2), collapse = "\n")
    )

})

test_that(".check_initial_list throws an error when duplicate config_id is present", {

    x <- list(list(
        id = "config01",
        datasets = "dataset01",
        title = "Data Set 01",
        uri = "https://example.com/config01.R",
        description = "Long description of configuration 01."
    ))
    x <- append(x, x)

    expect_error(
        iSEEindex:::.check_initial_list(x),
        "duplicate config_id: config01"
    )

})
