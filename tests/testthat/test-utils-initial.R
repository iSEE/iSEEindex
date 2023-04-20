# .initial_choices ----

test_that(".initial_choices works ", {

    initial_table <- data.frame(
        dataset_id = "ID1",
        config_id = "config01",
        title = "Configuration 01")
    out <- iSEEindex:::.initial_choices("dummy_id", initial_table)

    expect_identical(out, c("Default" = "(Default)"))
})

test_that(".initial_choices works ", {

    initial_table <- data.frame(
        dataset_id = "ID1",
        config_id = "config01",
        title = "Configuration 01")

    out <- iSEEindex:::.initial_choices("ID1", initial_table)

    expect_identical(out, c(Default = "(Default)", "Configuration 01" = "config01"))
})

# .load_sce ----

test_that(".load_initial works for default choice", {

    out <- iSEEindex:::.load_initial(bfc, "dummy", "(Default)", "dummy")
    expect_null(out)

})

test_that(".load_initial works for non-default choice", {

    config_file <- paste0(
        "localhost://",
        system.file(package = "iSEEindex", "ReprocessedAllenData_config_01.R"))
    config_metadata <- list(uri = config_file)
    out <- iSEEindex:::.load_initial(bfc, "ID1", "config01", config_metadata)
    expect_type(out, "list")

})

test_that(".load_initial detects invalid scripts", {

    tf <- tempfile()
    file.create(tf)
    metadata <- list(uri = paste0("localhost://", tf))

    expect_error(
        iSEEindex:::.load_initial(bfc, "ID0", "config0", metadata),
        "No object named 'initial' was found")

})

# .check_initial_table ----

test_that(".check_initial_table works for NULL metadata", {

    out <- iSEEindex:::.check_initial_table(NULL)
    expect_null(out)

})

test_that(".check_initial_table works for valid metadata", {

    x <- data.frame(
        config_id = "dataset01_config01",
        dataset_id = "dataset01",
        title = "Data Set 01",
        uri = "https://example.com/dataset01_config01.R",
        description = "Configuration 01 for data set 01."
    )

    out <- iSEEindex:::.check_initial_table(x)
    expect_null(out)

})

test_that(".check_initial_table throws an error for missing required column", {

    x <- data.frame(
        config_id = "dataset01_config01",
        dataset_id = "dataset01",
        title = "Data Set 01",
        uri = "https://example.com/dataset01_config01.R"
    )

    txt <- "Required column 'description' missing in initial configurations metadata."
    expect_error(
        iSEEindex:::.check_initial_table(x),
        paste(strwrap(txt, exdent = 2), collapse = "\n")
    )

})

test_that(".check_initial_table throws an error for zero rows", {

    x <- data.frame(
        dataset_id = character(0),
        config_id = character(0),
        title = character(0),
        uri = character(0),
        description = character(0)
    )

    txt <- "If not NULL, initial configurations metadata must have at least one row."
    expect_error(
        iSEEindex:::.check_initial_table(x),
        paste(strwrap(txt, exdent = 2), collapse = "\n")
    )

})

test_that(".check_initial_table throws an error when duplicate config_id is present", {

    x <- data.frame(
        config_id = "dataset01_config01",
        dataset_id = "dataset01",
        title = "Data Set 01",
        uri = "https://example.com/dataset01_config01.R",
        description = "Configuration 01 for data set 01."
    )
    x <- rbind(x, x)

    expect_error(
        iSEEindex:::.check_initial_table(x),
        "duplicate config_id: dataset01_config01"
    )

})
