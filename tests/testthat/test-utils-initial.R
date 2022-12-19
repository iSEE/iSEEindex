# .initial_choices ----

test_that(".initial_choices works ", {
    
    initial_table <- data.frame(
        dataset_id="ID1",
        config_id="config01",
        label="Configuration 01")
    out <- iSEEindex:::.initial_choices("dummy_id", initial_table)
    
    expect_identical(out, c("Default" = "(Default)"))
})

test_that(".initial_choices works ", {
    
    initial_table <- data.frame(
        dataset_id="ID1",
        config_id="config01",
        label="Configuration 01")
    
    out <- iSEEindex:::.initial_choices("ID1", initial_table)
    
    expect_identical(out, c(Default = "(Default)", "Configuration 01" = "config01"))
})

# .load_sce ----

test_that(".load_initial works for default choice", {
    
    out <- iSEEindex:::.load_initial(bfc, "dummy", "(Default)", "dummy")
    expect_null(out)
    
})

test_that(".load_initial works for non-default choice", {
    
    config_file <- paste0("localhost://", system.file(package = "iSEEindex", "ReprocessedAllenData_config_01.R"))
    out <- iSEEindex:::.load_initial(bfc, "ID1", "config01", config_file)
    expect_type(out, "list")
    
})

test_that(".load_initial detects invalid scripts", {
    
    tf <- tempfile()
    file.create(tf)
    uri <- paste0("localhost://", tf)
    	
    expect_error(iSEEindex:::.load_initial(bfc, "ID0", "config0", uri), "No object named 'initial' was found")
    
})
