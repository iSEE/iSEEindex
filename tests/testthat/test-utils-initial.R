# .datasets_available ----

test_that(".initial_choices works ", {
    
    out <- iSEEindex:::.initial_choices("dummy_id", NULL)
    
    expect_identical(out, c("Default" = "(Default)"))
})

test_that(".initial_choices works ", {
    
    FUN <- function(x) {
        out <- c("name" = "value")
    }
    out <- iSEEindex:::.initial_choices("dummy_id", FUN)
    
    expect_identical(out, c(Default = "(Default)", name = "value"))
})

# .load_sce ----

test_that(".load_initial works for default choice", {
    
    pObjects <- new.env()
    pObjects[[iSEEindex:::.ui_initial]] <- iSEEindex:::.initial_default_choice
    
    out <- iSEEindex:::.load_initial(pObjects = pObjects)
    expect_null(out)
    
})

test_that(".load_initial works for non-default choice", {
    
    pObjects <- new.env()
    pObjects[[iSEEindex:::.ui_initial]] <- "dummy_choice"
    
    out <- iSEEindex:::.load_initial(pObjects = pObjects)
    expect_null(out)
    
})
