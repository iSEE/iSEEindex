test_that(".list_to_dataframe fills missing information with NA", {
    
    l <- list(
        list(A=1.1, B=2.2, C=3.1),
        list(A=2.1, B=2.1, C=3.2, D=4.2)
    )
    
    out <- iSEEindex:::.list_to_dataframe(l)
    
    expect_identical(out[1, "D"], NA_real_)
    
})
