test_that("Inputs are checked correctly", {

    data( test_df, package="easyTrackHubs" )
    package_dir <- system.file("extdata", package="easyTrackHubs", mustWork=TRUE)
    bigwig_files <- list.files( package_dir, full.names=TRUE )
    names(bigwig_files) <- gsub(".bigWig", "", basename(bigwig_files))
    test_df$file_path <- bigwig_files[test_df$sample_name]
    test_df$group <- test_df$target
    ## multiwig option without group
    expect_error(
        easyTrackHub( test_df[,!colnames(test_df) %in% "group"], trackhub_name="mth3", trackhub_path="mth3",
                      maintainer_email="prueba@novartis.com", copy=FALSE, is_multiwig = TRUE),
          "Please provide a data.frame with a 'group' column if using the multiwig option.")
    unlink("mth3", force=TRUE, recursive=TRUE)
    ##works with only one group
    expect_message( 
        easyTrackHub( test_df, trackhub_name="mth3", trackhub_path="mth3", maintainer_email="prueba@novartis.com", copy=TRUE ),
        sprintf("Writing trackhub 'mth3' with %s tracks organized into a main grouping variable",
                nrow(test_df))
    )
    expect_true(all(is(list.files("mth3"), "character")))
    unlink("mth3", force=TRUE, recursive=TRUE)
    ##runs with NA values
    test_df$subgroup1_samegroup <- NA
    easyTrackHub( test_df, trackhub_name="mth3", trackhub_path="mth3", maintainer_email="prueba@novartis.com", copy=TRUE )
    expect_true(all(is(list.files("mth3"), "character")))
    unlink("mth3", force=TRUE, recursive=TRUE)

    test_df$subgroup1_samegroup <- NULL
    test_df$file_format[2] <- "bam"
    expect_error(
        easyTrackHub( test_df, trackhub_name="mth3", trackhub_path="mth3", maintainer_email="prueba@novartis.com", copy=TRUE ),
        "Invalid file format" )
    unlink("mth3", force=TRUE, recursive=TRUE)

    test_df$file_format <- "bigWig"
    for( i in seq_len(10)-1 ){
        i <- paste0("subgroup", i, "_", LETTERS[i+1])
        test_df[[i]] <- "A"
    }
    suppressWarnings(expect_warning(
        easyTrackHub( test_df, trackhub_name="mth3", trackhub_path="mth3", maintainer_email="prueba@novartis.com", copy=TRUE ),
        "More than 9 subgroups"))
    unlink("mth3", force=TRUE, recursive=TRUE)    
} )
