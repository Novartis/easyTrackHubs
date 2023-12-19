test_that("Testing that attributes are written correctly", { 

    data( test_df, package="easyTrackHubs" )
    package_dir <- system.file("extdata", package="easyTrackHubs", mustWork=TRUE)
    bigwig_files <- list.files( package_dir, full.names=TRUE )
    names(bigwig_files) <- gsub(".bigWig", "", basename(bigwig_files))
    test_df$file_path <- bigwig_files[test_df$sample_name]
    
    test_df$ucscattr_visibility <- 2

    expect_message(
        easyTrackHub( test_df, trackhub_name="mth", trackhub_path="mth", maintainer_email="prueba@novartis.com" ),
        sprintf("Writing trackhub 'mth' with %s tracks...", nrow(test_df) ) )
    expect_equal(
        sum(grepl("visibility 2", readLines("mth/mm10/trackDb.txt"))),
        sum(test_df$reference_genome == "mm10"))
    unlink("mth", force=TRUE, recursive=TRUE)

    test_df$ucscattr_visibility <- NULL
    expect_message(
        easyTrackHub( test_df, trackhub_name="mth", trackhub_path="mth", maintainer_email="prueba@novartis.com" ),
        sprintf("Writing trackhub 'mth' with %s tracks...", nrow(test_df) ))
    expect_equal(
        sum(grepl("visibility full", readLines("mth/mm10/trackDb.txt"))),
        sum(test_df$reference_genome == "mm10"))
    unlink("mth", force=TRUE, recursive=TRUE)

    ## test_df$ucscattr_visibility <- "full"
    expect_message(
        easyTrackHub( test_df, trackhub_name="mth", trackhub_path="mth", maintainer_email="prueba@novartis.com", visibility="other" ),
        sprintf("Writing trackhub 'mth' with %s tracks...", nrow(test_df) ))
    expect_equal(
        sum(grepl("visibility other", readLines("mth/mm10/trackDb.txt"))),
        sum(test_df$reference_genome == "mm10"))
    unlink("mth", force=TRUE, recursive=TRUE)

    test_df$ucscattr_visibility <- "other"
    expect_warning(
        easyTrackHub( test_df, trackhub_name="mth", trackhub_path="mth", maintainer_email="prueba@novartis.com", visibility="other" ),
        "The following UCSC attribute" )
    expect_equal(
        sum(grepl("visibility other", readLines("mth/mm10/trackDb.txt"))),
        sum(test_df$reference_genome == "mm10"))
    unlink("mth", force=TRUE, recursive=TRUE)

    test_df$ucscattr_visibility <- NULL
    expect_message(
        easyTrackHub( test_df, trackhub_name="mth", trackhub_path="mth", maintainer_email="prueba@novartis.com",
                     randomParam=3 ),
        sprintf("Writing trackhub 'mth' with %s tracks...", nrow(test_df) ))
    unlink("mth", force=TRUE, recursive=TRUE)

    test_df$ucscattr_randomParam <- 3
    expect_warning(
        easyTrackHub( test_df, trackhub_name="mth", trackhub_path="mth", maintainer_email="prueba@novartis.com",
                     visibility="other", randomParam=3 ),
        "The following UCSC attribute" )
    unlink("mth", force=TRUE, recursive=TRUE)

    
})
