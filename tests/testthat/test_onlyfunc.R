
test_that("Inputs are checked correctly", {
    unlink("mth", force=TRUE, recursive=TRUE) # clear any data from previously run tests
    data( test_df, package="easyTrackHubs" )
    package_dir <- system.file("extdata", package="easyTrackHubs", mustWork=TRUE)
    bigwig_files <- list.files( package_dir, full.names=TRUE )
    names(bigwig_files) <- gsub(".bigWig", "", basename(bigwig_files))
    test_df$file_path <- bigwig_files[test_df$sample_name]
    expect_error(easyTrackHub( as.list(test_df), trackhub_name="mth", trackhub_path="mth4", maintainer_email="prueba@novartis.com", copy=TRUE ))
    expect_error(easyTrackHub( test_df, trackhub_name=12, trackhub_path="mth4", maintainer_email="prueba@novartis.com", copy=TRUE ))
    test_df$file_path[1] <- paste0(test_df$file_path[1], "error")
    expect_error(easyTrackHub( test_df, trackhub_name="mth", trackhub_path="mth4", maintainer_email="prueba@novartis.com", copy=TRUE ), "At least" )
    test_df$file_path[1] <- gsub("error$", "", test_df$file_path[1])
    file.create("mth")
    expect_error(
        easyTrackHub( test_df, trackhub_name="mth", trackhub_path="mth", maintainer_email="prueba@novartis.com", copy=TRUE ), "The directory")
    file.remove("mth")
    expect_message(
        easyTrackHub( test_df, trackhub_name="mth", trackhub_path="mth", maintainer_email="prueba@novartis.com" ),
                   sprintf("Writing trackhub 'mth' with %s tracks...", nrow(test_df) ) )
    expect_true(all(is(list.files("mth"), "character")))
    unlink("mth", force=TRUE, recursive=TRUE)
    test_df$color <- c("thisisnotacolor")
    
    expect_error( easyTrackHub( test_df, trackhub_name="mth", trackhub_path="mth", maintainer_email="prueba@novartis.com" ), "invalid color name" )
    unlink("mth", force=TRUE, recursive=TRUE)    
    test_df$color <- RColorBrewer::brewer.pal( nrow(test_df)/3, "Dark2" )
    easyTrackHub( test_df, trackhub_name="mth", trackhub_path="mth", maintainer_email="prueba@novartis.com", copy=TRUE )
    expect_true(all(is(list.files("mth"), "character")))
    unlink("mth", force=TRUE, recursive=TRUE)
    test_df[test_df$reference_genome == "mm10", "color"] <- NA
    easyTrackHub( test_df, trackhub_name="mth", trackhub_path="mth", maintainer_email="prueba@novartis.com", copy=TRUE )
    expect_true(all(is(list.files("mth"), "character")))
    expect_equal(0, length(grep('color',readLines("mth/mm10/trackDb.txt")))) # no color should be set for mm10
    expect_equal( sum(test_df$reference_genome == "hg38"), length(grep('color',readLines("mth/hg38/trackDb.txt")))) # and it should be still there for hg38
    unlink("mth", force=TRUE, recursive=TRUE)
} )
