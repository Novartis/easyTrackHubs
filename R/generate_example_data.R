#' @docType data
#' @keywords datasets
#' @name test_df
#' @usage data(test_df)
#'
#' @title Example data for input to easyTrackHubs
#'
#' @description An example dataset used as input to easyTrackHubs.
#' Example data, also contained int he package, was downloaded
#' from ENCODE and subset in order to keep the files small. 
#' 
#' @format A data frame with 21 rows (samples) and 6 variables:
#' sample_name: Sample identifier from ENCODE.
#' file_format: Specification of the file format (bigWig).
#' cell_type: Cell type used in the assay.
#' reference_genome: Genome assembly in UCSC format.
#' data_type: Name of the assay (ChIP-seq, CUT&RUN, etc).
NULL

## ## identify the example datasets
## library(AnnotationHub)
## ah <- AnnotationHub()
## ss <- query(ah, c("ENCODE", "Metadata"))
## encodeMetadata <- ss[["AH75132"]]

## encodeMetadata <- encodeMetadata %>%
##     dplyr::filter(
##                assay == "Histone ChIP-seq", assembly %in% c("GRCh38", "mm10"),
##                file_type == "bigWig", output_type == "fold change over control",
##                biosample_name %in% c("HCT116", "HeLa-S3", "liver" ),
##                target %in% c("H3K27ac", "H3K27me3")
##            ) %>%
##     dplyr::filter(
##     ( target == "H3K27me3" & assembly == "mm10" & dataset_description == "H3K27me3 ChIP-seq on postnatal 0 day mouse liver") |
##     ( target %in% c("H3K27ac", "H3K27me3") & assembly == "GRCh38" & biosample_name %in% c("HCT116", "HeLa-S3" ) ) ) %>%
##     dplyr::select( title, file_format, target, biosample_name, assembly, `cloud_metadata.url`, cloud_metadata.file_size )

## ## ## download the files
## ## tmpDir <- tempdir()
## ## downloadedFiles <- lapply( encodeMetadata$`cloud_metadata.url`, function(x){
## ##     outFile <- file.path( tmpDir, basename(x) )
## ##     system(sprintf("wget -O %s %s", outFile, x))
## ##     outFile
## ## })


## ## ## subset files to an example region
## ## library(rtracklayer)
## ## targetRegion <- resize( GRanges("chr12:6534512-6538374"), 15000, fix="center" )
## ## for( xx in downloadedFiles ){
## ##     cov <- import( xx )
## ##     subs <- subsetByOverlaps(cov, targetRegion)
## ##     export( subs, con=file.path("../inst/extdata", basename(xx)), format="BigWig" )
## ## }

## ## reformat and save data
## encodeMetadata$`cloud_metadata.url` <- NULL
## encodeMetadata$`cloud_metadata.file_size` <- NULL
## test_df <- encodeMetadata %>%
##     dplyr::rename( `sample_name`=title, cell_type=biosample_name,
##                   reference_genome=assembly ) %>%
##     dplyr::mutate( data_type="ChIP-seq" )

## test_df$reference_genome <- gsub("GRCh38", "hg38", test_df$reference_genome)

## save( test_df, file="../data/test_df.RData" )

