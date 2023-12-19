#' @title Building UCSC trackhubs
#' @description This function inputs a data frame containing file paths to genomic coverage files
#' and their corresponding metadata. The function reorganizes the information and writes a directory
#' with the structure of a UCSC trackhub.
#' @param file_info A data frame with the columns "sample_name", "file_path", "file_format", "reference_genome" and "data_type".
#' @param trackhub_name The identifier of the trackhub.
#' @param trackhub_path The path where to write the new trackhub.
#' @param maintainer_email A character string with an e-mail address.
#' @param is_multiwig Logical indicating if track should be created as multwig overlay.
#' @param copy_data Logical indicating whether to copy the data or to create symbolic links to the data.
#' @param ... Additional parameters to be passed as UCSC parameters.
#' @importFrom R.utils createLink
#' @importFrom methods is
#' @importFrom grDevices col2rgb
#' @examples
#' library(easyTrackHubs)
#' data( test_df, package="easyTrackHubs" )
#' package_dir <- system.file("extdata", package="easyTrackHubs", mustWork=TRUE)
#' bigwig_files <- list.files( package_dir, full.names=TRUE )
#' names(bigwig_files) <- gsub(".bigWig", "", basename(bigwig_files))
#' test_df$file_path <- bigwig_files[test_df$sample_name]
#' easyTrackHub( test_df, trackhub_name="mth", trackhub_path="mth4", maintainer_email="prueba@novartis.com" )
#' 
#' @export
easyTrackHub <- function( file_info, trackhub_name, trackhub_path, maintainer_email, is_multiwig = FALSE, copy_data=FALSE, ... ){
    ## input checks ##
    stopifnot(is(file_info, "data.frame"))
    stopifnot(is(trackhub_name, "character"))
    stopifnot(is(trackhub_path, "character"))
    stopifnot(is(maintainer_email, "character"))
    if( file.exists( trackhub_path ) )
        stop(sprintf("The directory '%s' already exists. Please either remove it or set a different value for 'trackhub_path'.", trackhub_path))
    if( !all( file.exists( file_info$file_path ) ) )
        stop("At least one of the input files that are non-existent.")
    if( !all( c("sample_name", "file_path", "file_format", "reference_genome", "data_type") %in% colnames( file_info ) ) )
        stop("Invalid 'file_info' columns. Please make sure all the required columns are present")
    if( is_multiwig ) if ( !('group' %in% colnames(file_info)) ) 
        stop("Please provide a data.frame with a 'group' column if using the multiwig option.")
    if( "color" %in% colnames( file_info ) ) hasColorFlag <- TRUE else hasColorFlag <- FALSE
    ##
    extraDefaults <- list(...)
    file_info <- addExtraDefaults( file_info, extraDefaults )
    file_info$file_path <- normalizePath( file_info$file_path )
    new_dirs <- unique(file.path( trackhub_path, file_info$reference_genome, file_info$data_type ))
    ## create directory structure for trackhub ##
    for( i in new_dirs )
        dir.create( path=i, recursive=TRUE )
    ## create database files for each reference genome ##
    for( i in file.path( trackhub_path, unique(file_info$reference_genome), "trackDb.txt" ) ){
        file.create( path=i )
        cat("\n", file=i, append=TRUE)
    }
    ## write file with trackhub information ##
    cat(
        sprintf("hub %s \nshortLabel %s\nlongLabel %s\ngenomesFile genomes.txt\nemail %s\n",
                trackhub_name, trackhub_name, trackhub_name, maintainer_email),
        file=file.path(trackhub_path, "hub.txt") )
    ## write file with reference genomes ##
    cat( "\n", file=file.path(trackhub_path, "genomes.txt") )
    for( i in unique(file_info$reference_genome) )
        cat(sprintf("genome %s\ntrackDb %s/trackDb.txt\n\n", i, i), append=TRUE, file=file.path(trackhub_path, "genomes.txt"))
    ## if no grouping is present, just write the trackhubs per sample
    if( is.null(file_info$group) ){
        message( sprintf( "Writing trackhub '%s' with %d tracks...", trackhub_name, nrow( file_info ) ) )
        writeTracks( file_info, trackhub_path, hasColorFlag, copy_data )
    }else{
        if( is_multiwig ) {
            message( sprintf("Writing trackhub '%s' with %d tracks organized as multiwig overlay according to the grouping variable",
                            trackhub_name, nrow( file_info) ) )
            writeTracksWithGroups( file_info, trackhub_path, hasColorFlag, copy_data, is_multiwig )
            
        } else {
            numSubGroups <- sum( grepl("subgroup\\d_\\S+", colnames( file_info ) ) )
            message( sprintf("Writing trackhub '%s' with %d tracks organized into a main grouping variable and %d subgrouping variables...",
                            trackhub_name, nrow( file_info), numSubGroups ) )
            writeTracksWithGroups( file_info, trackhub_path, hasColorFlag, copy_data, is_multiwig )
        }
    }
    invisible()
}

writeTracksWithGroups <- function( file_info, trackhub_path, hasColorFlag, copy_data, is_multiwig ){
    file_info_sp <- split(
        file_info,
        file_info[,c("reference_genome", "group")],
        sep=":", drop=TRUE )
    for( x in names( file_info_sp ) ){
        rg <- gsub("(\\S+):\\S+", "\\1", x)
        grp <- gsub("\\S+:(\\S+)", "\\1", x)
        ff <- unique(file_info_sp[[x]]$file_format)
        if( length(ff) != 1 ){
            stop("Invalid file format specification. All tracks of within one group must have the same file format")
        }
        hasSubgroups <- grepl("subgroup\\d_\\S+", colnames( file_info_sp[[x]] ))
        if( any( hasSubgroups ) & !is_multiwig ){
            subgroupDF <- file_info_sp[[x]][,hasSubgroups,drop=FALSE]
        }else{
            subgroupDF <- NULL
            if( any( hasSubgroups ) ) file_info_sp[[x]] <- file_info_sp[[x]][,!hasSubgroups]
        }
        writeGroupTrack( grp, trackhub_path, rg, ff, subgroup_df=subgroupDF, is_multiwig )
        writeTracks( file_info_sp[[x]], trackhub_path, hasColorFlag, copy_data, indent="    " )
    }
    invisible()
}

writeGroupTrack <- function( groupLabel, trackhub_path, referenceGenome, fileFormat, subgroup_df, is_multiwig ){
    groupTrack <- sprintf("\ntrack %s\nshortLabel %s\nlongLabel %s\ntype %s\n",#allButtonPair on\n",
                          groupLabel, groupLabel, groupLabel, fileFormat )
    trackDbFile <- file.path( trackhub_path, referenceGenome, "trackDb.txt" )
    cat( groupTrack,
        file=trackDbFile,
        append=TRUE )
    if( !is.null( subgroup_df ) ){
        if( ncol(subgroup_df) > 9 ){
            warning("More than 9 subgroups are not supported by UCSC, only using the first 9...")
            subgroup_df <- subgroup_df[,seq(1, 9)]
        }
        for(i in seq_len(ncol(subgroup_df))){
            groupName <- getGroupName(colnames(subgroup_df)[i])
            groupNumb <- getSubGroup(colnames(subgroup_df)[i])
            levs <- unique(subgroup_df[[i]])
            cat(sprintf("%s %s %s %s\n",
                    gsub("subgroup", "subGroup", groupNumb),
                    groupName, groupName,
                    paste(sprintf("%s=%s", levs, levs), collapse=" ")),
                file=trackDbFile, append=TRUE)
        }
        possibleDimensions <- paste0("dim", c("X", "Y", LETTERS[seq_len(7L)]))
        possibleDimensions <- possibleDimensions[seq_len(ncol(subgroup_df))]
        dimensionsLine <- paste(possibleDimensions, getGroupName(colnames(subgroup_df)), sep="=")
        cat( sprintf("dimensions %s\n", paste(dimensionsLine, collapse=" ")), file=trackDbFile, append=TRUE )
        cat( sprintf("sortOrder %s\n", paste(paste(getGroupName(colnames(subgroup_df)), "+", sep="="), collapse=" ")),
                     file=trackDbFile, append=TRUE )
        if( length(possibleDimensions) > 2 ){
            filtCompDimensions <- possibleDimensions[seq( 3, length(possibleDimensions), 1)]
            cat( sprintf("filterComposite %s\n", paste( filtCompDimensions, collapse=" " )), file=trackDbFile, append=TRUE )
        }
    }
    cat("autoScale on\n", file=trackDbFile, append=TRUE)
    cat("visibility full\n", file=trackDbFile, append=TRUE)
    if( is_multiwig ) {
        cat("container multiWig\naggregate transparentOverlay\n\n", file=trackDbFile, append=TRUE)
    }else{
        cat("compositeTrack on\n\n", file=trackDbFile, append=TRUE)
    }
    invisible()
}

writeTracks <- function( file_info, trackhub_path, hasColorFlag, copy_data, indent="", subgroup_df=NULL ){
    hasSubgroups <- grepl("subgroup\\d_\\S+", colnames( file_info ))
    ## create symlink and write the track in the corresponding database file foreach track ##
    for( i in seq_len(nrow( file_info ) ) ){
        dest <- file.path( trackhub_path, file_info[i,"reference_genome"], file_info[i,"data_type"], basename(file_info[i, "file_path"]) )
        trackLab <- file_info[i,"sample_name"]
        createSymLink( dest, file_info[i, "file_path"], copy_data )
        if( file_info[i,"file_format"] == "bam" ){
            createSymLink( paste0(dest, ".bai"), paste0(file_info[i, "file_path"], ".bai"), copy_data )
        }
        trackDbFile <- file.path( trackhub_path, file_info[i,"reference_genome"], "trackDb.txt" )
        cat( sprintf("%strack %s\n", indent, trackLab), file=trackDbFile, append=TRUE )
        ## if there is grouping, add parent group label ##
        if( !is.null( file_info[i,"group"] ) ){
            cat( sprintf("%sparent %s on\n", indent, file_info[i,"group"]), file=trackDbFile, append=TRUE )
        }
        ## print basic track stuff ##
        cat(
            sprintf("%sbigDataUrl %s\n%sshortLabel %s\n%slongLabel %s\n%stype %s\n",
                    indent, file.path(file_info[i,"data_type"], basename(file_info[i, "file_path"])),
                    indent, trackLab,
                    indent, trackLab,
                    indent, file_info[i,"file_format"] ),
            file=trackDbFile, append=TRUE )
        ## print subgroup labels in case there is any ##
        if( any( hasSubgroups ) ){
            subgroupLabels <- file_info[i,hasSubgroups]
            subgroupColumns <- getGroupName(colnames(file_info)[hasSubgroups])
            subgroupColumns <- subgroupColumns[!is.na(subgroupLabels)]
            subgroupLabels <- subgroupLabels[!is.na(subgroupLabels)]
            if( length( subgroupLabels ) > 0 ){
                tags <- paste( subgroupColumns, subgroupLabels, sep="=" )
                tags <- paste( tags, collapse=" " )
                cat(sprintf("%ssubGroups %s\n", indent, tags), file=trackDbFile, append=TRUE)
            }
        }
        ## if color column is specified in the input data frame, add the color to the track
        if( hasColorFlag && !is.na(file_info[i,"color"]) ){
            colRgb <- paste(col2rgb(file_info[i,"color"])[,1L], collapse=",")
            cat( sprintf("%scolor %s\n", indent, colRgb), file=trackDbFile, append=TRUE )
        }
        addCustomAttrsToTrack(file_info[i,], colnames(file_info), trackDbFile, indent)
        cat( "\n", file=trackDbFile, append=TRUE )
    }
    invisible()
}

addCustomAttrsToTrack <- function( file_info_row, file_info_colnames, trackDbFile, indent="" ) {
    ## add custom attributes to the track
    ## if it has NA value - skip
    additionalAttr.ind <- grep("^ucscattr_", file_info_colnames)
    for (attr.ind in additionalAttr.ind) {
        attr <-sub("^ucscattr_", "", file_info_colnames[attr.ind])
        val <- file_info_row[[attr.ind]]
        if (!is.na(val)) {
            cat( sprintf("%s%s %s\n", indent, attr, val), file=trackDbFile, append=TRUE )
        }
    }
    invisible()
}
