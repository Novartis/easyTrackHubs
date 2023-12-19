getGroupName <- function(x){
    gsub("^subgroup\\d_(\\S+)", "\\1", x)
}

getSubGroup <- function(x){
    gsub("^(subgroup\\d)_\\S+", "\\1", x)
}

createSymLink <- function( dest, target, copy_data ){
    if( copy_data ){
        rr <- file.copy( target, dest )
        if( !rr )
            stop(sprintf("File %s could not be copied.", target))            
    }else{
        rr <- createLink(
            link=dest,
            target=target )
        if( is.null(rr) )
            stop(sprintf("Symbolic link for file %s could not be created.", target))
    }
        invisible()
}

addExtraDefaults <- function( file_info, extraDefaults ){
    cls <- which(grepl("^ucscattr_", colnames(file_info)))
    perSampleAttributeCols <- gsub("^ucscattr_", "", colnames(file_info)[cls])
    doubleArgs <- names(extraDefaults) %in% perSampleAttributeCols
    if( any(doubleArgs) ){
        warning(sprintf("The following UCSC attribute(s) was/were already specified in the `file_info` column: %s",
                        paste(names(extraDefaults)[which(doubleArgs)], collapse=",")))
        extraDefaults <- extraDefaults[!doubleArgs]
    }
    if( !"visibility" %in% names(extraDefaults) & !"visibility" %in% perSampleAttributeCols )
        extraDefaults[["visibility"]] <- "full"
    if( !"autoScale" %in% names(extraDefaults) & !"autoScale" %in% perSampleAttributeCols )
        extraDefaults[["autoScale"]] <- "on"
    file_info <- addExtraDefaultCols( file_info, extraDefaults )
    file_info
}

addExtraDefaultCols <- function( file_info, extraDefaults ){
    if(length(extraDefaults) > 0){
        for( xx in names(extraDefaults) ){
            xxName <- sprintf("ucscattr_%s", xx)
            file_info[[xxName]] <- extraDefaults[[xx]]
        }
    }
    file_info
}
