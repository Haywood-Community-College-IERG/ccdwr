#' Merge all files in a specified path
#'
#' This function allows you to merge all the files in a specified folder.
#' It handles removing the extra header lines, if necessary. It also
#' adds the file name as an extra variable, if this is necessary.
#'
#' @param path Defaults to the current working directory using getwd()
#' @param pattern The pattern used to filter the files prior to the merge
#' @param type The type of merge. Possible options are csv, fixed, other
#' @param FNvar The name of the variable which will contain the filename. Will only add filename if this parameter has a variable name.
#' @param FUN The function to use for reading in the data. Defaults to
#'     `readr::read_csv` for CSV files, `readr::read_fwf` for fixed files,
#'     and `readr::read_lines` for all other files.
#' @param col_types A col_types definition to be passed to the corresponding
#'     `readr` function.
#' @param ... All other arguments passed to corresponding `readr` function.
#' @return A dataframe with all selected files combined.
#' @keywords file
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom magrittr `%>%`
#'
merge_files <- function( path=getwd(), pattern="*", type="csv", FNvar="", FUN=NA,
                        col_types=readr::cols(.default=readr::col_character()), ... ) {

    useColTypes <- FALSE

    if (is.na(FUN)) {
#    if (!is.function(match.fun(FUN, descend=FALSE))) {
        if (type=="csv") {
            readfile <- readr::read_csv
            useColTypes <- TRUE
        } else if (type=="fixed") {
            readfile <- readr::read_fwf
            useColTypes <- TRUE
        } else {
            readfile <- readr::read_lines
        }
    } else {
        readfile = match.fun(FUN, descend=FALSE)
    }

    readfile_addFileName <- function(.file, ...) {
        dat <- readfile( .file, ... )
        dat[[FNvar]] <- as.character(.file)
        dat    # return the dataframe
    }

    if (FNvar!="") {
        readfun <- readfile_addFileName
    } else {
        readfun <- readfile
    }

    if (useColTypes) {
        df <- list.files( path        = path,
                          pattern     = pattern,
                          full.names  = TRUE,
                          ignore.case = TRUE ) %>%
            lapply(readfun,col_types=col_types, ...) %>%
            dplyr::bind_rows()
    } else {
        df <- list.files( path        = path,
                          pattern     = pattern,
                          full.names  = TRUE,
                          ignore.case = TRUE ) %>%
              lapply(readfun, ...) %>%
              dplyr::bind_rows()
    }

    df
}
