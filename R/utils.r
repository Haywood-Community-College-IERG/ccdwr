#' Return a list of elements from x that are not in y.
#'
#' @param x Elements that are not to be found in the List y.
#' @param y List to check against.
#' @export
#'
"%nin%" <- function(x, y) !(x %in% y)

#' Coalesce a value to a non-null and non-blank.
#'
#' @param var Value to return if not null or blank.
#' @param default Value to return if var is null or blank.
#' @export
#'
coalesce_blanks <- function( var, default = '' ) {
    return( dplyr::if_else( is.na(var) | var == '', default, var ) )
}

#' Calculate the age.
#'
#' @param from The birthdate or start date.
#' @param to The current date or end date.
#' @export
#'
age <- function(from, to) {
    from_lt = as.POSIXlt(from)
    to_lt = as.POSIXlt(to)

    age = to_lt$year - from_lt$year

    ifelse(to_lt$mon < from_lt$mon |
               (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
           age - 1, age)
}

#' Return an element if it is like any one of the provided patterns.
#'
#' @param x Elements to return if it matches any one pattern.
#' @param pattern Patterns to match.
#' @export
#'
`%like any%` <- function(x, pattern) {

    pattern <- sapply(pattern, function(z){
        if (!substr(z, 1, 1) == "%") {
            z <- paste("^", z, sep="")
        } else {
            z <- substr(z, 2, nchar(z) )
        }
        if (!substr(z, nchar(z), nchar(z)) == "%") {
            z <- paste(z, "$", sep="")
        } else {
            z <- substr(z, 1, nchar(z)-1 )
        }
        return(z)
    })

    grepl(pattern=paste(pattern, collapse = "|"), x=x)

    # since 0.99.17: better returning the values, than a logical vector:
    # grep(pattern=paste(pattern, collapse = "|"), x=x, value=TRUE)

    # rolled back 26.4.2016: did not really prove successful

}

#' The opposite of \%like any\%.
#'
#' @param x Elements to return if it matches any one pattern.
#' @param pattern Patterns to match.
#' @export
#'
`%nlike any%` <- function(x, pattern) !(`%like any%`(x,pattern))

#' Add randome IDs to data frame
#'
#' @param df Data frame with which to add new random ID
#' @param ID The existing ID field.
#' @param drop Whether to drop the original ID field. If
#'     ID field is NOT dropped, it will be renamed OID.
#'     (default=TRUE)
#' @importFrom rlang := .data
#' @importFrom magrittr `%>%`
#' @importFrom dplyr select rename mutate distinct arrange left_join
#' @export
add_random_ids <- function(df, ID, drop=TRUE) {
    # Make the
    #random_numbers <- OID <- NID <- NULL

    IDs <- df %>% dplyr::select( {{ ID }} ) %>% dplyr::distinct()

    ID_name <- names( IDs )

    id_count <- length(IDs[[1]])
    IDs$random_numbers <- stats::runif(n=id_count, min=1, max = 50000)

    IDs %<>% dplyr::arrange( .data$random_numbers )
    IDs$NID <- seq( from=1, to=id_count, by=1 )
    IDs$random_numbers <- NULL

    df %<>% dplyr::left_join( IDs, by = ID_name ) %>%
        dplyr::rename( .data$OID := {{ ID }} ) %>%
        dplyr::mutate( {{ ID }} := .data$NID ) %>%
        dplyr::select( -.data$NID )

    if (drop) {
        df %<>% dplyr::select( -.data$OID )
    }

    df
}
