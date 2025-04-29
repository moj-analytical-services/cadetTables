#' @export
renameColumns <- function(table,metadata) {

    df <- table

    columns <- metadata$columns

    description_lookup <- setNames(
        sapply(columns, function(col) col$description),
        names(columns)
    )

    new_colnames <- sapply(names(table), function(col_name) {
        if (col_name %in% names(description_lookup) &&
            nzchar(description_lookup[[col_name]])) {
            description_lookup[[col_name]]
        } else {
            col_name
        }

    })

    colnames(df) <- new_colnames

    return(df)

}
