formatNumbers <- function(wb,meta,k) {

    nsheets <- length(names(wb))


        df <- openxlsx::read.xlsx(wb, sheet = k, colNames = FALSE)

        digit_mask <- sapply(df, function(cell) grepl("^\\d+$", cell))

        table_range <- attr(openxlsx::getTables(wb,k),"refs")
        table_row_range <- stringr::str_extract_all(table_range, "\\d+")[[1]]
        table_row_start <- as.numeric(table_row_range[[1]])
        table_row_end <- as.numeric(table_row_range[[2]])

        col_indices <- seq_len(ncol(df))

        for (i in table_row_start:table_row_end) {
            for (j in col_indices) {
                if (digit_mask[i, j]) {
                    numeric_value <- as.numeric(df[i, j])
                    openxlsx::writeData(
                        wb,
                        sheet = k,
                        x = numeric_value,
                        startCol = j,
                        startRow = i,
                        colNames = FALSE,
                        rowNames = FALSE)
                }

                if (length(meta$columns[[j]]$meta$number_format) > 0) {
                    style <- openxlsx::createStyle(numFmt = meta$columns[[j]]$meta$number_format)
                    openxlsx::addStyle(
                        wb,
                        sheet = k,
                        style = style,
                        rows = i,
                        cols = j,
                        gridExpand = FALSE,
                        stack = TRUE)
                }

                cell_value <- names(openxlsx::read.xlsx(wb, sheet = k, rows = i, cols = j))

                if (length(cell_value) == 0 || is.na(cell_value) || cell_value == "") {
                    openxlsx::writeData(
                        wb,
                        sheet = k,
                        x = "[x]",
                        startRow = i,
                        startCol = j)
                }


            }
        }



    return(wb)

}
