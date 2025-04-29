#' @export
createTable <- function(table_name,env="dev",run_time="2025-04-29T19:16:55",github_action_name,
                        cover_table = aftables::demo_aftable$table[[1]],
                        notes_table = aftables::demo_aftable$table[[3]]) {

    afvalues <- list(tab_title = list(), table_title = list(), source = list(),table=list())

    for (i in 1:length(table_name)) {

    data <- getTable(table_name,env)

    meta <- getManifest(table_name,env,run_time,github_action_name)

    contents_df <- data.frame(
        "Sheet name" = c("Notes", meta$meta$tab_title),
        "Sheet title" = c(
            "Notes used in this workbook",
            meta$description
        ),
        check.names = FALSE
    )

    table <- renameColumns(data,meta)

    table_title <- paste(gsub("_"," ",meta$meta$tab_title),meta$description,sep=": ")

    table_title <- paste(table_title,paste0("[note ",meta$meta$notes,"]",collapse=" "))

    afvalues$tab_title[[i]] <- meta$meta$tab_title
    afvalues$table_title[[i]] <- table_title
    afvalues$source[[i]] <- meta$meta$source
    afvalues$table[[i]] <- table
    }


    aft <- aftables::create_aftable(
        tab_titles = c("Cover","Contents","Notes",afvalues$tab_title),
        sheet_types = c("cover", "contents", "notes", "tables"),
        sheet_titles = c("Cover","Contents","Notes",afvalues$table_title),
        sources = c(
            rep(NA_character_, 3),
            afvalues$source),
        tables = c(list(cover_table,contents_df,notes_table),afvalues$table)
    )

    wb <- aftables::generate_workbook(aft)

    wb <- formatNumbers(wb,meta)

    return(wb)

}
