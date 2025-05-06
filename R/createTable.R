#' @export
createTable <- function(table_name,env="dev",run_time="2025-05-06T08:50:12",github_action_name,
                        cover_table = aftables::demo_aftable$table[[1]],
                        notes_table = aftables::demo_aftable$table[[3]]) {

    afvalues <- list(tab_title = list(), table_title = list(), source = list(),table=list())

    contents_df <- data.frame(
      "Sheet name" = c("Notes"),
      "Sheet title" = c(
        "Notes used in this workbook"
      ),
      check.names = FALSE
    )

    for (i in 1:length(table_name)) {

    data <- getTable(table_name[i],env)

    meta <- getManifest(table_name[i],env,run_time,github_action_name)

    new_contents <- data.frame( "Sheet name" = meta$meta$tab_title,
                                "Sheet title" = meta$description)

    names(new_contents) <- names(contents_df)

    contents_df <- rbind(contents_df,new_contents)

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
        sheet_types = c("cover", "contents", "notes", rep("tables",length(table_name))),
        sheet_titles = c("Cover","Contents","Notes",afvalues$table_title),
        sources = c(
            rep(NA_character_, 3),
            afvalues$source),
        tables = c(list(cover_table,contents_df,notes_table),afvalues$table)
    )

    wb <- aftables::generate_workbook(aft)

    for (i in 1:length(table_name)) {

    meta <- getManifest(table_name[i],env,run_time,github_action_name)

    wb <- formatNumbers(wb,meta,i+3)

    }

    return(wb)

}
