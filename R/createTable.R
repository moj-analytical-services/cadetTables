#' @export
createTable <- function(table_name,env="dev",run_time,github_action_name) {

    data <- getTable(table_name,env)

    meta <- getManifest(table_name,env,run_time,github_action_name)

    table <- renameColumns(data,meta)

    return(table)

}
