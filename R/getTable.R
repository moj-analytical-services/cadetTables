getTable <- function(table_name, env="prod") {

    table_name_env <- if (env == "dev") { gsub("__", "_dev_dbt__", table_name) } else table_name

    con <- Rdbtools::connect_athena()
    data <- Rdbtools::dbGetQuery(con,
                                 paste("SELECT * FROM",
                                       gsub("__", "\\.", table_name_env)
                                       )
                                 )
    Rdbtools::dbDisconnect(con)

    return(data)

}
