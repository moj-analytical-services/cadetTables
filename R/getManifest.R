#' @export
getManifest <- function(table_name,env="dev",run_time="2025-04-24T12:35:18",github_action_name="deploy-daily") {

    path <- if (env=="dev") {
                paste0("mojap-derived-tables/dev/run_artefacts/deploy-dev/run_time=",run_time,"/target/manifest.json")
            } else {
                paste0("mojap-derived-tables/prod/run_artefacts/",github_action_name,"/latest/target/manifest.json")
            }

    data <- Rs3tools::read_using(FUN=jsonlite::fromJSON,
                                 s3_path = path, simplifyVector = FALSE)

    key <- paste0("model.mojap_derived_tables.",
                  gsub("\\.", "__", table_name)
                  )

    return(data$nodes[[key]])

}
