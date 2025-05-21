library(dplyr)
library(tidyr)

bind_tree_table <- function(test = FALSE, scenarios = c("BAU", "AMF", "RSB", "ASEA", "ACG", "NOG"), columnsToFilter = "") {
  df <- data.frame()
  for(provinceName in c("Barcelona", "Girona","Lleida", "Tarragona")) {
    for(management_scen in scenarios) {
      cli::cli_progress_step(paste0("Province: ", provinceName, " Scenario: ", management_scen))
      for(climate_model in c("mpiesm_rca4")) {
        for(climate_scen in c("rcp45", "rcp85")) {
          if(test) {
            bind_file <- paste0("data/MEDFATE/Test_binded/Test_", provinceName, "_", management_scen, "_", climate_model,"_", climate_scen, ".rds")
          } else {
            bind_file <- paste0("data/MEDFATE/binded/", provinceName, "_", management_scen, "_", climate_model,"_", climate_scen, ".rds")
          }
          if(file.exists(bind_file)) {
            scen_list <- readRDS(file = bind_file)
            df_i <- scen_list$tree_table 
            df_i <- df_i[,!(names(df_i) %in% columnsToFilter), drop = FALSE]
            df <- dplyr::bind_rows(df, df_i) 
          }
        }
      }
    }
  }
  return(df)
}

df <- bind_tree_table(scenarios = "BAU",
                      columnsToFilter = c("Volume", "Aerial", "Roots", "DBHclass"))
saveRDS(df, "data/MEDFATE/Tree_table_output/BAU_tree_table_output.rds")
gc()
df <- bind_tree_table(scenarios = "ACG",
                      columnsToFilter = c("Volume", "Aerial", "Roots", "DBHclass"))
saveRDS(df, "data/MEDFATE/Tree_table_output/ACG_tree_table_output.rds")
gc()
df <- bind_tree_table(scenarios = "RSB",
                      columnsToFilter = c("Volume", "Aerial", "Roots", "DBHclass"))
saveRDS(df, "data/MEDFATE/Tree_table_output/RSB_tree_table_output.rds")
gc()
df <- bind_tree_table(scenarios = "ASEA",
                      columnsToFilter = c("Volume", "Aerial", "Roots", "DBHclass"))
saveRDS(df, "data/MEDFATE/Tree_table_output/ASEA_tree_table_output.rds")
gc()
df <- bind_tree_table(scenarios = "AMF",
                      columnsToFilter = c("Volume", "Aerial", "Roots", "DBHclass"))
saveRDS(df, "data/MEDFATE/Tree_table_output/AMF_tree_table_output.rds")
gc()
df <- bind_tree_table(scenarios = "NOG",
                      columnsToFilter = c("Volume", "Aerial", "Roots", "DBHclass"))
saveRDS(df, "data/MEDFATE/Tree_table_output/NOG_tree_table_output.rds")
gc()