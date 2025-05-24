rm(list=ls())
library(tidyverse)
library(tidyterra)
library(medfate)
library(sf)


# Load spatial data -------------------------------------------------------
nfiplot <- dplyr::bind_rows(readRDS(paste0("data/nfiplot.rds")))
lonlat <- sf::st_transform(nfiplot[,"id"], crs = 4326)
lonlat$longitude <- sf::st_coordinates(lonlat)[,1]
longitude_df <- sf::st_drop_geometry(lonlat)
K_LS <- sf::read_sf("data-raw/ErosionData.gpkg")

# ES1 - Annual timber extraction (m3/ha/yr) -----------
ES1_function_annual <- function(ALL) {
  ES1 <- ALL |>
    ungroup() |>
    filter(Year!=2000) |>
    select(Climate, Management, Province, id, Year, CutStructure, CutAdultFirewood, CutSaplingFirewood) |>
    group_by(Climate, Management, Province) |>
    complete(id, Year, fill = list(CutStructure = 0, CutAdultFirewood =0, CutSaplingFirewood = 0)) |>
    rename(ES1_CutStructure = CutStructure,
           ES1_CutAdultFirewood = CutAdultFirewood,
           ES1_CutSaplingFirewood = CutSaplingFirewood)|>
    dplyr::mutate(Climate = toupper(Climate))
  return(ES1)
}
# ES1 - Timber stocks (m3/ha/yr) every year -----------
ES1_function_state_annual <- function(ALL) {
  ES1 <- ALL |>
    ungroup() |>
    select(Climate, Management, Province, id, Year, VolumeStructure, VolumeAdultFirewood, VolumeSaplingFirewood) |>
    group_by(Climate, Management, Province) |>
    complete(id, Year, fill = list(VolumeStructure = 0, VolumeAdultFirewood = 0, VolumeSaplingFirewood = 0))|>
    rename(ES1_VolumeStructure = VolumeStructure,
           ES1_VolumeAdultFirewood = VolumeAdultFirewood,
           ES1_VolumeSaplingFirewood = VolumeSaplingFirewood) |>
    tidyr::replace_na(list(ES1_VolumeStructure = 0, ES1_VolumeAdultFirewood = 0, ES1_VolumeSaplingFirewood = 0)) |>
    dplyr::mutate(Climate = toupper(Climate))
  return(ES1)
}

# ES2 - Carbon sequestration rate (Mg C/ha/yr) per year -----------
ES2_function_annual <- function(ALL) {
  ES2<-ALL |>
    ungroup() |>
    select(Climate, Management, Province, id, Year, AdultTreeBiomass, SaplingTreeBiomass, ShrubBiomass, 
           CutBiomassStructure, CutBiomassAdultFirewood) |>
    group_by(Climate, Management, Province) |>
    complete(id, Year, fill = list(AdultTreeBiomass = 0, SaplingTreeBiomass = 0))|>
    mutate(ES2_AdultTreeBiomassChange = AdultTreeBiomass - dplyr::lag(AdultTreeBiomass, n=1))|>
    mutate(ES2_SaplingTreeBiomassChange = SaplingTreeBiomass - dplyr::lag(SaplingTreeBiomass, n=1))|>
    mutate(ES2_ShrubBiomassChange = ShrubBiomass - dplyr::lag(ShrubBiomass, n=1))|>
    mutate(ES2_CutBiomassStructure = CutBiomassStructure,
           ES2_CutBiomassAdultFirewood = CutBiomassAdultFirewood,
           ES2_AdultTreeBiomassSequestr = ES2_AdultTreeBiomassChange + CutBiomassStructure,
           ES2_SaplingTreeBiomassSequestr = ES2_SaplingTreeBiomassChange,
           ES2_ShrubBiomassSequestr = ES2_ShrubBiomassChange,
           ES2_LiveBiomassSequestr = ES2_AdultTreeBiomassSequestr+ES2_SaplingTreeBiomassSequestr+ES2_ShrubBiomassSequestr) |>
    select(-AdultTreeBiomass, -SaplingTreeBiomass, -ShrubBiomass, -CutBiomassStructure, -CutBiomassAdultFirewood) |>
    filter(Year!=2000) |>
    dplyr::mutate(Climate = toupper(Climate))
  return(ES2)
}

# ES2 - Carbon stocks (Mg C/ha) per year -----------
ES2_function_state_annual <- function(ALL) {
  ES2 <- ALL |>
    ungroup() |>
    select(Climate, Management, Province, id, Year, AdultTreeBiomass, SaplingTreeBiomass, ShrubBiomass) |>
    group_by(Climate, Management, Province) |>
    complete(id, Year, fill = list(AdultTreeBiomass = 0, SaplingTreeBiomass = 0, ShrubBiomass = 0))|>
    rename(ES2_AdultTreeBiomass = AdultTreeBiomass,
           ES2_SaplingTreeBiomass = SaplingTreeBiomass,
           ES2_ShrubBiomass = ShrubBiomass) |>
    tidyr::replace_na(list(ES2_AdultTreeBiomass = 0, ES2_SaplingTreeBiomass = 0, ES2_ShrubBiomass = 0)) |>
    mutate(ES2_LiveBiomass = ES2_AdultTreeBiomass + ES2_SaplingTreeBiomass+ES2_ShrubBiomass)|> 
    dplyr::mutate(Climate = toupper(Climate))
  return(ES2)
}

# ES3 - Water provision (l/m3/yr) -----------------------------------------
ES3_function_annual <- function(ALL) {
  ES3 <- ALL |>
    ungroup() |>
    filter(Year!=2000) |>
    select(Climate, Management, Province, id, Year, LAI_max, BlueWater, Precipitation) |>
    rename(ES3_LAI = LAI_max,
           ES3_BlueWater = BlueWater,
           ES3_Precipitation = Precipitation) |>
    mutate(ES3_RunoffCoefficient = 100*(ES3_BlueWater/ES3_Precipitation)) |>
    dplyr::mutate(Climate = toupper(Climate))
  return(ES3)
}

# ES4 - Erosion control (Mg/ha/yr) -----------------------------------------------
ES4_function_annual <- function(ALL) {
  # constants
  a = 2
  b0 = 0.117
  b1 = -0.015

  ES4 <- ALL |>
    filter(Year!=2000) |>
    select(Climate, Management, Province, id, Year, Pdaymax, Precipitation, PARground)|>
    mutate(idparcela = as.character(as.numeric(substr(id, 1,6)))) |>
    left_join(longitude_df, by="id") |>
    mutate(ES4_RainfallErosivity = b0*Precipitation*sqrt(Pdaymax)*(a+b1*longitude),
           C = PARground/100) |>
    left_join(sf::st_drop_geometry(K_LS), by="id") |>
    mutate(ES4_StructuralImpact = Kst*LS*ES4_RainfallErosivity,
           ES4_ErosionMitigation = ES4_StructuralImpact*(1-C)) |>
    select(-c(longitude, idparcela, C, LS, K, Kst, Pdaymax, PARground, Precipitation)) |>
    dplyr::mutate(Climate = toupper(Climate))
  
  return(ES4)
}

# ES - ALL ----------------------------------------------------------------
generate_ES_table <- function(type = "period", test = FALSE) {
  ES_function<-function(type = "state", ALL) {
    if(type=="period") {
      ES <- ES1_function_annual(ALL) |>
        left_join(ES2_function_annual(ALL), by=c("Climate", "Management", "Province", "id", "Year"))|>
        left_join(ES3_function_annual(ALL), by=c("Climate", "Management", "Province", "id", "Year"))|>
        left_join(ES4_function_annual(ALL), by=c("Climate", "Management", "Province", "id", "Year"))
    } else {
      ES <- ES1_function_state_annual(ALL) |>
        left_join(ES2_function_state_annual(ALL), by=c("Climate", "Management", "Province", "id", "Year"))
    }
    return(ES)
  }
  
  if(test) {
    cli::cli_progress_step("BAU/RCP45")
    BAU_rcp45 <- ES_function(type, readRDS("data/MEDFATE/Test_annual_indicators/Test_BAU_mpiesm_rca4_rcp45.rds"))
    cli::cli_progress_step("BAU/RCP85")
    BAU_rcp85 <- ES_function(type, readRDS("data/MEDFATE/Test_annual_indicators/Test_BAU_mpiesm_rca4_rcp85.rds"))
    cli::cli_progress_step("AMF/RCP45")
    AMF_rcp45 <- ES_function(type, readRDS("data/MEDFATE/Test_annual_indicators/Test_AMF_mpiesm_rca4_rcp45.rds"))
    cli::cli_progress_step("AMF/RCP85")
    AMF_rcp85 <- ES_function(type, readRDS("data/MEDFATE/Test_annual_indicators/Test_AMF_mpiesm_rca4_rcp85.rds"))
    cli::cli_progress_step("RSB/RCP45")
    RSB_rcp45 <- ES_function(type, readRDS("data/MEDFATE/Test_annual_indicators/Test_RSB_mpiesm_rca4_rcp45.rds"))
    cli::cli_progress_step("RSB/RCP85")
    RSB_rcp85 <- ES_function(type, readRDS("data/MEDFATE/Test_annual_indicators/Test_RSB_mpiesm_rca4_rcp85.rds"))
    cli::cli_progress_step("ASEA/RCP45")
    ASEA_rcp45 <- ES_function(type, readRDS("data/MEDFATE/Test_annual_indicators/Test_ASEA_mpiesm_rca4_rcp45.rds"))
    cli::cli_progress_step("ASEA/RCP85")
    ASEA_rcp85 <- ES_function(type, readRDS("data/MEDFATE/Test_annual_indicators/Test_ASEA_mpiesm_rca4_rcp85.rds"))
    cli::cli_progress_step("ACG/RCP45")
    ACG_rcp45 <- ES_function(type, readRDS("data/MEDFATE/Test_annual_indicators/Test_ACG_mpiesm_rca4_rcp45.rds"))
    cli::cli_progress_step("ACG/RCP85")
    ACG_rcp85 <- ES_function(type, readRDS("data/MEDFATE/Test_annual_indicators/Test_ACG_mpiesm_rca4_rcp85.rds"))
  } else {
    cli::cli_progress_step("BAU/RCP45")
    BAU_rcp45 <- ES_function(type, readRDS("data/MEDFATE/annual_indicators/BAU_mpiesm_rca4_rcp45.rds"))
    cli::cli_progress_step("BAU/RCP85")
    BAU_rcp85 <- ES_function(type, readRDS("data/MEDFATE/annual_indicators/BAU_mpiesm_rca4_rcp85.rds"))
    cli::cli_progress_step("AMF/RCP45")
    AMF_rcp45 <- ES_function(type, readRDS("data/MEDFATE/annual_indicators/AMF_mpiesm_rca4_rcp45.rds"))
    cli::cli_progress_step("AMF/RCP85")
    AMF_rcp85 <- ES_function(type, readRDS("data/MEDFATE/annual_indicators/AMF_mpiesm_rca4_rcp85.rds"))
    cli::cli_progress_step("RSB/RCP45")
    RSB_rcp45 <- ES_function(type, readRDS("data/MEDFATE/annual_indicators/RSB_mpiesm_rca4_rcp45.rds"))
    cli::cli_progress_step("RSB/RCP85")
    RSB_rcp85 <- ES_function(type, readRDS("data/MEDFATE/annual_indicators/RSB_mpiesm_rca4_rcp85.rds"))
    cli::cli_progress_step("ASEA/RCP45")
    ASEA_rcp45 <- ES_function(type, readRDS("data/MEDFATE/annual_indicators/ASEA_mpiesm_rca4_rcp45.rds"))
    cli::cli_progress_step("ASEA/RCP85")
    ASEA_rcp85 <- ES_function(type, readRDS("data/MEDFATE/annual_indicators/ASEA_mpiesm_rca4_rcp85.rds"))
    cli::cli_progress_step("ACG/RCP45")
    ACG_rcp45 <- ES_function(type, readRDS("data/MEDFATE/annual_indicators/ACG_mpiesm_rca4_rcp45.rds"))
    cli::cli_progress_step("ACG/RCP85")
    ACG_rcp85 <- ES_function(type, readRDS("data/MEDFATE/annual_indicators/ACG_mpiesm_rca4_rcp85.rds"))
  }
  cli::cli_progress_step("Binding")
  ALL_ES <- bind_rows(BAU_rcp45, BAU_rcp85,
                      AMF_rcp45, AMF_rcp85,
                      RSB_rcp45, RSB_rcp85,
                      ASEA_rcp45, ASEA_rcp85,
                      ACG_rcp45, ACG_rcp85)

  return(ALL_ES)
}



# ES calculation ----------------------------------------------------------
# ES_period_FORDYN_test <- generate_ES_table("period", TRUE)
# ES_period_FORDYN_test <- ES_period_FORDYN_test |>
#   left_join(nfiplot[,c("id")], by="id") |>
#   sf::st_as_sf()
# saveRDS(ES_period_FORDYN_test, "data/ES_period_FORDYN_test_annual.rds")
# 
# ES_state_FORDYN_test <- generate_ES_table("state", TRUE)
# ES_state_FORDYN_test <- ES_state_FORDYN_test |>
#   left_join(nfiplot[,c("id")], by="id") |>
#   sf::st_as_sf()
# saveRDS(ES_state_FORDYN_test, "data/ES_state_FORDYN_test_annual.rds")

ES_period_MEDFATE <- generate_ES_table("period",FALSE)
ES_period_MEDFATE <- ES_period_MEDFATE |>
  left_join(nfiplot[,c("id")], by="id") |>
  sf::st_as_sf()
saveRDS(ES_period_MEDFATE, "data/ES_period_FORDYN_annual.rds")

ES_state_MEDFATE <- generate_ES_table("state", FALSE)
ES_state_MEDFATE <- ES_state_MEDFATE |>
  left_join(nfiplot[,c("id")], by="id") |>
  sf::st_as_sf()
saveRDS(ES_state_MEDFATE, "data/ES_state_MEDFATE_annual.rds")


