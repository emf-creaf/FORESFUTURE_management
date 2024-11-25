library(tidyverse)
library(Hmisc)

firewood_species <- c("Quercus ilex", "Quercus ilex ssp. ballota", 
                      "Quercus pubescens", "Quercus pubescens (Q. humilis)",
                      "Quercus faginea", "Quercus cerrioides", "Erica arborea")

climate_model <- "mpiesm_rca4"

# Generates FES indicators for each year and plot in a selected province, climate model, climate scenario and management scenario
scenario_annual_province_indicators <- function(iprov, climate_model, climate_scen, management_scen, test = FALSE, formes = FALSE) {
  provinces <- c(8,17,25,43)
  provinceStrings <- c("Barcelona", "Girona", "Lleida", "Tarragona")
  
  provinceCode <- provinces[iprov]
  provinceName <- provinceStrings[iprov]

  
  if(!formes) {
    if(test) {
      bind_file <- paste0("Rdata/MEDFATE/Test_binded/Test_", provinceName, "_", management_scen, "_", climate_model,"_", climate_scen, ".rds")
    } else {
      bind_file <- paste0("Rdata/MEDFATE/binded/", provinceName, "_", management_scen, "_", climate_model,"_", climate_scen, ".rds")
    }
  } else {
    bind_file <- paste0("Rdata/FORMES/binded/", provinceName, "_", management_scen, "_", climate_model,"_", climate_scen, ".rds")
  }
  if(!file.exists(bind_file)) return(data.frame())
  
  scen_list <- readRDS(file = bind_file)
  
  tt <- scen_list$tree_table
  ctt <- scen_list$cut_tree_table
  cst <- scen_list$cut_shrub_table
  dtt <- scen_list$dead_tree_table
  st <- scen_list$shrub_table
  dst <- scen_list$dead_shrub_table
  summary_table <- NULL
  if(!formes) {
    summary_table <- scen_list$summary_table
  } else {
    bind_file <- paste0("Rdata/FORMES/wb_summary_binded/", provinceName, "_", management_scen, "_", climate_model,"_", climate_scen, ".rds")
    if(file.exists(bind_file))   summary_table <- readRDS(file = bind_file)
  }
  
  if(!formes) {
    tt_na <- tt |>
      tidyr::replace_na(list(Year = 2000))
    ctt_na <- ctt |>
      tidyr::replace_na(list(Year = 2000))
    dtt_na <- dtt |>
      tidyr::replace_na(list(Year = 2000))
  } else {
    tt_na <- tt
    tt_na$Management <- management_scen
    ctt_na <- ctt[ctt$Year!="2000", ]
    dtt_na <- dtt[ctt$Year!="2000", ]
    ctt_na$Management <- management_scen
    dtt_na$Management <- management_scen
    tt_na$Province[tt_na$Province=="LLeida"] <- "Lleida"
    ctt_na$Province[ctt_na$Province=="LLeida"] <- "Lleida"
    dtt_na$Province[dtt_na$Province=="LLeida"] <- "Lleida"
  }
  
  # Structure (all trees)
  struct_trees <- tt_na |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(TreeRichness = length(unique(Species)), .groups = "drop") 
  
  
  wtd.var2 <-function(x, weights ) {
    if(sum((!is.na(x)) & (!is.na(weights)))>1 && sum(weights, na.rm = TRUE) >1) {
      return(wtd.var(x, weights = weights, na.rm=TRUE))
    }
    return(NA)
  }
  # Structure (no saplings)
  struct_nosapl <- tt_na |>
    dplyr::filter(DBH> 7.5) |>
    dplyr::mutate(BA = N*pi*(DBH/200)^2) |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year)|>
    dplyr::summarise(TreeDensity = sum(N, na.rm=TRUE), 
                     BasalArea = sum(BA, na.rm=TRUE), 
                     meanDBH = weighted.mean(DBH, weights = N, na.rm=TRUE),
                     sdDBH  = sqrt(wtd.var2(DBH, weights = N)),
                     maxDBH = max(DBH, na.rm = TRUE),
                     meanHeight = weighted.mean(Height, weights = N, na.rm=TRUE),
                     maxHeight = max(Height, na.rm = TRUE),
                     sdHeight  = sqrt(wtd.var2(Height, weights = N)),
                     .groups = "drop") |>
    dplyr::mutate(QMD = sqrt(BasalArea/(0.00007854*TreeDensity)),
                  cvDBH = sdDBH/meanDBH)

  # Structure shrubs
  if(!formes) {
    struct_shrub <- st |>
      tidyr::replace_na(list(Year = 2000)) |>
      dplyr::select(-Step) |>
      dplyr::group_by(Climate, Management, Province, id, Year) |>
      dplyr::summarise(ShrubCover = min(100, sum(Cover, na.rm=TRUE)), 
                       MaxShrubCover = min(100, max(Cover, na.rm=TRUE)), 
                       ShrubRichness = length(unique(Species)), .groups = "drop")
  }
  
  # Struct all
  if(!formes) {
    struct <- struct_trees |>
      dplyr::full_join(struct_nosapl, by=c("Climate", "Management", "Province", "id", "Year"))|>
      dplyr::full_join(struct_shrub, by=c("Climate", "Management", "Province", "id", "Year"))
  } else {
    struct <- struct_trees |>
      dplyr::full_join(struct_nosapl, by=c("Climate", "Management", "Province", "id", "Year")) |>
      mutate(ShrubCover = NA,
             MaxShrubCover = NA,
             ShrubRichness = NA)
  }
  
  # Volume stock
  all_volume_stock <- tt_na |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(AllVolume = sum(Volume, na.rm=TRUE), .groups = "drop")
  struct_volume_stock <- tt_na |>
    dplyr::filter(DBH> 7.5) |>
    dplyr::filter((DBH > 22.5) & !(Species %in% firewood_species)) |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(VolumeStructure = sum(Volume, na.rm=TRUE), .groups = "drop")
  firewood_volume_stock <- tt_na |>
    dplyr::filter(DBH> 7.5) |>
    dplyr::filter((DBH <= 22.5) | (Species %in% firewood_species)) |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(VolumeAdultFirewood = sum(Volume, na.rm=TRUE), .groups = "drop")
  sapling_firewood_volume_stock <- tt_na |>
    dplyr::filter(DBH<= 7.5) |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(VolumeSaplingFirewood = sum(Volume, na.rm=TRUE), .groups = "drop")
  volume_stock <- all_volume_stock |>
    dplyr::full_join(struct_volume_stock, by=c("Climate", "Management", "Province", "id", "Year")) |>
    dplyr::full_join(firewood_volume_stock, struct_volume_stock, by=c("Climate", "Management", "Province", "id", "Year"))|>
    dplyr::full_join(sapling_firewood_volume_stock, struct_volume_stock, by=c("Climate", "Management", "Province", "id", "Year"))
  
  
    
  # Volume cut
  struct_volume_cut <- ctt_na |>
    dplyr::filter(DBH> 7.5) |>
    dplyr::filter((DBH > 22.5) & !(Species %in% firewood_species)) |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(CutStructure = sum(Volume, na.rm=TRUE)/10, .groups = "drop") # we divide by 10 to have annual rate
  firewood_volume_cut <- ctt_na |>
    dplyr::filter(DBH> 7.5) |>
    dplyr::filter((DBH <= 22.5) | (Species %in% firewood_species)) |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(CutAdultFirewood = sum(Volume, na.rm=TRUE)/10, .groups = "drop")  # we divide by 10 to have annual rate
  sapling_firewood_volume_cut <- ctt_na |>
    dplyr::filter(DBH <= 7.5) |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(CutSaplingFirewood = sum(Volume, na.rm=TRUE)/10, .groups = "drop") # we divide by 10 to have annual rate
  volume_cut <- struct_volume_cut |>
    dplyr::full_join(firewood_volume_cut, by=c("Climate", "Management", "Province", "id", "Year"))|>
    dplyr::full_join(sapling_firewood_volume_cut, by=c("Climate", "Management", "Province", "id", "Year")) |>
    tidyr::replace_na(list(CutAdultFirewood = 0, CutStructure = 0, CutSaplingFirewood = 0)) |>
    dplyr::mutate(CutAll = CutStructure+CutAdultFirewood+ CutSaplingFirewood) 

  
  # Biomass stock
  biom_adult_trees <- tt_na |>
    dplyr::filter(DBH> 7.5) |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(AdultTreeBiomass = sum(Aerial+Roots, na.rm=TRUE), .groups = "drop")
  biom_sapling_trees <- tt_na |>
    dplyr::filter(DBH <= 7.5) |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(SaplingTreeBiomass = sum(Aerial+Roots, na.rm=TRUE), .groups = "drop")
  if(!formes) {
    biom_shrubs <- st |>
      tidyr::replace_na(list(Year = 2000)) |>
      dplyr::select(-Step) |>
      dplyr::group_by(Climate, Management, Province, id, Year) |>
      dplyr::summarise(ShrubBiomass = sum(Aerial+Roots, na.rm=TRUE), .groups = "drop")
    biom_live <- biom_adult_trees |>
      dplyr::full_join(biom_shrubs, by=c("Climate", "Management", "Province", "id", "Year")) |>
      dplyr::full_join(biom_sapling_trees, by=c("Climate", "Management", "Province", "id", "Year")) |>
      dplyr::mutate(LiveBiomass = AdultTreeBiomass + SaplingTreeBiomass +ShrubBiomass)
  } else {
    biom_live <- biom_adult_trees |>
      dplyr::mutate(LiveBiomass = AdultTreeBiomass,
                    SaplingTreeBiomass = NA,
                    ShrubBiomass = NA)
  }

  # Biomass cut
  # Only count Aerial part
  adult_tree_biom_cut <- ctt_na |>
    dplyr::filter(DBH > 7.5) |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(CutBiomassAdultTree = sum(Aerial, na.rm=TRUE)/10, .groups = "drop") # we divide by 10 to have annual rate
  struct_biom_cut <- ctt_na |>
    dplyr::filter(DBH> 7.5) |>
    dplyr::filter((DBH > 22.5) & !(Species %in% firewood_species)) |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(CutBiomassStructure = sum(Aerial, na.rm=TRUE)/10, .groups = "drop") # we divide by 10 to have annual rate
  firewood_biom_cut <- ctt_na |>
    dplyr::filter(DBH> 7.5) |>
    dplyr::filter((DBH <= 22.5) | (Species %in% firewood_species)) |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(CutBiomassAdultFirewood = sum(Aerial, na.rm=TRUE)/10, .groups = "drop") # we divide by 10 to have annual rate
  sapling_tree_biom_cut <- ctt_na |>
    dplyr::filter(DBH <= 7.5) |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(CutBiomassSaplingTree = sum(Aerial, na.rm=TRUE)/10, .groups = "drop") # we divide by 10 to have annual rate
  if(!formes) {
    shrub_biom_cut <- cst |>
      tidyr::replace_na(list(Year = 2000)) |>
      dplyr::select(-Step) |>
      dplyr::group_by(Climate, Management, Province, id, Year) |>
      dplyr::summarise(CutBiomassShrub = sum(Aerial, na.rm=TRUE), .groups = "drop") 
    biom_cut <- adult_tree_biom_cut |>
      dplyr::full_join(shrub_biom_cut, by=c("Climate", "Management", "Province", "id", "Year")) |>
      dplyr::full_join(sapling_tree_biom_cut, by=c("Climate", "Management", "Province", "id", "Year")) |>
      dplyr::full_join(struct_biom_cut, by=c("Climate", "Management", "Province", "id", "Year")) |>
      dplyr::full_join(firewood_biom_cut, by=c("Climate", "Management", "Province", "id", "Year")) |>
      tidyr::replace_na(list(CutBiomassAdultTree = 0, CutBiomassSaplingTree = 0, CutBiomassShrub = 0,
                             CutBiomassStructure = 0, CutBiomassAdultFirewood = 0)) |>
      dplyr::mutate(CutBiomassAll = CutBiomassAdultTree+CutBiomassSaplingTree+CutBiomassShrub) 
  } else {
    biom_cut <- adult_tree_biom_cut |>
      dplyr::full_join(struct_biom_cut, by=c("Climate", "Management", "Province", "id", "Year")) |>
      dplyr::full_join(firewood_biom_cut, by=c("Climate", "Management", "Province", "id", "Year")) |>
      tidyr::replace_na(list(CutBiomassAdultTree = 0, CutBiomassStructure = 0, CutBiomassAdultFirewood = 0)) |>
      dplyr::mutate(CutBiomassAll = CutBiomassAdultTree,
                    CutBiomassSaplingTree= 0,
                    CutBiomassShrub=0) 
  }
  
  # Biomass dead
  tree_biom_dead <- dtt_na |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(TreeDeadBiomass = sum(Aerial+Roots, na.rm=TRUE)/10,# we divide by 10 to have annual rate
                     N_dead = sum(N)/10, # we divide by 10 to have annual rate
                     N_starvation = sum(N_starvation)/10, # we divide by 10 to have annual rate
                     N_dessication = sum(N_dessication)/10, .groups = "drop") |># we divide by 10 to have annual rate
    dplyr::mutate(TreeDeadBiomass_starvation = TreeDeadBiomass*N_starvation/N_dead,
                  TreeDeadBiomass_dessication = TreeDeadBiomass*N_dessication/N_dead,
                  TreeDeadBiomass_other = TreeDeadBiomass - TreeDeadBiomass_starvation - TreeDeadBiomass_dessication) |>
    dplyr::select(-c(N_dead, N_starvation, N_dessication))
  
  if(!formes) {
    shrub_biom_dead <- dst |>
      tidyr::replace_na(list(Year = 2000)) |>
      dplyr::select(-Step) |>
      dplyr::group_by(Climate, Management, Province, id, Year) |>
      dplyr::summarise(ShrubDeadBiomass = sum(Aerial+Roots, na.rm=TRUE),
                       Cover_dead = sum(Cover),
                       Cover_starvation = sum(Cover_starvation), 
                       Cover_dessication = sum(Cover_dessication), .groups = "drop") |>
      dplyr::mutate(ShrubDeadBiomass_starvation = ShrubDeadBiomass*Cover_starvation/Cover_dead,
                    ShrubDeadBiomass_dessication = ShrubDeadBiomass*Cover_dessication/Cover_dead,
                    ShrubDeadBiomass_other = ShrubDeadBiomass - ShrubDeadBiomass_starvation - ShrubDeadBiomass_dessication) |>
      dplyr::select(-c(Cover_dead, Cover_starvation, Cover_dessication))
    biom_dead <- tree_biom_dead |>
      dplyr::full_join(shrub_biom_dead, by=c("Climate", "Management", "Province", "id", "Year")) |>
      tidyr::replace_na(list(TreeDeadBiomass = 0, ShrubDeadBiomass = 0,
                             TreeDeadBiomass_other = 0, ShrubDeadBiomass_other = 0,
                             TreeDeadBiomass_starvation = 0, ShrubDeadBiomass_starvation = 0,
                             TreeDeadBiomass_dessication = 0, ShrubDeadBiomass_dessication = 0)) |>
      dplyr::mutate(DeadBiomass = TreeDeadBiomass+ShrubDeadBiomass,
                    DeadBiomass_other = TreeDeadBiomass_other + ShrubDeadBiomass_other,
                    DeadBiomass_starvation = TreeDeadBiomass_starvation + ShrubDeadBiomass_starvation,
                    DeadBiomass_dessication = TreeDeadBiomass_dessication + ShrubDeadBiomass_dessication)
  } else {
    biom_dead <- tree_biom_dead  |>
      tidyr::replace_na(list(TreeDeadBiomass = 0, 
                             TreeDeadBiomass_other = 0, 
                             TreeDeadBiomass_starvation = 0, 
                             TreeDeadBiomass_dessication = 0)) |>
      dplyr::mutate(ShrubDeadBiomass = 0,
                    ShrubDeadBiomass_other = 0,
                    ShrubDeadBiomass_starvation = 0,
                    ShrubDeadBiomass_dessication = 0,
                    DeadBiomass = TreeDeadBiomass,
                    DeadBiomass_other = TreeDeadBiomass_other,
                    DeadBiomass_starvation = TreeDeadBiomass_starvation,
                    DeadBiomass_dessication = TreeDeadBiomass_dessication)
    
  }
  
  
  if(!is.null(summary_table)) {
    summary_table <- summary_table |>
      dplyr::mutate(PPET = Precipitation/PET,
                    BlueWater = Runoff+DeepDrainage,
                    RunoffCoefficient = BlueWater/Precipitation,
                    RegulationCoefficient = DeepDrainage/BlueWater) |>
      dplyr::group_by(Climate, Management, Province, id) |>
      dplyr::mutate(CumulativeDeepDrainage = cumsum(DeepDrainage),
                    CumulativeBlueWater = cumsum(BlueWater),
                    CumulativePrecipitation = cumsum(Precipitation),
                    CumulativePET = cumsum(PET))|>
      dplyr::ungroup() |>
      dplyr::mutate(CumulativeRunoffCoefficient = CumulativeBlueWater/CumulativePrecipitation,
                    CumulativeRegulation = CumulativeDeepDrainage/CumulativeBlueWater,
                    CumulativePPET = CumulativePrecipitation/CumulativePET,
                    Year = as.numeric(Year))
    if(!formes) {
      summary_table <- summary_table|>
        mutate(WaterUseEfficiency = NetPrimaryProduction/Transpiration,
               CarbonUseEfficiency = SynthesisRespiration/NetPrimaryProduction)
    } else {
      summary_table <- summary_table |>
        mutate(id = as.character(as.numeric(substr(id,1,6))),
               Year = as.character(cut(Year, breaks = seq(2000,2100, by = 10), 
                                         labels = paste0(seq(2001,2091, by=10), "-",seq(2010,2100, by=10))))) |>
        group_by(Climate, Management, Province, id, Year) |>
        summarise_all(mean, na.rm = TRUE)
        
    }
  }

  annual_vol_biom <- struct |>
    dplyr::full_join(volume_stock, by=c("Climate", "Management", "Province", "id", "Year"))
  annual_vol_biom <- annual_vol_biom |>
    dplyr::full_join(volume_cut, by=c("Climate", "Management", "Province", "id", "Year"))
  annual_vol_biom <- annual_vol_biom |>
    dplyr::full_join(biom_cut, by=c("Climate", "Management", "Province", "id", "Year"))
  annual_vol_biom <- annual_vol_biom |>
    dplyr::full_join(biom_live, by=c("Climate", "Management", "Province", "id", "Year")) |>
    dplyr::full_join(biom_dead, by=c("Climate", "Management", "Province", "id", "Year")) 
  if(!is.null(summary_table)) {
    annual_vol_biom <- annual_vol_biom |>
      dplyr::left_join(summary_table, by=c("Climate", "Management", "Province", "id", "Year"))
  }
  annual_vol_biom <- annual_vol_biom |>
    tidyr::replace_na(list(CutAll = 0, CutAdultFirewood = 0, CutSaplingFirewood = 0, CutStructure = 0,
                           AllVolume = 0, VolumeStructure = 0, VolumeAdultFireWood = 0,VolumeSaplingFireWood = 0,
                           AdultTreeBiomass = 0, SaplingTreeBiomass = 0, ShrubBiomass = 0, LiveBiomass = 0,
                           CutBiomassAdultTree = 0, CutBiomassSaplingTree = 0, CutBiomassShrub = 0, 
                           CutBiomassStructure = 0, CutBiomassAdultFirewood = 0, CutBiomassAll = 0,
                           DeadBiomass = 0, TreeDeadBiomass = 0, ShrubDeadBiomass = 0, 
                           DeadBiomass_starvation = 0, TreeDeadBiomass_starvation = 0, ShrubDeadBiomass_starvation = 0,
                           DeadBiomass_dessication = 0, TreeDeadBiomass_dessication = 0, ShrubDeadBiomass_dessication = 0,
                           DeadBiomass_other = 0, TreeDeadBiomass_other = 0, ShrubDeadBiomass_other = 0)) |>
    dplyr::arrange(Climate, Management, Province, id, Year) 
  
  annual_vol_biom <- annual_vol_biom |>
    dplyr::group_by(Climate, Management, Province, id) |>
    dplyr::mutate(CumulativeCutAdultFirewood = cumsum(CutAdultFirewood),
                  CumulativeCutSaplingFirewood = cumsum(CutSaplingFirewood),
                  CumulativeCutStructure = cumsum(CutStructure), 
                  CumulativeCutAll = cumsum(CutAll),
                  CumulativeDeadBiomass = cumsum(DeadBiomass),
                  CumulativeDeadBiomass_starvation = cumsum(DeadBiomass_starvation),
                  CumulativeDeadBiomass_dessication = cumsum(DeadBiomass_dessication),
                  CumulativeDeadBiomass_other = cumsum(DeadBiomass_other),
                  CumulativeTreeDeadBiomass = cumsum(TreeDeadBiomass),
                  CumulativeTreeDeadBiomass_starvation = cumsum(TreeDeadBiomass_starvation),
                  CumulativeTreeDeadBiomass_dessication = cumsum(TreeDeadBiomass_dessication),
                  CumulativeTreeDeadBiomass_other = cumsum(TreeDeadBiomass_other),
                  CumulativeShrubDeadBiomass = cumsum(ShrubDeadBiomass),
                  CumulativeShrubDeadBiomass_starvation = cumsum(ShrubDeadBiomass_starvation),
                  CumulativeShrubDeadBiomass_dessication = cumsum(ShrubDeadBiomass_dessication),
                  CumulativeShrubDeadBiomass_other = cumsum(ShrubDeadBiomass_other)) |>
    tidyr::replace_na(list(CumulativeCutAdultFirewood = 0, 
                           CumulativeCutSaplingFirewood = 0, 
                           CumulativeCutStructure = 0, 
                           CumulativeCutAll = 0))
  return(annual_vol_biom)
}

# Generates FES indicators for each year and plot for the whole study area in a selected climate model, climate scenario and management scenario
scenario_annual_indicators<-function(climate_model, climate_scen, management_scen, test = FALSE, formes = FALSE) {
  BCN <- scenario_annual_province_indicators(1, climate_model, climate_scen, management_scen, test, formes)
  GIR <- scenario_annual_province_indicators(2, climate_model, climate_scen, management_scen, test, formes)
  LLE <- scenario_annual_province_indicators(3, climate_model, climate_scen, management_scen, test, formes)
  TAR <- scenario_annual_province_indicators(4, climate_model, climate_scen, management_scen, test, formes)
  if(!formes) {
    if(test) {
      saveRDS(bind_rows(BCN, GIR, LLE, TAR),
              file = paste0("Rdata/MEDFATE/Test_annual_indicators/Test_", management_scen, "_", climate_model, "_", climate_scen, ".rds"))
    } else {
      saveRDS(bind_rows(BCN, GIR, LLE, TAR),
              file = paste0("Rdata/MEDFATE/annual_indicators/", management_scen, "_", climate_model, "_", climate_scen, ".rds"))
    }
  } else {
    saveRDS(bind_rows(BCN, GIR, LLE, TAR),
            file = paste0("Rdata/FORMES/annual_indicators_1206/", management_scen, "_", climate_model, "_", climate_scen, ".rds"))
  }
}

scenario_annual_province_species_abundance <- function(iprov, climate_model, climate_scen, management_scen, test = FALSE) {
  
  provinces <- c(8,17,25,43)
  provinceStrings <- c("Barcelona", "Girona", "Lleida", "Tarragona")
  
  provinceCode <- provinces[iprov]
  provinceName <- provinceStrings[iprov]
  
  if(test) {
    bind_file <- paste0("Rdata/Test_binded/Test_", provinceName, "_", management_scen, "_", climate_model,"_", climate_scen, ".rds")
  } else {
    bind_file <- paste0("Rdata/binded/", provinceName, "_", management_scen, "_", climate_model,"_", climate_scen, ".rds")
  }
  if(!file.exists(bind_file)) return(data.frame())
  
  scen_list <- readRDS(file = bind_file)
  
  tt <- scen_list$tree_table
  st <- scen_list$shrub_table
  
  tree_species_abundance <- tt |>
    dplyr::group_by(Climate, Management, Province, Step, Year, Species) |>
    dplyr::summarise(Volume = sum(Volume, na.rm=TRUE),
                     Aerial = sum(Aerial, na.rm=TRUE),
                     Roots = sum(Roots, na.rm=TRUE),
                     .groups = "drop") |>
    dplyr::mutate(GrowthForm = "Tree")

  shrub_species_abundance <- st |>
    dplyr::group_by(Climate, Management, Province, Step, Year, Species) |>
    dplyr::summarise(Aerial = sum(Aerial, na.rm=TRUE),
                     Roots = sum(Roots, na.rm=TRUE),
                     .groups = "drop") |>
    dplyr::mutate(GrowthForm = "Shrub") 
  
  species_abundance <- tree_species_abundance |>
    dplyr::bind_rows(shrub_species_abundance)
  return(species_abundance)
}

scenario_species_abundance<-function(climate_model, climate_scen, management_scen, test = FALSE) {
  BCN <- scenario_annual_province_species_abundance(1, climate_model, climate_scen, management_scen, test)
  GIR <- scenario_annual_province_species_abundance(2, climate_model, climate_scen, management_scen, test)
  LLE <- scenario_annual_province_species_abundance(3, climate_model, climate_scen, management_scen, test)
  TAR <- scenario_annual_province_species_abundance(4, climate_model, climate_scen, management_scen, test)
  if(test) {
    saveRDS(bind_rows(BCN, GIR, LLE, TAR),
            file = paste0("Rdata/Test_species_abundance/Test_", management_scen, "_", climate_model, "_", climate_scen, ".rds"))
  } else {
    saveRDS(bind_rows(BCN, GIR, LLE, TAR),
            file = paste0("Rdata/species_abundance/", management_scen, "_", climate_model, "_", climate_scen, ".rds"))
  }
  
}


# (1) BAU
# scenario_annual_indicators(climate_model, "rcp45", "BAU", test = TRUE)
# scenario_annual_indicators(climate_model, "rcp85", "BAU", test = TRUE)
# scenario_annual_indicators(climate_model, "rcp45", "BAU", test = FALSE)
# scenario_annual_indicators(climate_model, "rcp85", "BAU", test = FALSE)
scenario_annual_indicators(climate_model, "rcp45", "BAU", test = FALSE, formes = TRUE)
scenario_annual_indicators(climate_model, "rcp85", "BAU", test = FALSE, formes = TRUE)

# (2) AMF
# scenario_annual_indicators(climate_model, "rcp45", "AMF", test = TRUE)
# scenario_annual_indicators(climate_model, "rcp85", "AMF", test = TRUE)
# scenario_annual_indicators(climate_model, "rcp45", "AMF", test = FALSE)
# scenario_annual_indicators(climate_model, "rcp85", "AMF", test = FALSE)
scenario_annual_indicators(climate_model, "rcp45", "AMF", test = FALSE, formes = TRUE)
scenario_annual_indicators(climate_model, "rcp85", "AMF", test = FALSE, formes = TRUE)

# (3) RSB
# scenario_annual_indicators(climate_model, "rcp45", "RSB", test = TRUE)
# scenario_annual_indicators(climate_model, "rcp85", "RSB", test = TRUE)
# scenario_annual_indicators(climate_model, "rcp45", "RSB", test = FALSE)
# scenario_annual_indicators(climate_model, "rcp85", "RSB", test = FALSE)
scenario_annual_indicators(climate_model, "rcp45", "RSB", test = FALSE, formes = TRUE)
scenario_annual_indicators(climate_model, "rcp85", "RSB", test = FALSE, formes = TRUE)

# (4) ASEA
# scenario_annual_indicators(climate_model, "rcp45", "ASEA", test = TRUE)
# scenario_annual_indicators(climate_model, "rcp85", "ASEA", test = TRUE)
# scenario_annual_indicators(climate_model, "rcp45", "ASEA", test = FALSE)
# scenario_annual_indicators(climate_model, "rcp85", "ASEA", test = FALSE)
scenario_annual_indicators(climate_model, "rcp45", "ASEA", test = FALSE, formes = TRUE)
scenario_annual_indicators(climate_model, "rcp85", "ASEA", test = FALSE, formes = TRUE)

# (5) ACG
# scenario_annual_indicators(climate_model, "rcp45", "ACG", test = TRUE)
# scenario_annual_indicators(climate_model, "rcp85", "ACG", test = TRUE)
# scenario_annual_indicators(climate_model, "rcp45", "ACG", test = FALSE)
# scenario_annual_indicators(climate_model, "rcp85", "ACG", test = FALSE)
scenario_annual_indicators(climate_model, "rcp45", "ACG", test = FALSE, formes = TRUE)
scenario_annual_indicators(climate_model, "rcp85", "ACG", test = FALSE, formes = TRUE)

# (6) NOG
# scenario_annual_indicators(climate_model, "rcp45", "NOG", test = TRUE)
# scenario_annual_indicators(climate_model, "rcp85", "NOG", test = TRUE)
# scenario_annual_indicators(climate_model, "rcp45", "NOG", test = FALSE)
# scenario_annual_indicators(climate_model, "rcp85", "NOG", test = FALSE)
# scenario_annual_indicators(climate_model, "rcp45", "NOG", test = FALSE, formes = TRUE)
# scenario_annual_indicators(climate_model, "rcp85", "NOG", test = FALSE, formes = TRUE)