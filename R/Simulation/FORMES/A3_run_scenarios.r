#########################################################################################################
##
## Execute FORMES scenarios
##
#########################################################################################################

rm(list=ls())
library(sp)
library(sf)
library(tidyverse)
library(FORMES)
library(FORMESutils)
library(readxl)
options(dplyr.summarise.inform = FALSE)

# load demand function
source("R/Simulation/FORMES/A6_demand_function.R")
source("R/Simulation/FORMES/A3b_run_scenarios_function.R")

## A2.1 Build initial plotData ----------------------------------------------------------------------
nfiplot = readRDS("RData/nfiplot.rds")
# Variables:
# CCWD     CSWD      Rad     Temp     Prec       PET     SWHC 
# are not included in nfiplot as medfate does not need them.
# In the climatic datasets provided by Roberto, only Temp and Prec are available
# (already summarized by decade)

# Let's try to work with default plotDataIFN for all Catalonia. 
# To no use 'strata' to compute plot area, retrieve from nfiplot.
plotData = plotDataIFN[plotDataIFN$Province %in% c(8,17,25,43),]
nfiplot_area = st_drop_geometry(nfiplot[,c("IDPARCELA", "area")])
nfiplot_area$IDPARCELA[substr(nfiplot_area$IDPARCELA,1,1)=="0"] = 
  substr(nfiplot_area$IDPARCELA[substr(nfiplot_area$IDPARCELA,1,1)=="0"], 2, 20)
plotData = plotData %>% left_join(nfiplot_area, by=c("ID"="IDPARCELA"))
names(plotData)[ncol(plotData)] = "Area"


## A2.2 Build initial treeData ----------------------------------------------------------------------
# from column "tree_data_formes" in nfiplot

# are all the entries not empty?
ids_trees = NULL #8846
ids_not_trees = NULL
for(id in 1:nrow(nfiplot)){
  if(nrow(nfiplot$tree_data_formes[[id]])>0){
    ids_trees = c(ids_trees, id)
  } else {ids_not_trees = c(ids_not_trees, id)}
}
length(ids_trees)
length(ids_not_trees)


# merge all the dataframes

treedata_formes <- data.frame()
treedata_formes_ID <- c()
pb=txtProgressBar(1, nrow(nfiplot), style=3)
for(i in 1:nrow(nfiplot)){
  setTxtProgressBar(pb, i)
  treedata_formes <- rbind(treedata_formes, nfiplot$tree_data_formes[[i]])
  treedata_formes_ID <- c(treedata_formes_ID, rep(sub("^0+", "", nfiplot$IDPARCELA[i]), nrow(nfiplot$tree_data_formes[[i]])))
}

length(treedata_formes_ID)
dim(treedata_formes)
# modify according to FORMES standards
treedata_formes$Species <- sub("^0+", "", treedata_formes$IFNcode)
treedata_formes$ID <- treedata_formes_ID

treedata_formes <- treedata_formes %>% 
  select(-IFNcode) %>% 
  relocate(ID) %>% 
  mutate(Height = Height/100) %>% 
  rename(H = Height)

# save treedata 
saveRDS(treedata_formes, "RData/treedata_FORMES.rds")


# 
# # First retrieve ids of plots with treeData
# ids_trees = NULL #9731
# for(id in 1:nrow(nfiplot)){
#   forest = nfiplot$forest[id][[1]]
#   if(nrow(forest$treeData)>0){
#     ids_trees = c(ids_trees, id)
#   }
# }
# # Merge all tree data in a single data frame
# treeData = NULL
# pb = txtProgressBar(min = 0, max = length(ids_trees), style = 3, width = 50, char = "=")   
# for(id in ids_trees){
#   forest = nfiplot$forest[id][[1]]
#   trees = forest$treeData[,c("IFNcode", "Species", "DBH", "Height", "N")]
#   trees$ID = nfiplot$IDPARCELA[id]
#   if(is.null(treeData)){
#     treeData = trees
#   } else{
#     treeData = rbind(treeData, trees)
#   }  
#   setTxtProgressBar(pb, id)
# }
# # Give it the format needed by FORMES. treeData has 217.885 rows
# treeData$ID[substr(treeData$ID,1,1)=="0"] = 
#   substr(treeData$ID[substr(treeData$ID,1,1)=="0"], 2, 20)
# spp_name = group_by(treeData, IFNcode) %>% summarise(ID_TAXON=NA)
# for(i in 1:nrow(spp_name)){
#   s = strsplit(spp_name$IFNcode[i], "0")[[1]]
#   if(length(s)== 4){
#     spp_name$ID_TAXON[i] = paste0(s[1], s[2], s[3], s[4])  
#   } else if(length(s)== 3){
#     spp_name$ID_TAXON[i] = paste0(s[1], s[2], s[3])
#   } else if(length(s)== 2){
#     spp_name$ID_TAXON[i] = paste0(s[1], s[2])
#   } else{
#     spp_name$ID_TAXON[i] = s[1]
#   }
# }
# treeData = treeData %>% left_join(spp_name, by = "IFNcode")
# treeData = treeData[, c("ID", "ID_TAXON", "N", "DBH", "Height")]
# names(treeData) = c("ID", "Species", "N", "DBH", "H")
# ## save treeDAta as .rda or .rds  to be used later on and not need to re-run this piece of code


## A2.3 Build initial demands ----------------------------------------------------------------------
## Build demand for all Catalonia for the period 2001 - 2010 
# Import txt files with demands
aprofit_decade_prov_spp <- read.delim("Tables/AprofitamentsForestals_2001-2021/aprofit_decade_prov_spp.txt")
format(aprofit_decade_prov_spp$Volum, scientific = F)

# Load excel file created starting from "aprofit_decade_prov_spp.txt"
demand_2001_2020 <- read_excel("Tables/Demand_Province_Species_2001_2020.xlsx")

# transform it according to FORMES format and OCCC guidelines columns --> c("Step", "Name", "Species", "AnnualDemand")
# list of 4 elements (4 data.frame)
# Barcelona 
dem_Barcelona <- demand_2001_2020[demand_2001_2020$Provincia == "Barcelona",]
Barcelona <- data.frame(Step = ifelse(dem_Barcelona$Decade == "2001-2010", 1, 2),
                        Name = dem_Barcelona$Especie,
                        Species = dem_Barcelona$CodiIFN,
                        AnnualDemand = dem_Barcelona$Volum)


# Girona
dem_Girona <- demand_2001_2020[demand_2001_2020$Provincia == "Girona",]
Girona <- data.frame(Step = ifelse(dem_Girona$Decade == "2001-2010", 1, 2),
                        Name = dem_Girona$Especie,
                        Species = dem_Girona$CodiIFN,
                        AnnualDemand = dem_Girona$Volum)


# Lleida
dem_Lleida <- demand_2001_2020[demand_2001_2020$Provincia == "Lleida",]
Lleida <- data.frame(Step = ifelse(dem_Lleida$Decade == "2001-2010", 1, 2),
                        Name = dem_Lleida$Especie,
                        Species = dem_Lleida$CodiIFN,
                        AnnualDemand = dem_Lleida$Volum)


# Tarragona
dem_Tarragona <- demand_2001_2020[demand_2001_2020$Provincia == "Tarragona",]
Tarragona <- data.frame(Step = ifelse(dem_Tarragona$Decade == "2001-2010", 1, 2),
                     Name = dem_Tarragona$Especie,
                     Species = dem_Tarragona$CodiIFN,
                     AnnualDemand = dem_Tarragona$Volum)

# Combine the 4 dataframes into a list 
demand_2001_2020 <- list(Barcelona, Girona, Lleida, Tarragona)
names(demand_2001_2020) <- c("Barcelona", "Girona", "Lleida", "Tarragona")

# Save the demand 
saveRDS(demand_2001_2020, "RData/demand_2001_2020.rds")


## A2.4 Build climatic projections ----------------------------------------------------------------------



## A2.5 A 20-year simulation with BAU conditions ----------------------------------------------------------------------
## Retrieve default FORMES parameters
params = defaultParamsFORMES()
## Load demand
demand_2001_2020 <- readRDS("RData/demand_2001_2020.rds")
## Load treedata
treedata_FORMES <- readRDS("RData/treedata_FORMES.rds")
## Load PlotData
plotData_OCCC <- readRDS("RData/plotData_OCCC.rds")
row.names(plotData_OCCC) <- plotData_OCCC$ID
## Load PlotDataDyn
plotDataDyn_s1_2 <- readRDS("RData/plotDataDyn/plotDataDyn_s1_2.rds")
## No climate change (by now)
plotDataDyn = NULL
plotDataDyn = plotDataDyn_s1_2
# row.names(plotDataDyn_s1_2) <- plotDataDyn_s1_2$ID
## Time horizon = 10 years
params$numSteps = 2
## Use Regiones Procedencia  ¿not decided yet?
params$useRP = T
params$useProvince = T  
## Variability to TRUE
params$stochastic = T
## Do not simulate fire
params$fire = F
params$fire.table = F
burntAreaProv = NULL
## Local ingrowth
nbIFN = NULL

# Silvicultural prescriptions
scnPrescription = readxl::read_xlsx("Tables/SilviculturalPrescriptions_OCCC.xlsx", sheet = "PrescripcionsCat_BAU_v2", skip = 2)
scnPrescription$thinCommercial = FALSE
scnPrescription$thinningHB <- NA # add column thinningHB
scnPrescription$thinBAtarget <- NA


# Run the scenario
scenName = "BAU_2001-2020"
plotData_OCCC$ID = ifelse(nchar(plotData_OCCC$ID)==5, paste0("0", plotData_OCCC$ID), plotData_OCCC$ID)
plotData_OCCC$Province = ifelse(nchar(plotData_OCCC$Province)==1, paste0("0", plotData_OCCC$Province), plotData_OCCC$Province)
row.names(plotData_OCCC) = plotData_OCCC$ID
plotDataDyn_s1_2$ID = ifelse(nchar(plotDataDyn_s1_2$ID)==5, paste0("0", plotDataDyn_s1_2$ID), plotDataDyn_s1_2$ID)
treedata_FORMES$ID = ifelse(nchar(treedata_FORMES$ID)==5, paste0("0", treedata_FORMES$ID), treedata_FORMES$ID)
params$provs = c("08", "17", "25", "43")
runScenarioFORMES(scenDir = paste0("RData/Scenarios/", scenName), clim = plotDataDyn_s1_2, 
                  treeData = treedata_FORMES, params = params, plotData = plotData_OCCC,
                  customPrescription =  scnPrescription, customDemand = demand_2001_2020)

## Merge the results of the scenario
# mergeScenarioResults(scenName, fileName = "cat")


# check demandrow and prescriptionrow
barc <- readRDS("RData/Scenarios/BAU/BAU_2001-2020/8.rds")
gir <- readRDS("RData/Scenarios/BAU/BAU_2001-2020/17.rds")
llei <- readRDS("RData/Scenarios/BAU/BAU_2001-2020/25.rds")
tarr <- readRDS("RData/Scenarios/BAU/BAU_2001-2020/43.rds")


sum(is.na(barc$plotDataSuppl$DemandRow))
sum(is.na(gir$plotDataSuppl$DemandRow))
sum(is.na(llei$plotDataSuppl$DemandRow))
sum(is.na(tarr$plotDataSuppl$DemandRow))

barc$plotDataSuppl[is.na(barc$plotDataSuppl$DemandRow),]
gir$plotDataSuppl[is.na(gir$plotDataSuppl$DemandRow),]
llei$plotDataSuppl[is.na(llei$plotDataSuppl$DemandRow),]
tarr$plotDataSuppl[is.na(tarr$plotDataSuppl$DemandRow),]

sum(is.na(barc$plotDataSuppl$PrescriptionRow))
sum(is.na(gir$plotDataSuppl$PrescriptionRow))
sum(is.na(llei$plotDataSuppl$PrescriptionRow))
sum(is.na(tarr$plotDataSuppl$PrescriptionRow))

  
  
## A2.6 NOGEST, years 2001 to 2100 -----

## Retrieve default FORMES parameters
params = defaultParamsFORMES()
## set demand = to 0
params$demandMultiplier = 0
## Load treedata
treedata_FORMES <- readRDS("RData/treedata_FORMES.rds")
## Load PlotData
plotData_OCCC <- readRDS("RData/plotData_OCCC.rds")
row.names(plotData_OCCC) <- plotData_OCCC$ID

## Load PlotDataDyn
plotDataDyn_s1_2 <- readRDS("RData/plotDataDyn/plotDataDyn_s1_2.rds")

plot_data_files <- list.files("RData/plotDataDyn/")
plot_data_files <- grep("(2021_2030|2031_2040|2041_2050|2051_2060|2061_2070|2071_2080|2081_2090|2091_2100)",plot_data_files, value = T)
# load files
for (f in plot_data_files){
  assign(substr(f, 1,24),readRDS(paste0("RData/plotDataDyn/", f)))
}

## Merge all the plotDataDyn for the 10 steps...only works for this scenario
plotDataDyn_45 = rbind(plotDataDyn_s1_2,
                       plotDataDyn_45_2021_2030,
                       plotDataDyn_45_2031_2040, 
                       plotDataDyn_45_2041_2050,
                       plotDataDyn_45_2051_2060,
                       plotDataDyn_45_2061_2070,
                       plotDataDyn_45_2071_2080,
                       plotDataDyn_45_2081_2090,
                       plotDataDyn_45_2091_2100)

plotDataDyn_85 = rbind(plotDataDyn_s1_2,
                       plotDataDyn_85_2021_2030,
                       plotDataDyn_85_2031_2040, 
                       plotDataDyn_85_2041_2050,
                       plotDataDyn_85_2051_2060,
                       plotDataDyn_85_2061_2070,
                       plotDataDyn_85_2071_2080,
                       plotDataDyn_85_2081_2090,
                       plotDataDyn_85_2091_2100)

# remove plotDataDyn files 
rm(list=substr(plot_data_files, 1, 24))
rm(plotDataDyn_s1_2)

## Local ingrowth
nbIFN = NULL
# row.names(plotDataDyn_s1_2) <- plotDataDyn_s1_2$ID
## Time horizon = 10 years
params$numSteps = 10
## Use Regiones Procedencia  ¿not decided yet?
params$useRP = T
params$useProvince = T  
## Variability to TRUE
params$stochastic = T
## Do not simulate fire
params$fire = F
params$fire.table = F
burntAreaProv = NULL


# Run the scenario 45
scenName = "NOG_2001-2100_45"
runScenarioFORMES(scenDir = paste0("RData/Scenarios/", scenName), clim = plotDataDyn_45, 
                  treeData = treedata_FORMES, params = params, plotData = plotData_OCCC)

# Run the scenario 85
scenName = "NOG_2001-2100_85"
runScenarioFORMES(scenDir = paste0("RData/Scenarios/", scenName), clim = plotDataDyn_85, 
                  treeData = treedata_FORMES, params = params, plotData = plotData_OCCC)












## A2.7 BAU, years 2021 to 2100-----

# set rcp 
rcp = "85"

## Retrieve default FORMES parameters
params = defaultParamsFORMES()
prov <- params$provs
# Continue from...
params$continueFromDir = "RData/Scenarios/BAU/BAU_2001-2020/"

## Load demand.... function demand 30% del creixement
# need to specify which directory...
demand_2001_2020 <- readRDS("RData/demand_2001_2020.rds")
demand_BAU <- demand_generation(demand_default = demand_2001_2020,
                                dir = "BAU/BAU_2001-2020",
                                step = 1,
                                perc = 30)

## Load treedata
# treedata_FORMES <- readRDS("RData/treedata_FORMES.rds")
## Load PlotData BAU
plotData_OCCC <- readRDS("RData/plotData_OCCC.rds")
row.names(plotData_OCCC) <- plotData_OCCC$ID

## Load PlotDataDyn (ATTENTION to the rcp)
if(rcp == "45"){plotDataDyn <- readRDS("RData/plotDataDyn/plotDataDyn_45_2021-2030.rds")}
if(rcp == "85"){plotDataDyn <- readRDS("RData/plotDataDyn/plotDataDyn_85_2021-2030.rds")}

plotDataDyn$Step = 1
## Time horizon = 10 years
params$numSteps = 1
## Use Regiones Procedencia  ¿not decided yet?
params$useRP = T
params$useProvince = T  
## Variability to TRUE
params$stochastic = T
## Do not simulate fire
params$fire = F
params$fire.table = F
burntAreaProv = NULL
## Local ingrowth
nbIFN = NULL

# Silvicultural prescriptions
scnPrescription = readxl::read_xlsx("Tables/SilviculturalPrescriptions_OCCC.xlsx", sheet = "PrescripcionsCat_BAU_v2", skip = 2)
scnPrescription$thinCommercial = FALSE
scnPrescription$thinningHB <- NA # add column thinningHB
scnPrescription$thinBAtarget <- NA


# scenName = "BAU/BAU_45/BAU_2021-2030_45"
scenName = paste0("BAU/BAU_",rcp,"/BAU_2021-2030_", rcp)
runScenarioFORMES(scenDir = paste0("RData/Scenarios/", scenName), clim = plotDataDyn, 
                  treeData = NULL, params = params, plotData = plotData_OCCC,
                  customPrescription =  scnPrescription, customDemand = demand_BAU)

# save demand
saveRDS(demand_BAU, paste0("RData/Demand/BAU/BAU_",rcp,"/demand_2021-2030.rds" ))

##
# change these before to run next step
# previous step #
prev_steps <- c("2021-2030", "2031-2040", "2041-2050", "2051-2060", "2061-2070", "2071-2080",
                "2081-2090")
next_steps <- c("2031-2040", "2041-2050", "2051-2060", "2061-2070", "2071-2080",
                "2081-2090", "2091-2100")
# NOT RUN
for(s in 1:length(prev_steps)){
  params$continueFromDir = paste0("RData/Scenarios/BAU/BAU_",rcp,"/BAU_", prev_steps[s] ,"_", rcp)
  demand_BAU <- demand_generation(demand_default = demand_2001_2020,
                                  dir = paste0("BAU/BAU_",rcp,"/BAU_", prev_steps[s], "_", rcp),
                                  step = 1,
                                  perc = 30)

  ## Load PlotDataDyn (ATTENTION to the rcp)
  plotDataDyn <- readRDS(paste0("RData/plotDataDyn/plotDataDyn_",rcp,"_", next_steps[s] ,".rds")) #_
  plotDataDyn$Step = 1
  scenName = paste0("BAU/BAU_",rcp,"/BAU_",next_steps[s],"_", rcp)
  # --- #

  # run #
  runScenarioFORMES(scenDir = paste0("RData/Scenarios/", scenName), clim = plotDataDyn,
                    treeData = NULL, params = params, plotData = plotData_OCCC,
                    customPrescription =  scnPrescription, customDemand = demand_BAU)
  
  # save demand
  saveRDS(demand_BAU, paste0("RData/Demand/BAU/BAU_",rcp,"/demand_",next_steps[s],".rds" ))

}



# params$continueFromDir = paste0("RData/Scenarios/BAU/BAU_", prev_step ,"_85/")
# demand_BAU <- demand_generation(demand_default = demand_2001_2020,
#                                 dir = paste0("BAU/BAU_", prev_step, "_85"),
#                                 step = 1,
#                                 perc = 30)
# # --- #
# 
# # next step #
# next_steps <- c("2031-2040", "2041-2050", "2051-2060", "2061-2070", "2071-2080",
#                 "2081-2090", "2091-2100")
# # next_step = "2031-2040"
# ## Load PlotDataDyn (ATTENTION to the rcp)
# plotDataDyn <- readRDS(paste0("RData/plotDataDyn/plotDataDyn_85_", next_step ,".rds")) #_
# plotDataDyn$Step = 1
# scenName = paste0("BAU/BAU_",next_step,"_85") #-
# # --- #
# 
# # run #
# runScenarioFORMES(scenDir = paste0("RData/Scenarios/", scenName), clim = plotDataDyn, 
#                   treeData = NULL, params = params, plotData = plotData_OCCC,
#                   customPrescription =  scnPrescription, customDemand = demand_BAU)
# 


## A2.8 RSB, years 2021 to 2100-----

rcp = "85"
mgm = "RSB"

## Retrieve default FORMES parameters
params = defaultParamsFORMES()
prov <- params$provs
# Continue from...
params$continueFromDir = "RData/Scenarios/BAU/BAU_2001-2020/" # only second step

## Load demand.... function demand 30% del creixement
# need to specify which directory...
demand_2001_2020 <- readRDS("RData/demand_2001_2020.rds")
demand <- demand_generation(demand_default = demand_2001_2020,
                            dir = "BAU/BAU_2001-2020", # previous step directory
                            step = 1,
                            perc = 30)

## Load treedata
# treedata_FORMES <- readRDS("RData/treedata_FORMES.rds")
## Load PlotData RSB
plotData <- readRDS(paste0("RData/plotData_",mgm, ".rds"))
row.names(plotData) <- plotData$ID

## Load PlotDataDyn (ATTENTION to the rcp)
if(rcp == "45"){plotDataDyn <- readRDS("RData/plotDataDyn/plotDataDyn_45_2021-2030.rds")}
if(rcp == "85"){plotDataDyn <- readRDS("RData/plotDataDyn/plotDataDyn_85_2021-2030.rds")}
plotDataDyn$Step = 1
## Time horizon = 10 years
params$numSteps = 1
## Use Regiones Procedencia  ¿not decided yet?
params$useRP = T
params$useProvince = T  
## Variability to TRUE
params$stochastic = T
## Do not simulate fire
params$fire = F
params$fire.table = F
burntAreaProv = NULL
## Local ingrowth
nbIFN = NULL

# Silvicultural prescriptions
scnPrescription = readxl::read_xlsx("Tables/SilviculturalPrescriptions_OCCC.xlsx", sheet = "PrescripcionsCat_BAU_v2", skip = 2)
scnPrescription$thinCommercial = FALSE
scnPrescription$thinningHB <- NA # add column thinningHB
scnPrescription$thinBAtarget <- NA

# retrieve plots to exclude from nfiplot object
# View(head(nfiplot))
nfiplot$IDPARCELA <- sub("^0+", "", nfiplot$IDPARCELA)
exclude <- st_drop_geometry(nfiplot[,c("IDPARCELA", "prior_agri", "prior_pasture")])
exclude_1 <- exclude[(exclude$prior_agri>0 & exclude$prior_agri< 11) | (exclude$prior_pasture>0 & exclude$prior_pasture< 11),]$IDPARCELA
exclude_2 <- exclude[exclude$prior_agri>10 | exclude$prior_pasture>10,]$IDPARCELA

## run step 2
# 1 filter RSB (2021-2030)
# exclude plots in plotData and plotdatasuppl
plotData <- plotData[!plotData$ID %in%  exclude_1,]

setwd("C:/Users/giuseppe.capizzi/OneDrive - ctfc.cat/OCCC_ScnForestals/RData/Scenarios/RSB/BAU_RSB/for_2021-2030")
# provincias <- c("Barcelona", "Girona", "Lleida", "Tarragona")
for(p in 1:length(prov)){
  # assign(paste0(provincias[p]), readRDS(paste0(prov[p],".rds")))
  scn <- readRDS(paste0(prov[p],".rds"))
  scn$plotDataSuppl <- scn$plotDataSuppl[!row.names(scn$plotDataSuppl) %in% exclude_1,]
  saveRDS(scn, paste0(prov[p], ".rds"))
}

# run 
scenName = paste0(mgm, "/",mgm,"_",rcp,"/",mgm,"_2021-2030_",rcp)
runScenarioFORMES(scenDir = paste0("RData/Scenarios/", scenName), clim = plotDataDyn, 
                  treeData = NULL, params = params, plotData = plotData,
                  customPrescription =  scnPrescription, customDemand = demand)

provincias <- c("Barcelona", "Girona", "Lleida", "Tarragona")
# check that exclude_1 are not in the results.........
for(p in 1:length(prov)){
  assign(paste0(provincias[p]), readRDS(paste0("RData/Scenarios/",mgm, "/",mgm,"_",rcp,"/",mgm,"_2021-2030_",rcp,"/", prov[p],".rds")))
} 

sum(exclude_1 %in% Barcelona$treeDataSequence$ID) # 0, not in data sequence
sum(exclude_1 %in% Girona$treeDataSequence$ID) # 0, not in data sequence
sum(exclude_1 %in% Lleida$treeDataSequence$ID) # 0, not in data sequence
sum(exclude_1 %in% Tarragona$treeDataSequence$ID) # 0, not in data sequence

## run step 3
# 2 filter RSB (2031-2040)
plotData <- plotData[!plotData$ID %in%  exclude_1,]
plotData <- plotData[!plotData$ID %in%  exclude_2,]


setwd("C:/Users/giuseppe.capizzi/OneDrive - ctfc.cat/OCCC_ScnForestals/RData/Scenarios/RSB/BAU_RSB/for_2031-2040")
# provincias <- c("Barcelona", "Girona", "Lleida", "Tarragona")
for(p in 1:length(prov)){
  # assign(paste0(provincias[p]), readRDS(paste0(prov[p],".rds")))
  scn <- readRDS(paste0(prov[p],".rds"))
  scn$plotDataSuppl <- scn$plotDataSuppl[!row.names(scn$plotDataSuppl) %in% exclude_1,]
  scn$plotDataSuppl <- scn$plotDataSuppl[!row.names(scn$plotDataSuppl) %in% exclude_2,]
  saveRDS(scn, paste0(prov[p], ".rds"))
}

saveRDS(demand, paste0("RData/Demand/",mgm,"/",mgm,"_",rcp,"/demand_2021-2030.rds"))

### --- ###
# run other steps
prev_steps <- c("2021-2030", "2031-2040", "2041-2050", "2051-2060", "2061-2070", "2071-2080",
                "2081-2090")
next_steps <- c("2031-2040", "2041-2050", "2051-2060", "2061-2070", "2071-2080",
                "2081-2090", "2091-2100")
run_scenarios(rcp = rcp, mgm = mgm, plotData = plotData)




#--------------# other steps #-------------#

# run other steps
prev_step = "2081-2090"
params$continueFromDir = paste0("RData/Scenarios/RSB/RSB_85/RSB_", prev_step ,"_85/")
demand_RSB <- demand_generation(demand_default = demand_2001_2020,
                                dir = paste0("RSB/RSB_85/RSB_", prev_step, "_85"),
                                step = 1,
                                perc = 30)
# --- #

next_step = "2091-2100"
## Load PlotDataDyn (ATTENTION to the rcp)
plotDataDyn <- readRDS(paste0("RData/plotDataDyn/plotDataDyn_85_", next_step ,".rds")) #_
plotDataDyn$Step = 1
scenName = paste0("RSB/RSB_85/RSB_",next_step,"_85") #-
# --- #

# run #
runScenarioFORMES(scenDir = paste0("RData/Scenarios/", scenName), clim = plotDataDyn, 
                  treeData = NULL, params = params, plotData = plotData_RSB,
                  customPrescription =  scnPrescription, customDemand = demand_RSB)



# check that exclude_1 and exclude_2 are not in the results.........
for(p in 1:length(prov)){
  assign(paste0(provincias[p]), readRDS(paste0("RData/Scenarios/RSB/RSB_85/RSB_2041-2050_85/", prov[p],".rds")))
} 

sum(exclude_1 %in% Barcelona$treeDataSequence$ID) # 0, not in data sequence
sum(exclude_1 %in% Girona$treeDataSequence$ID) # 0, not in data sequence
sum(exclude_1 %in% Lleida$treeDataSequence$ID) # 0, not in data sequence
sum(exclude_1 %in% Tarragona$treeDataSequence$ID) # 0, not in data sequence

sum(exclude_2 %in% Barcelona$treeDataSequence$ID) # 0, not in data sequence
sum(exclude_2 %in% Girona$treeDataSequence$ID) # 0, not in data sequence
sum(exclude_2 %in% Lleida$treeDataSequence$ID) # 0, not in data sequence
sum(exclude_2 %in% Tarragona$treeDataSequence$ID) # 0, not in data sequence





## A2.9 ASEA, years 2021 to 2100-----

rcp = "85"
mgm = "ASEA"

## Retrieve default FORMES parameters
params = defaultParamsFORMES()
prov <- params$provs
# Continue from...
params$continueFromDir = "RData/Scenarios/BAU/BAU_2001-2020/" # only second step

## Load demand.... function demand 30% del creixement
# need to specify which directory...
demand_2001_2020 <- readRDS("RData/demand_2001_2020.rds")
demand_ASEA <- demand_generation(demand_default = demand_2001_2020,
                                dir = "BAU/BAU_2001-2020", # previous step directory
                                step = 1,
                                perc = 30)

## Load treedata
# treedata_FORMES <- readRDS("RData/treedata_FORMES.rds")
## Load PlotData AMF
plotData <- readRDS(paste0("RData/plotData_",mgm, ".rds"))
row.names(plotData) <- plotData$ID

## Load PlotDataDyn (ATTENTION to the rcp)
if(rcp == "45"){plotDataDyn <- readRDS("RData/plotDataDyn/plotDataDyn_45_2021-2030.rds")}
if(rcp == "85"){plotDataDyn <- readRDS("RData/plotDataDyn/plotDataDyn_85_2021-2030.rds")}
plotDataDyn$Step = 1
## Time horizon = 10 years
params$numSteps = 1
## Use Regiones Procedencia  ¿not decided yet?
params$useRP = T
params$useProvince = T  
## Variability to TRUE
params$stochastic = T
## Do not simulate fire
params$fire = F
params$fire.table = F
burntAreaProv = NULL
## Local ingrowth
nbIFN = NULL

# Silvicultural prescriptions
scnPrescription = readxl::read_xlsx("Tables/SilviculturalPrescriptions_OCCC.xlsx", sheet = "PrescripcionsCat_BAU_v2", skip = 2)
scnPrescription$thinCommercial = FALSE
scnPrescription$thinningHB <- NA # add column thinningHB
scnPrescription$thinBAtarget <- NA

# run step 2
scenName = paste0(mgm, "/",mgm,"_",rcp,"/",mgm,"_2021-2030_",rcp)
runScenarioFORMES(scenDir = paste0("RData/Scenarios/", scenName), clim = plotDataDyn, 
                  treeData = NULL, params = params, plotData = plotData,
                  customPrescription =  scnPrescription, customDemand = demand_ASEA)

# save demand ATTENZIONE AL MGM
saveRDS(demand_ASEA, paste0("RData/Demand/",mgm,"/",mgm,"_",rcp,"/demand_2021-2030.rds" ))

### --- ###
# run other steps
prev_steps <- c("2021-2030", "2031-2040", "2041-2050", "2051-2060", "2061-2070", "2071-2080",
                "2081-2090")
next_steps <- c("2031-2040", "2041-2050", "2051-2060", "2061-2070", "2071-2080",
                "2081-2090", "2091-2100")
run_scenarios(rcp = rcp, mgm = "ASEA", plotData = plotData)


# --- #

## A2.10 AMF, years 2021 to 2100-----

rcp = "85"
mgm = "AMF"

## Retrieve default FORMES parameters
params = defaultParamsFORMES()
prov <- params$provs
# Continue from...
params$continueFromDir = "RData/Scenarios/BAU/BAU_2001-2020/" # only second step

## Load demand.... function demand 30% del creixement
# need to specify which directory...
demand_2001_2020 <- readRDS("RData/demand_2001_2020.rds")
demand <- demand_generation(demand_default = demand_2001_2020,
                                 dir = "BAU/BAU_2001-2020", # previous step directory
                                 step = 1,
                                 perc = 40)

## Load treedata
# treedata_FORMES <- readRDS("RData/treedata_FORMES.rds")
## Load PlotData AMF
plotData <- readRDS(paste0("RData/plotData_",mgm, ".rds"))
row.names(plotData) <- plotData$ID

## Load PlotDataDyn (ATTENTION to the rcp)
if(rcp == "45"){plotDataDyn <- readRDS("RData/plotDataDyn/plotDataDyn_45_2021-2030.rds")}
if(rcp == "85"){plotDataDyn <- readRDS("RData/plotDataDyn/plotDataDyn_85_2021-2030.rds")}
plotDataDyn$Step = 1
## Time horizon = 10 years
params$numSteps = 1
## Use Regiones Procedencia  ¿not decided yet?
params$useRP = T
params$useProvince = T  
## Variability to TRUE
params$stochastic = T
## Do not simulate fire
params$fire = F
params$fire.table = F
burntAreaProv = NULL
## Local ingrowth
nbIFN = NULL

# Silvicultural prescriptions
scnPrescription = readxl::read_xlsx("Tables/SilviculturalPrescriptions_OCCC.xlsx", sheet = "PrescripcionsCat_BAU_v2", skip = 2)
scnPrescription$thinCommercial = FALSE
scnPrescription$thinningHB <- NA # add column thinningHB
scnPrescription$thinBAtarget <- NA

# run step 2
scenName = paste0(mgm, "/",mgm,"_",rcp,"/",mgm,"_2021-2030_",rcp)
runScenarioFORMES(scenDir = paste0("RData/Scenarios/", scenName), clim = plotDataDyn, 
                  treeData = NULL, params = params, plotData = plotData,
                  customPrescription =  scnPrescription, customDemand = demand)

# save demand ATTENZIONE AL MGM
saveRDS(demand, paste0("RData/Demand/",mgm,"/",mgm,"_",rcp,"/demand_2021-2030.rds" ))

### --- ###
# run other steps
prev_steps <- c("2021-2030", "2031-2040", "2041-2050", "2051-2060", "2061-2070", "2071-2080",
                "2081-2090")
next_steps <- c("2031-2040", "2041-2050", "2051-2060", "2061-2070", "2071-2080",
                "2081-2090", "2091-2100")
run_scenarios(rcp = rcp, mgm = mgm, plotData = plotData, perc = 70)


# --- #


## A2.11 ACG, years 2021 to 2100-----

rcp = "45"
mgm = "ACG"

## Retrieve default FORMES parameters
params = defaultParamsFORMES()
prov <- params$provs
# Continue from...
params$continueFromDir = "RData/Scenarios/BAU/BAU_2001-2020/" # only second step

## Load demand....in case of ACG the demand is infinite
demand_ACG <- readRDS("RData/demand_2001_2020.rds")
# select 1 step
demand_ACG$Barcelona <- demand_ACG$Barcelona[demand_ACG$Barcelona$Step == 1,]
demand_ACG$Lleida <- demand_ACG$Lleida[demand_ACG$Lleida$Step == 1,]
demand_ACG$Girona <- demand_ACG$Girona[demand_ACG$Girona$Step == 1,]
demand_ACG$Tarragona <- demand_ACG$Tarragona[demand_ACG$Tarragona$Step == 1,]
# Increase the demand
demand_ACG$Barcelona$AnnualDemand <- 10^6
demand_ACG$Lleida$AnnualDemand <- 10^6
demand_ACG$Girona$AnnualDemand <- 10^6
demand_ACG$Tarragona$AnnualDemand <- 10^6

## Load treedata
# treedata_FORMES <- readRDS("RData/treedata_FORMES.rds") # only for BAU
## Load PlotData AMF
plotData_ACG <- readRDS("RData/plotData_ACG.rds")
row.names(plotData_ACG) <- plotData_ACG$ID

## Load PlotDataDyn (ATTENTION to the rcp)
if(rcp == "45"){plotDataDyn <- readRDS("RData/plotDataDyn/plotDataDyn_45_2021-2030.rds")}
if(rcp == "85"){plotDataDyn <- readRDS("RData/plotDataDyn/plotDataDyn_85_2021-2030.rds")}
plotDataDyn$Step = 1
## Time horizon = 10 years
params$numSteps = 1
## Use Regiones Procedencia  ¿not decided yet?
params$useRP = T
params$useProvince = T  
## Variability to TRUE
params$stochastic = T
## Do not simulate fire
params$fire = F
params$fire.table = F
burntAreaProv = NULL
## Local ingrowth
nbIFN = NULL

# Silvicultural prescriptions (DIFFERENT FOR ACG)
scnPrescription_ACG = readxl::read_xlsx("Tables/SilviculturalPrescriptions_OCCC.xlsx", sheet = "PrescripcionsCat_ACG_v2", skip = 2)
scnPrescription_ACG$thinCommercial = FALSE
scnPrescription_ACG$thinningHB <- NA # add column thinningHB
scnPrescription_ACG$thinBAtarget <- NA



# plotData_ACG <- plotData_ACG[!plotData_ACG$ID %in% c("83376","83377"),]

# debug
setwd("C:/Users/giuseppe.capizzi/OneDrive - ctfc.cat/FORMES/FORMES")
devtools::load_all()

# run step 2
scenName = "ACG/ACG_45/ACG_2021-2030_45"
runScenarioFORMES(scenDir = paste0("RData/Scenarios/", scenName), clim = plotDataDyn, 
                  treeData = NULL, params = params, plotData = plotData_ACG,
                  customPrescription =  scnPrescription_ACG, customDemand = demand_ACG)

# save demand ATTENZIONE AL MGM
saveRDS(demand_ACG, paste0("RData/Demand/",mgm,"/",mgm,"_",rcp,"/demand_2021-2030.rds" ))


# run other steps
prev_step = "2081-2090"
params$continueFromDir = paste0("RData/Scenarios/ACG/ACG_85/ACG_", prev_step ,"_85/")

# --- #

next_step = "2091-2100"
## Load PlotDataDyn (ATTENTION to the rcp)
plotDataDyn <- readRDS(paste0("RData/plotDataDyn/plotDataDyn_85_", next_step ,".rds")) #_
plotDataDyn$Step = 1
scenName = paste0("ACG/ACG_85/ACG_",next_step,"_85") #-
# --- #

# run #
runScenarioFORMES(scenDir = paste0("RData/Scenarios/", scenName), clim = plotDataDyn, 
                  treeData = NULL, params = params, plotData = plotData_ACG,
                  customPrescription =  scnPrescription_ACG, customDemand = demand_ACG)


saveRDS(demand_ACG, paste0("RData/Demand/",mgm,"/",mgm,"_",rcp,"/demand_",next_step,".rds" ))







