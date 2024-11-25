## script from FORMESdata/Rscript/
## modify according to the soil_parameters present in the interpolated_data, column "soil"
## example nfi_1976$soil


## load packages
rm(list=ls())
library(sf)
library(sp)
library(medfate)
library(tidyverse)



# load interpolated data from FORMESdata directory
setwd("C:/Users/giuseppe.capizzi/OneDrive - ctfc.cat/FORMESdata/Rdata/Interpolated_climate_historic")

nfi_files <- list.files()
nfi_files <- grep("\\.rds$", nfi_files, value = T)

for (f in nfi_files[1:2]){
  cat(paste0("Reading ", f , "\n"))
  assign(substr(f, 1, 8), readRDS(f))
}


nfi_1976$soil[[1]]$clay
dim(nfi_1976)[1]
nfi_1976$IDPARCELA <- sub("^0+", "", nfi_1976$IDPARCELA) 
soil(nfi_1976$soil[[1]])


# soil parameters are always the same 
# for(p in 1:dim(nfi_1976)[1]){
#   if(!identical(nfi_1976$soil[[p]] , nfi_1977$soil[[p]])){
#     print("LOL")
#   }
# }


## Compute SWHC 
swhc = data.frame(L12=rep(NA, length(nfi_1976$IDPARCELA )), L13=rep(NA, length(nfi_1976$IDPARCELA )))
row.names(swhc) = nfi_1976$IDPARCELA
pb=txtProgressBar(1, length(nfi_1976$IDPARCELA ), style=3)
for(i in 1:length(nfi_1976$IDPARCELA)) {
  setTxtProgressBar(pb, i)
  # if(!is.null(soil_list_IFN3_Spain[[i]])) {
  if(!any(is.na(nfi_1976$soil[[i]]$clay))){
    s = soil(nfi_1976$soil[[i]])
    nl = length(s$dVec)
    wf = soil_waterFC(s) # Water at field capacity (mm)
    tfc = soil_thetaFC(s) # Volumetric percent moisture at field capacity (-33 kPa)
    twp = soil_thetaWP(s)
    W = twp/tfc # Moisture at wilting point as proportion of field capacity
    swhc$L12[i] = sum(wf[1:2]*(1-W[1:2])) #Difference between wilting point and field capacity
    swhc$L13[i] = sum(wf[1:3]*(1-W[1:3])) #Difference between wilting point and field capacity
  }
}



length(swhc$L12[is.na(swhc$L12)]) # 0 elements without swhc
swhc$L12[is.na(swhc$L12)] = median(swhc$L12, na.rm=T)
swhc$L13[is.na(swhc$L13)] = median(swhc$L13, na.rm=T)  # this is final SWHC

setwd("C:/Users/giuseppe.capizzi/OneDrive - ctfc.cat/OCCC_ScnForestals")
saveRDS(swhc, file = "Rdata/SWHC_OCCC.rds")










