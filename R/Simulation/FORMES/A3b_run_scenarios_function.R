# run scenario function

run_scenarios <- function(rcp,mgm, plotData, perc = 30){
  for(s in 1:length(prev_steps)){
    params$continueFromDir = paste0("Rdata/Scenarios/",mgm,"/",mgm,"_",rcp,"/",mgm,"_", prev_steps[s] ,"_", rcp)
    demand <- demand_generation(demand_default = demand_2001_2020,
                                    dir = paste0(mgm,"/",mgm,"_",rcp,"/",mgm,"_", prev_steps[s], "_", rcp),
                                    step = 1,
                                    perc = perc)
    
    ## Load PlotDataDyn
    plotDataDyn <- readRDS(paste0("Rdata/plotDataDyn/plotDataDyn_",rcp,"_", next_steps[s] ,".rds")) #_
    plotDataDyn$Step = 1
    scenName = paste0(mgm,"/",mgm,"_",rcp,"/",mgm,"_",next_steps[s],"_", rcp)
    # --- #
    
    # run #
    runScenarioFORMES(scenDir = paste0("Rdata/Scenarios/", scenName), clim = plotDataDyn,
                      treeData = NULL, params = params, plotData = plotData,
                      customPrescription =  scnPrescription, customDemand = demand)
    
    # save demand
    saveRDS(demand, paste0("Rdata/Demand/",mgm,"/",mgm,"_",rcp,"/demand_",next_steps[s],".rds" ))
    
  }
}
