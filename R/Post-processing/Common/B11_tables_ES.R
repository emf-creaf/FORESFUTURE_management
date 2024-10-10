rm(list=ls())
library(ggplot2)
library(cowplot)
library(sf)
library(tidyverse)
library(tidyterra)
library(gt)
library(grid)

# Load spatial data -------------------------------------------------------
nfiplot <- dplyr::bind_rows(readRDS(paste0("Rdata/nfiplot.rds")))
comarques <- sf::read_sf("Data/Comarques/comarques.shp")
cat <- sf::st_union(comarques)

# Load calculated ES and select common IFN plots -------------------------------------
# Load state data
ES_state_FORMES <- readRDS("Rdata/ES/ES_state_FORMES_1206.rds")
ids_formes <- unique(ES_state_FORMES$id)
n_plots_state <- length(ids_formes)
ES_state_FORDYN  <- readRDS("Rdata/ES/ES_state_MEDFATE_0209.rds")|>
  mutate(id = as.character(as.numeric(substr(id, 1,6))))|>
  filter(id %in% ids_formes)
ES_state <- dplyr::bind_rows(ES_state_FORDYN, ES_state_FORMES)
rm(ES_state_FORDYN)
rm(ES_state_FORMES)

# load period data

nfiplot$IDPARCELA <- sub("^0+", "", nfiplot$IDPARCELA)
exclude <- st_drop_geometry(nfiplot[,c("IDPARCELA", "prior_agri", "prior_pasture")])
exclude_1 <- exclude[(exclude$prior_agri>0 & exclude$prior_agri< 11) | (exclude$prior_pasture>0 & exclude$prior_pasture< 11),]$IDPARCELA
exclude_2 <- exclude[exclude$prior_agri>10 | exclude$prior_pasture>10,]$IDPARCELA

ES_period_new <- readRDS("Rdata/ES_Miquel/ES_period_0209.rds") # ALREADY CORRECTED AND COMPLETE OF THE TWO MODELS
# First filter (decade 2021-2030)
es2 <- c("ES2_AdultTreeBiomassChange", "ES2_AdultTreeBiomassSequestr", "ES2_SequestrPlusFirewood")
ES_period_new[which((!ES_period_new$Period %in% c("2001-2010", "2001-2020", "2011-2020")) &
                      (ES_period_new$Management == "RSB") &
                      (ES_period_new$id %in% exclude_1)),es2] <- NA

# 
# # First filter (decade 2031-2040)
ES_period_new[which((!ES_period_new$Period %in% c("2001-2010", "2001-2020", "2011-2020","2021-2030")) &
                      (ES_period_new$Management == "RSB") &
                      (ES_period_new$id %in% exclude_2)),es2] <- NA




# Function to generate pdf tables 
pdf_tables_generation <- function(data,
                                  type,
                                  var,
                                  title_es,
                                  outliers = c(-Inf, Inf), dir_output = "Tables/Tables_ES/"){
  # browser()
  get_viewport <- function(index) {
    # Define a 2x2 grid layout
    row <- ceiling(index / 2)
    col <- index %% 2
    if (col == 0) col <- 2  # Fix modulo results to be within grid
    viewport(layout.pos.row = row, layout.pos.col = col)
  }
  
  pdf(paste0(dir_output, deparse(substitute(var)), ".pdf"),width = 11, height = 8.5) 
  
  i = 0
  for(model in unique(data$Model)){
    for(clim in unique(data$Climate)){
      message(paste0("Generating table for ES: ",deparse(substitute(var))," - MODEL: ", model," - CLIMATE: ",clim ))
      if(type == "state"){
        ES_sum <- data |> st_drop_geometry() |> 
          filter(!(Management %in% c("NOG", "NOGEST")),
                 {{var}} > outliers[1],
                 {{var}} < outliers[2]) |>
          mutate(Period = case_when((Year>=2000 & Year<=2020) ~ "2001-2020",
                                    (Year>=2031 & Year<=2050) ~ "2031-2050",
                                    (Year>=2081 & Year<=2100) ~ "2081-2100")) |> 
          group_by(Climate, Management, Model, Period) |>
          summarise(ES = mean({{var}}, na.rm=TRUE), 
                    ES_se = sd({{var}}, na.rm=TRUE)/sqrt(sum(!is.na({{var}}))),
                    ES_q25 = quantile({{var}}, probs=0.25, na.rm = TRUE),
                    ES_q75 = quantile({{var}}, probs=0.75, na.rm = TRUE),
                    .groups = "drop") |> na.omit() |> 
          filter(Climate == clim, Model == model) |> 
          select(Management,Period, ES) |> 
          pivot_wider(names_from = Period, values_from = ES)
        
      } else if(type == "period"){
        
        ES_sum <- data |> st_drop_geometry() |> 
          filter(Period %in% c("2001-2020", "2031-2050", "2081-2100"),
                 !(Management %in% c("NOG", "NOGEST")),
                 {{var}} > outliers[1],
                 {{var}} < outliers[2]) |>
          group_by(Climate, Management, Period, MidYear, Model) |>
          summarise(ES = mean({{var}}, na.rm=TRUE), 
                    ES_se = sd({{var}}, na.rm=TRUE)/sqrt(sum(!is.na({{var}}))),
                    ES_q25 = quantile({{var}}, probs=0.25, na.rm = TRUE),
                    ES_q75 = quantile({{var}}, probs=0.75, na.rm = TRUE),
                    .groups = "drop") |> na.omit() |> 
          filter(Climate == clim, Model == model) |> 
          select(Management,Period, ES) |> 
          pivot_wider(names_from = Period, values_from = ES)
      }
      
      
      
      
      table_plot <- gridExtra::tableGrob(ES_sum, rows = NULL)

      if (i %% 4 == 0) {
        grid.newpage()
        
        # General title for the entire PDF, on every new page
        pushViewport(viewport(layout = grid.layout(3, 1, heights = unit(c(0.1, 0.05, 0.85), "npc"))))
        pushViewport(viewport(layout.pos.row = 1))  # Top 10% of the page for title
        grid.text(paste0(deparse(substitute(var)), " - ", title_es), y = unit(0.5, "npc"), gp = gpar(fontsize = 18, fontface = "bold"))
        upViewport()
        
        # Set the viewport for the table layout, taking up the remaining space
        pushViewport(viewport(layout.pos.row = 3))
        pushViewport(viewport(layout = grid.layout(2, 2)))  # 2x2 grid for 4 tables
      }
      
      vp <- get_viewport((i %% 4) + 1)
      pushViewport(vp)
      grid.text(paste0("Model: ", model, " | Climate: ", clim), y = unit(0.9, "npc"), gp = gpar(fontsize = 12, fontface = "bold"))
      grid.draw(table_plot)
      
      # Go back to the previous viewport (so we don't overwrite positioning)
      upViewport()
      
      # Increment table counter
      i <- i + 1
    }
  }
  dev.off()
}

# PERIOD ES -----
pdf_tables_generation(data = ES_period_new, type = "period",
                      var = ES1_CutStructure,
                      title_es = "Provisió de fusta\nestructural (m3/ha/any)",
                      outliers = c(-1,25))

pdf_tables_generation(data = ES_period_new, type = "period",
                      var = ES1_CutAdultFirewood, 
                      title_es = "Provisió de\nllenyes (m3/ha/any)", 
                      outliers = c(-2,20))

pdf_tables_generation(data = ES_period_new, type = "period",
                      var = ES2_AdultTreeBiomassChange, 
                      title_es = "Embornal de carboni\narbres (Mg C/ha/any)", 
                      outliers = c(-10,10))

pdf_tables_generation(data = ES_period_new, type = "period",
                      var = ES2_CutBiomassStructure, 
                      title_es = "Embornal de carboni\nfusta estructural (Mg C/ha/any)", 
                      outlier = c(-1,50))

pdf_tables_generation(data = ES_period_new, type = "period",
                      var = ES2_SequestrPlusFirewood, 
                      title_es = "Embornal de carboni\narbres + fusta + llenya (Mg C/ha/any)", 
                      outlier = c(-1,50))

pdf_tables_generation(data = ES_period_new, type = "period",
                      var = ES2_CutBiomassAdultFirewood, 
                      title_es = "Biomassa llenya (Mg C/ha/any)", 
                      outlier = c(-1,50))

pdf_tables_generation(data = ES_period_new, type = "period",
                      var = ES2_AdultTreeBiomassSequestr, 
                      title_es = "Embornal de carboni\narbres+fusta (Mg C/ha/any)", 
                      outlier = c(-50,50))

pdf_tables_generation(data = ES_period_new, type = "period",
                      var = ES2_LiveBiomassSequestr, 
                      title_es = "Embornal de carboni\ntotal (Mg C/ha/any)", 
                      outliers = c(-50,50))

pdf_tables_generation(data = ES_period_new, type = "period",
                      var = ES3_LAI, 
                      title_es = "Índex d'àrea foliar")

pdf_tables_generation(data = ES_period_new, type = "period",
                      var = ES3_Precipitation, 
                      title_es = "Precipitacio (mm/any)")

pdf_tables_generation(data = ES_period_new, type = "period",
                      var = ES3_BlueWater, 
                      title_es = "Aigua blava (mm/any)")

pdf_tables_generation(data = ES_period_new, type = "period",
                      var = ES3_RunoffCoefficient, 
                      title_es = "Coeficient d'escolament [%]")

pdf_tables_generation(data = ES_period_new, type = "period",
                      var = ES4_ErosionMitigation, 
                      title_es = "Mitigació de l'erosió (Mg/ha/any)", 
                      outliers = c(-1,3000))

pdf_tables_generation(data = ES_period_new, type = "period",
                      var = ES6_SurfaceFirePotential, 
                      title_es = "Risk d'incendi\nde superficie [0-9]")

pdf_tables_generation(data = ES_period_new, type = "period",
                      var = ES6_CrownFirePotential, 
                      title_es = "Risk d'incendi\nde capçada [0-9]")



# STATE ES -----
pdf_tables_generation(data = ES_state, type = "state",
                      var = ES1_VolumeStructure, 
                      title_es = "Stock fusta estructural (m3/ha)")
pdf_tables_generation(data = ES_state, type = "state",
                      ES1_VolumeAdultFirewood, "Stock llenyes (m3/ha)")
pdf_tables_generation(data = ES_state, type = "state",
                      ES2_AdultTreeBiomass, "Stock de carboni\narbres (Mg C/ha)",
                      outliers = c(-1, 1000))
pdf_tables_generation(data = ES_state, type = "state",
                      ES5_RecreationalValue, "Valor recreatiu [0-1]",
                      outliers = c(-1, 1000))





















