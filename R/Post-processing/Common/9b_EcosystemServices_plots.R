rm(list=ls())
library(ggplot2)
library(cowplot)
library(sf)
library(tidyverse)
library(tidyterra)
library(gt)


# Load spatial data -------------------------------------------------------
nfiplot <- dplyr::bind_rows(readRDS(paste0("Rdata/nfiplot.rds")))
comarques <- sf::read_sf("Data/Comarques/comarques.shp")
cat <- sf::st_union(comarques)

# Load calculated ES and select common IFN plots -------------------------------------
ES_state_FORMES <- readRDS("Rdata/ES/ES_state_FORMES_1206.rds")
ids_formes <- unique(ES_state_FORMES$id)
n_plots_state <- length(ids_formes)
ES_state_FORDYN  <- readRDS("Rdata/ES/ES_state_MEDFATE_0209.rds")|>
  mutate(id = as.character(as.numeric(substr(id, 1,6))))|>
  filter(id %in% ids_formes)
ES_state <- dplyr::bind_rows(ES_state_FORDYN, ES_state_FORMES)
rm(ES_state_FORDYN)
rm(ES_state_FORMES)


# ES_period_FORMES <- readRDS("Rdata/ES/ES_period_FORMES.rds")
# ES_period_FORMES <- readRDS("Rdata/ES/ES_period_FORMES_1607.rds")
# ES_period_FORMES <- readRDS("Rdata/ES/ES_period_FORMES_1206.rds")
# 
# ids_formes <- unique(ES_period_FORMES$id)
# n_plots_period <- length(ids_formes)
# # ES_period_FORDYN  <- readRDS("Rdata/ES_Miquel/ES_period_FORDYN.rds")|>
# #   mutate(id = as.character(as.numeric(substr(id, 1,6)))) |>
# #   filter(id %in% ids_formes)
# 
# # We put to NA the values of the land use change plots for scenario RSB (for the decades from 2021-2040)
# ES_period_FORDYN  <- readRDS("Rdata/ES/ES_period_MEDFATE_1607.rds")|>
#   mutate(id = as.character(as.numeric(substr(id, 1,6)))) |>
#   filter(id %in% ids_formes)
# ES_period_FORDYN$Model="FORDYN"
# 
# 
nfiplot$IDPARCELA <- sub("^0+", "", nfiplot$IDPARCELA)
exclude <- st_drop_geometry(nfiplot[,c("IDPARCELA", "prior_agri", "prior_pasture")])
exclude_1 <- exclude[(exclude$prior_agri>0 & exclude$prior_agri< 11) | (exclude$prior_pasture>0 & exclude$prior_pasture< 11),]$IDPARCELA
exclude_2 <- exclude[exclude$prior_agri>10 | exclude$prior_pasture>10,]$IDPARCELA
# 
# # table(ES_period_FORDYN$Period)
# 
# # First filter (decade 2021-2030)
# es2 <- c("ES2_AdultTreeBiomassChange", "ES2_AdultTreeBiomassSequestr", "ES2_SequestrPlusFirewood")
# ES_period_FORDYN[which((!ES_period_FORDYN$Period %in% c("2001-2010", "2001-2020", "2011-2020")) &
#         (ES_period_FORDYN$Management == "RSB") &
#         (ES_period_FORDYN$id %in% exclude_1)),es2] <- NA
# 
# # First filter (decade 2031-2040)
# ES_period_FORDYN[which((!ES_period_FORDYN$Period %in% c("2001-2010", "2001-2020", "2011-2020","2021-2030")) &
#                          (ES_period_FORDYN$Management == "RSB") &
#                          (ES_period_FORDYN$id %in% exclude_2)),es2] <- NA
# 
# 
# ES_period <- dplyr::bind_rows(ES_period_FORDYN, ES_period_FORMES)
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
# 

# rm(ES_period_FORDYN)
# rm(ES_period_FORMES)
# gc()

# Functions to generate plots --------------------------------------------------------
plot_ES_state <- function(ES_state, var, ylab, ylim, outliers = c(-Inf,Inf), add_formes = FALSE) {
  ES_sum <- ES_state |>
    filter(!(Management %in% c("NOG", "NOGEST")),
           {{var}} > outliers[1],
           {{var}} < outliers[2]) |>
    group_by(Climate, Management, Year, Model) |>
    summarise(ES = mean({{var}}, na.rm=TRUE), 
              ES_se = sd({{var}}, na.rm=TRUE)/sqrt(sum(!is.na({{var}}))),
              ES_q25 = quantile({{var}}, probs=0.25, na.rm = TRUE),
              ES_q75 = quantile({{var}}, probs=0.75, na.rm = TRUE),
              .groups = "drop")
  p1<-ggplot(ES_sum[ES_sum$Climate=="RCP45" & ES_sum$Model =="FORDYN",])+
    geom_point(aes(x = Year, y = ES, col = Management))+
    geom_ribbon(aes(x = Year, ymin = ES - ES_se*1.96, ymax = ES + ES_se*1.96, fill = Management), alpha = 0.3)+
    geom_line(aes(x = Year, y = ES, col = Management))+
    geom_vline(xintercept = 2020, linetype="dashed")+
    annotate("rect", xmin = 1995, xmax = 2020, ymin = -Inf, ymax = Inf, alpha = 0.3)+
    scale_x_continuous("",limits = c(1995,2105), expand = c(0,0), n.breaks = 10)+
    scale_fill_brewer("Escenaris de\ngestió", palette = "Set1")+
    scale_color_brewer("Escenaris de\ngestió", palette = "Set1")+
    ylab(ylab)+ ylim(ylim)+labs(title = "FORDYN", subtitle = "RCP 4.5")+theme_bw()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
          axis.title.y = element_text(size = 15),
          plot.title = element_text(size=22),
          axis.text.y = element_text(size=12),
          plot.subtitle=element_text(size=18),
          legend.text = element_text(size=12),
          legend.title = element_text(size=15),
          legend.key.size = unit(0.6, 'cm'))
  l <- get_legend(p1)
  p2<-ggplot(ES_sum[ES_sum$Climate=="RCP85" & ES_sum$Model =="FORDYN",])+
    geom_point(aes(x = Year, y = ES, col = Management))+
    geom_ribbon(aes(x = Year, ymin = ES - ES_se*1.96, ymax = ES + ES_se*1.96, fill = Management), alpha = 0.3)+
    geom_line(aes(x = Year, y = ES, col = Management))+
    geom_vline(xintercept = 2020, linetype="dashed")+
    annotate("rect", xmin = 1995, xmax = 2020, ymin = -Inf, ymax = Inf, alpha = 0.3)+
    scale_x_continuous("",limits = c(1995,2105), expand = c(0,0), n.breaks = 10)+
    scale_fill_brewer(palette = "Set1")+
    scale_color_brewer(palette = "Set1")+
    ylab("")+ ylim(ylim)+labs(title = "", subtitle = "RCP 8.5")+ theme_bw()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
          axis.title.y = element_text(size = 15),
          plot.title = element_text(size=22),
          axis.text.y = element_text(size=12),
          plot.subtitle=element_text(size=18),
          legend.text = element_text(size=12),
          legend.title = element_text(size=15),
          legend.key.size = unit(0.6, 'cm'))
  pA <- plot_grid(p1 + theme(legend.position = "none"), 
                  p2+ theme(legend.position = "none"), nrow = 1)
  if(add_formes) {
    p3<-ggplot(ES_sum[ES_sum$Climate=="RCP45" & ES_sum$Model =="FORMES",])+
      geom_point(aes(x = Year, y = ES, col = Management))+
      geom_ribbon(aes(x = Year, ymin = ES - ES_se*1.96, ymax = ES + ES_se*1.96, fill = Management), alpha = 0.3)+
      geom_line(aes(x = Year, y = ES, col = Management))+
      geom_vline(xintercept = 2020, linetype="dashed")+
      annotate("rect", xmin = 1995, xmax = 2020, ymin = -Inf, ymax = Inf, alpha = 0.3)+
      scale_x_continuous("",limits = c(1995,2105), expand = c(0,0), n.breaks = 10)+
      scale_fill_brewer(palette = "Set1")+
      scale_color_brewer(palette = "Set1")+
      ylab(ylab)+ ylim(ylim)+labs(title = "FORMES", subtitle = "RCP 4.5")+theme_bw()+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
            axis.title.y = element_text(size = 15),
            plot.title = element_text(size=22),
            axis.text.y = element_text(size=12),
            plot.subtitle=element_text(size=18),
            legend.text = element_text(size=12),
            legend.title = element_text(size=15),
            legend.key.size = unit(0.6, 'cm'))
    l <- get_legend(p1)
    p4<-ggplot(ES_sum[ES_sum$Climate=="RCP85"& ES_sum$Model =="FORMES",])+
      geom_point(aes(x = Year, y = ES, col = Management))+
      geom_ribbon(aes(x = Year, ymin = ES - ES_se*1.96, ymax = ES + ES_se*1.96, fill = Management), alpha = 0.3)+
      geom_line(aes(x = Year, y = ES, col = Management))+
      geom_vline(xintercept = 2020, linetype="dashed")+
      annotate("rect", xmin = 1995, xmax = 2020, ymin = -Inf, ymax = Inf, alpha = 0.3)+
      scale_x_continuous("",limits = c(1995,2105), expand = c(0,0), n.breaks = 10)+
      scale_fill_brewer(palette = "Set1")+
      scale_color_brewer(palette = "Set1")+
      ylab("")+ ylim(ylim)+labs(title = "", subtitle = "RCP 8.5")+ theme_bw()+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
            axis.title.y = element_text(size = 15),
            plot.title = element_text(size=22),
            axis.text.y = element_text(size=12),
            plot.subtitle=element_text(size=18),
            legend.text = element_text(size=12),
            legend.title = element_text(size=15),
            legend.key.size = unit(0.6, 'cm'))
    pB <- plot_grid(p3 + theme(legend.position = "none"), 
                    p4+ theme(legend.position = "none"), nrow = 1)
    pALL <- plot_grid(pA,pB, nrow = 2)
  } else {
    pALL <- pA
  }
  return(plot_grid(pALL, l, rel_widths = c(1,0.2)))
}
plot_ES_period <- function(ES_period, var, ylab, ylim, outliers = c(-Inf,Inf), add_formes = FALSE) {
  # browser()
  ES_sum <- ES_period |>
    filter(Period %in% paste0(seq(2001,2091, by=10), "-",seq(2010,2100, by=10)),
           !(Management %in% c("NOG", "NOGEST")),
           {{var}} > outliers[1],
           {{var}} < outliers[2]) |>
    group_by(Climate, Management, Period, MidYear, Model) |>
    summarise(ES = mean({{var}}, na.rm=TRUE), 
              ES_se = sd({{var}}, na.rm=TRUE)/sqrt(sum(!is.na({{var}}))),
              ES_q25 = quantile({{var}}, probs=0.25, na.rm = TRUE),
              ES_q75 = quantile({{var}}, probs=0.75, na.rm = TRUE),
              .groups = "drop")
  p1<-ggplot(ES_sum[ES_sum$Climate=="RCP45" & ES_sum$Model =="FORDYN",])+
    geom_point(aes(x = MidYear, y = ES, col = Management))+
    geom_ribbon(aes(x = MidYear, ymin = ES - ES_se*1.96, ymax = ES + ES_se*1.96, fill = Management), alpha = 0.3)+
    geom_line(aes(x = MidYear, y = ES, col = Management))+
    geom_vline(xintercept = 2020, linetype="dashed")+
    annotate("rect", xmin = 2000, xmax = 2020, ymin = -Inf, ymax = Inf, alpha = 0.3)+
    scale_x_continuous("",breaks = unique(ES_sum$MidYear),
                       labels = unique(ES_sum$Period), limits = c(2000,2101), expand = c(0,0))+    
    scale_fill_brewer("Escenaris de\ngestió", palette = "Set1")+
    scale_color_brewer("Escenaris de\ngestió", palette = "Set1")+
    ylab(ylab)+ ylim(ylim)+labs(title = "FORDYN" , subtitle="RCP 4.5")+theme_bw()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=10),
          axis.title.y = element_text(size = 15),
          plot.title = element_text(size=22),
          axis.text.y = element_text(size=12),
          plot.subtitle=element_text(size=18),
          legend.text = element_text(size=12),
          legend.title = element_text(size=15),
          legend.key.size = unit(0.6, 'cm'))
  l <- get_legend(p1)
  p2<-ggplot(ES_sum[ES_sum$Climate=="RCP85" & ES_sum$Model =="FORDYN",])+
    geom_point(aes(x = MidYear, y = ES, col = Management))+
    geom_ribbon(aes(x = MidYear, ymin = ES - ES_se*1.96, ymax = ES + ES_se*1.96, fill = Management), alpha = 0.3)+
    geom_line(aes(x = MidYear, y = ES, col = Management))+
    geom_vline(xintercept = 2020, linetype="dashed")+
    annotate("rect", xmin = 2000, xmax = 2020, ymin = -Inf, ymax = Inf, alpha = 0.3)+
    scale_x_continuous("",breaks = unique(ES_sum$MidYear),
                       labels = unique(ES_sum$Period), limits = c(2000,2101), expand = c(0,0))+
    scale_fill_brewer(palette = "Set1")+
    scale_color_brewer(palette = "Set1")+
    ylab("")+ ylim(ylim)+labs(title = "", subtitle = "RCP 8.5")+ theme_bw()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=10),
          axis.title.y = element_text(size = 15),
          plot.title = element_text(size=22),
          axis.text.y = element_text(size=12),
          plot.subtitle=element_text(size=18),
          legend.text = element_text(size=12),
          legend.title = element_text(size=15),
          legend.key.size = unit(0.6, 'cm'))
  pA <- plot_grid(p1 + theme(legend.position = "none"), 
                  p2+ theme(legend.position = "none"), nrow = 1)
  if(add_formes) {
    p3<-ggplot(ES_sum[ES_sum$Climate=="RCP45" & ES_sum$Model =="FORMES",])+
      geom_point(aes(x = MidYear, y = ES, col = Management))+
      geom_ribbon(aes(x = MidYear, ymin = ES - ES_se*1.96, ymax = ES + ES_se*1.96, fill = Management), alpha = 0.3)+
      geom_line(aes(x = MidYear, y = ES, col = Management))+
      geom_vline(xintercept = 2020, linetype="dashed")+
      annotate("rect", xmin = 2000, xmax = 2020, ymin = -Inf, ymax = Inf, alpha = 0.3)+
      scale_x_continuous("",breaks = unique(ES_sum$MidYear),
                         labels = unique(ES_sum$Period), limits = c(2000,2101), expand = c(0,0))+
      scale_fill_brewer(palette = "Set1")+
      scale_color_brewer(palette = "Set1")+
      ylab(ylab)+ ylim(ylim)+labs(title = "FORMES",  subtitle = "RCP 4.5")+theme_bw()+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=10),
            axis.title.y = element_text(size = 15),
            plot.title = element_text(size=22),
            axis.text.y = element_text(size=12),
            plot.subtitle=element_text(size=18),
            legend.text = element_text(size=12),
            legend.title = element_text(size=15),
            legend.key.size = unit(0.6, 'cm'))
    l <- get_legend(p1)
    p4<-ggplot(ES_sum[ES_sum$Climate=="RCP85"& ES_sum$Model =="FORMES",])+
      geom_point(aes(x = MidYear, y = ES, col = Management))+
      geom_ribbon(aes(x = MidYear, ymin = ES - ES_se*1.96, ymax = ES + ES_se*1.96, fill = Management), alpha = 0.3)+
      geom_line(aes(x = MidYear, y = ES, col = Management))+
      geom_vline(xintercept = 2020, linetype="dashed")+
      annotate("rect", xmin = 2000, xmax = 2020, ymin = -Inf, ymax = Inf, alpha = 0.3)+
      scale_x_continuous("",breaks = unique(ES_sum$MidYear), limits = c(2000,2101), expand = c(0,0),
                         labels = unique(ES_sum$Period))+
      scale_fill_brewer(palette = "Set1")+
      scale_color_brewer(palette = "Set1")+
      ylab("")+ ylim(ylim)+labs(title = "", subtitle = "RCP 8.5")+ theme_bw()+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=10),
            axis.title.y = element_text(size = 15),
            plot.title = element_text(size=22),
            axis.text.y = element_text(size=12),
            plot.subtitle=element_text(size=18),
            legend.text = element_text(size=12),
            legend.title = element_text(size=15),
            legend.key.size = unit(0.6, 'cm'))
    pB <- plot_grid(p3 + theme(legend.position = "none"), 
                    p4+ theme(legend.position = "none"), nrow = 1)
    pALL <- plot_grid(pA,pB, nrow = 2)
  } else {
    pALL <- pA
  }
  return(plot_grid(pALL, l, rel_widths = c(1,0.2)))
}

# Functions to generate maps --------------------------------------------------------
sf::st_bbox(nfiplot)
xmin <- 260000
xmax <- 520000
ymin <- 4495000
ymax <- 4750000
res = 5000

map_scenario_state<-function(sf_ALL, var, model, climate_scen, breaks, breaks_diff, units, units_2, type = "div", palette = "YlGnBu") {
  r <-terra::rast(resolution  = c(res,res), xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
  limits <- c(min(breaks), max(breaks))
  limits_diff <- c(min(breaks_diff), max(breaks_diff))
  leg_pos <- c(0.85,0.2)
  
  sf_1 <- sf_ALL |>
    filter(Year == 2010, Climate == climate_scen, Management == "BAU") |>
    select(Management, all_of(var)) |>
    pivot_wider(names_from = "Management", values_from = var)
  sf_2 <- sf_ALL |>
    filter(Year == 2050, Climate == climate_scen) |>
    select(Management, all_of(var)) |>
    pivot_wider(names_from = "Management", values_from = var) |>
    mutate(diff_AMF = AMF - BAU,
           diff_RSB = RSB - BAU,
           diff_ASEA = ASEA - BAU,
           diff_ACG = ACG - BAU) 
  
  sf_3 <- sf_ALL |>
    filter(Year == 2090, Climate == climate_scen) |>
    select(Management, all_of(var)) |>
    pivot_wider(names_from = "Management", values_from = var) |>
    mutate(diff_AMF = AMF - BAU,
           diff_RSB = RSB - BAU,
           diff_ASEA = ASEA - BAU,
           diff_ACG = ACG - BAU)
  
  raster_01<-terra::rasterize(terra::vect(sf_1),r, "BAU", fun = mean, na.rm = TRUE)
  raster_02<-terra::rasterize(terra::vect(sf_2),r,  "BAU", fun = mean, na.rm = TRUE)
  raster_03<-terra::rasterize(terra::vect(sf_3),r,  "BAU", fun = mean, na.rm = TRUE)
  names(raster_01) <- "m1"
  names(raster_02) <- "m2"
  names(raster_03) <- "m3"
  g1<-ggplot()+
    geom_spatraster(aes(fill=m1), data = raster_01)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, palette = palette, 
                         breaks = breaks, limits = limits, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = paste(model, climate_scen, "BAU", sep=" / "), subtitle = "2010")+
    theme_bw()
  g2<-ggplot()+
    geom_spatraster(aes(fill=m2), data = raster_02)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, palette = palette, 
                         breaks = breaks, limits = limits, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2050")+
    theme_bw()
  g3<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, palette = palette, 
                         breaks = breaks, limits = limits, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2090")+
    theme_bw()
  ## ASEA
  raster_02<-terra::rasterize(terra::vect(sf_2),r,  "diff_ASEA" , fun = mean, na.rm = TRUE)
  raster_03<-terra::rasterize(terra::vect(sf_3),r,  "diff_ASEA", fun = mean, na.rm = TRUE)
  names(raster_02) <- "m2"
  names(raster_03) <- "m3"
  g22<-ggplot()+
    geom_spatraster(aes(fill=m2), data = raster_02)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff, limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units_2, title = paste(model, climate_scen, "ASEA Diff.", sep=" / "), subtitle = "2050")+
    theme_bw()
  g23<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff,  limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2090")+
    theme_bw()
  ## ACG
  raster_02<-terra::rasterize(terra::vect(sf_2),r,  "diff_ACG", fun = mean, na.rm = TRUE)
  raster_03<-terra::rasterize(terra::vect(sf_3),r,  "diff_ACG", fun = mean, na.rm = TRUE)
  names(raster_02) <- "m2"
  names(raster_03) <- "m3"
  g32<-ggplot()+
    geom_spatraster(aes(fill=m2), data = raster_02)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff, limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = paste(model, climate_scen, "ACG Diff.", sep=" / "), subtitle = "2050")+
    theme_bw()
  g33<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff,  limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2090")+
    theme_bw()
  ## AMF
  raster_02<-terra::rasterize(terra::vect(sf_2),r,  "diff_AMF", fun = mean, na.rm = TRUE)
  raster_03<-terra::rasterize(terra::vect(sf_3),r,  "diff_AMF", fun = mean, na.rm = TRUE)
  names(raster_02) <- "m2"
  names(raster_03) <- "m3"
  g42<-ggplot()+
    geom_spatraster(aes(fill=m2), data = raster_02)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff, limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = paste(model, climate_scen, "AMF Diff.", sep=" / "), subtitle = "2050")+
    theme_bw()
  g43<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff,  limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2090")+
    theme_bw()
  ## RSB
  raster_02<-terra::rasterize(terra::vect(sf_2),r,  "diff_RSB", fun = mean, na.rm = TRUE)
  raster_03<-terra::rasterize(terra::vect(sf_3),r,  "diff_RSB", fun = mean, na.rm = TRUE)
  names(raster_02) <- "m2"
  names(raster_03) <- "m3"
  g52<-ggplot()+
    geom_spatraster(aes(fill=m2), data = raster_02)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff, limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = paste(model, climate_scen, "RSB Diff.", sep=" / "), subtitle = "2050")+
    theme_bw()
  g53<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff,  limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2090")+
    theme_bw()
  leg1 <- g1 + theme(legend.key.size = unit(2, 'cm'), #change legend key size
                     legend.key.height = unit(1.5, 'cm'), #change legend key height
                     legend.key.width = unit(1.5, 'cm'), #change legend key width
                     legend.title = element_text(size=18), #change legend title font size
                     legend.text = element_text(size=14),
                     legend.title.align=0.5) 
  leg2 <- g22 + theme(legend.key.size = unit(2, 'cm'), #change legend key size
                      legend.key.height = unit(1.5, 'cm'), #change legend key height
                      legend.key.width = unit(1.5, 'cm'), #change legend key width
                      legend.title = element_text(size=18), #change legend title font size
                      legend.text = element_text(size=14),
                      legend.title.align=0.5)
  g<-plot_grid(g1+theme(legend.position = "none", plot.title = element_text(size=22)),
               g2+theme(legend.position = "none"),
               g3+theme(legend.position =  "none"), 
               get_legend(leg1) ,
               g22+theme(legend.position = "none"), 
               g23+theme(legend.position = "none"),
               get_legend(leg2),
               g52+theme(legend.position = "none"), 
               g53+theme(legend.position = "none"),
               NULL,
               g42+theme(legend.position = "none"), 
               g43+theme(legend.position = "none"),
               NULL,
               g32+theme(legend.position = "none"), 
               g33+theme(legend.position = "none"),
               nrow = 5, ncol = 3)
  return(g)
}

map_scenario_states<-function(ES_state, var, breaks, breaks_diff, units,units_2, type = "div", palette = "YlGnBu", draw_formes = TRUE) {
  m_ES <- map_scenario_state(ES_state[ES_state$Model=="FORDYN",], model = "FORDYN", var = var, climate_scen = "RCP45", 
                             breaks = breaks, breaks_diff = breaks_diff, units = units,units_2=units_2, type = type, palette = palette)
  ggsave2(paste0("Plots/ES_maps_0209/", var, "_fordyn_rcp45.png"),m_ES, width = 13, height = 22, bg = "white")
  m_ES <- map_scenario_state(ES_state[ES_state$Model=="FORDYN",], model = "FORDYN",var = var, climate_scen = "RCP85", 
                             breaks = breaks, breaks_diff = breaks_diff, units = units,units_2=units_2)
  ggsave2(paste0("Plots/ES_maps_0209/",var,"_fordyn_rcp85.png"),m_ES, width = 13, height = 22, bg = "white")
  if(draw_formes) {
    m_ES <- map_scenario_state(ES_state[ES_state$Model=="FORMES",], model = "FORMES",var = var, climate_scen = "RCP45",
                               breaks = breaks, breaks_diff = breaks_diff, units = units,units_2=units_2)
    ggsave2(paste0("Plots/ES_maps_0209/",var,"_formes_rcp45.png"),m_ES, width = 13, height = 22, bg = "white")
    m_ES <- map_scenario_state(ES_state[ES_state$Model=="FORMES",], model = "FORMES", var = var, climate_scen = "RCP85",
                               breaks = breaks, breaks_diff = breaks_diff, units = units,units_2=units_2)
    ggsave2(paste0("Plots/ES_maps_0209/",var,"_formes_rcp85.png"),m_ES, width = 13, height = 22, bg = "white")
  }
}

map_scenario_period<-function(sf_ALL, var, model, climate_scen, breaks, breaks_diff, units, units_2, type = "div", palette = "YlGnBu") {
  r <-terra::rast(resolution  = c(res,res), xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
  limits <- c(min(breaks), max(breaks))
  limits_diff <- c(min(breaks_diff), max(breaks_diff))
  leg_pos <- c(0.85,0.2)
  sf_1 <- sf_ALL |>
    filter(Period == "2001-2020", Climate == climate_scen, Management == "BAU") |>
    select(Management, all_of(var)) |>
    pivot_wider(names_from = "Management", values_from = var)
  
  sf_2 <- sf_ALL |>
    filter(Period == "2031-2050", Climate == climate_scen) |>
    select(Management, all_of(var)) |>
    pivot_wider(names_from = "Management", values_from = var) |>
    mutate(diff_AMF = AMF - BAU,
           diff_RSB = RSB - BAU,
           diff_ASEA = ASEA - BAU,
           diff_ACG = ACG - BAU) 
  
  sf_3 <- sf_ALL |>
    filter(Period == "2081-2100", Climate == climate_scen) |>
    select(Management, all_of(var)) |>
    pivot_wider(names_from = "Management", values_from = var) |>
    mutate(diff_AMF = AMF - BAU,
           diff_RSB = RSB - BAU,
           diff_ASEA = ASEA - BAU,
           diff_ACG = ACG - BAU)
  
  raster_01<-terra::rasterize(terra::vect(sf_1),r, "BAU", fun = mean, na.rm = TRUE)
  raster_02<-terra::rasterize(terra::vect(sf_2),r,  "BAU", fun = mean, na.rm = TRUE)
  raster_03<-terra::rasterize(terra::vect(sf_3),r,  "BAU", fun = mean, na.rm = TRUE)
  names(raster_01) <- "m1"
  names(raster_02) <- "m2"
  names(raster_03) <- "m3"
  g1<-ggplot()+
    geom_spatraster(aes(fill=m1), data = raster_01)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, palette = palette, 
                         breaks = breaks, limits = limits, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = paste(model, climate_scen, "BAU", sep=" / "), subtitle = "2001-2020")+
    theme_bw()
  g2<-ggplot()+
    geom_spatraster(aes(fill=m2), data = raster_02)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, palette = palette, 
                         breaks = breaks, limits = limits, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2031-2050")+
    theme_bw()
  g3<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, palette = palette, 
                         breaks = breaks, limits = limits, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2081-2100")+
    theme_bw()
  ## ASEA
  raster_02<-terra::rasterize(terra::vect(sf_2),r,  "diff_ASEA", fun = mean, na.rm = TRUE)
  raster_03<-terra::rasterize(terra::vect(sf_3),r,  "diff_ASEA", fun = mean, na.rm = TRUE)
  names(raster_02) <- "m2"
  names(raster_03) <- "m3"
  g22<-ggplot()+
    geom_spatraster(aes(fill=m2), data = raster_02)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff, limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units_2, title = paste(model, climate_scen, "ASEA Diff.", sep=" / "), subtitle = "2031-2050")+
    theme_bw()
  g23<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff,  limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2081-2100")+
    theme_bw()
  ## ACG
  raster_02<-terra::rasterize(terra::vect(sf_2),r,  "diff_ACG", fun = mean, na.rm = TRUE)
  raster_03<-terra::rasterize(terra::vect(sf_3),r,  "diff_ACG", fun = mean, na.rm = TRUE)
  names(raster_02) <- "m2"
  names(raster_03) <- "m3"
  g32<-ggplot()+
    geom_spatraster(aes(fill=m2), data = raster_02)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff, limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = paste(model, climate_scen, "ACG Diff.", sep=" / "), subtitle = "2031-2050")+
    theme_bw()
  g33<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff,  limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2081-2100")+
    theme_bw()
  ## AMF
  raster_02<-terra::rasterize(terra::vect(sf_2),r,  "diff_AMF", fun = mean, na.rm = TRUE)
  raster_03<-terra::rasterize(terra::vect(sf_3),r,  "diff_AMF", fun = mean, na.rm = TRUE)
  names(raster_02) <- "m2"
  names(raster_03) <- "m3"
  g42<-ggplot()+
    geom_spatraster(aes(fill=m2), data = raster_02)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff, limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = paste(model, climate_scen, "AMF Diff.", sep=" / "), subtitle = "2031-2050")+
    theme_bw()
  g43<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff,  limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2081-2100")+
    theme_bw()
  ## RSB
  raster_02<-terra::rasterize(terra::vect(sf_2),r,  "diff_RSB", fun = mean, na.rm = TRUE)
  raster_03<-terra::rasterize(terra::vect(sf_3),r,  "diff_RSB", fun = mean, na.rm = TRUE)
  names(raster_02) <- "m2"
  names(raster_03) <- "m3"
  g52<-ggplot()+
    geom_spatraster(aes(fill=m2), data = raster_02)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff, limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = paste(model, climate_scen,"RSB Diff.", sep=" / "), subtitle = "2031-2050")+
    theme_bw()
  g53<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff,  limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2081-2100")+
    theme_bw()
  # browser() # scale argument
  leg1 <- g1 + theme(legend.key.size = unit(2, 'cm'), #change legend key size
                     legend.key.height = unit(1.5, 'cm'), #change legend key height
                     legend.key.width = unit(1.5, 'cm'), #change legend key width
                     legend.title = element_text(size=18), #change legend title font size
                     legend.text = element_text(size=14),
                     legend.title.align=0.5) 
  leg2 <- g22 + theme(legend.key.size = unit(2, 'cm'), #change legend key size
                       legend.key.height = unit(1.5, 'cm'), #change legend key height
                       legend.key.width = unit(1.5, 'cm'), #change legend key width
                       legend.title = element_text(size=18), #change legend title font size
                       legend.text = element_text(size=14),
                       legend.title.align=0.5)
  
  
  
  ll <- plot_grid(get_legend(leg1), get_legend(leg2), nrow = 1) +
    theme(plot.margin = margin(t = 0, r = 50, b = 0, l = 50, unit = "pt"))
  
  g<-plot_grid(g1+theme(legend.position = "none", plot.title = element_text(size=22)), # BAU
               g2+theme(legend.position = "none"),
               g3+theme(legend.position =  "none"), 
               get_legend(leg1)  ,
               g22+theme(legend.position = "none"), # ASEA
               g23+theme(legend.position = "none"),
               get_legend(leg2),
               g52+theme(legend.position = "none"), # RSB
               g53+theme(legend.position = "none"),
               NULL,
               g42+theme(legend.position = "none"), # AMF
               g43+theme(legend.position = "none"),
               NULL,
               g32+theme(legend.position = "none"), # ACG
               g33+theme(legend.position = "none"),
               nrow = 5, ncol = 3)
  return(g)
}
map_scenario_periods<-function(ES_period, var, breaks, breaks_diff, units,units_2, type = "div", palette = "YlGnBu", draw_formes = TRUE) {
  m_ES <- map_scenario_period(ES_period[ES_period$Model=="FORDYN",], model = "FORDYN", var = var, climate_scen = "RCP45", 
                             breaks = breaks, breaks_diff = breaks_diff, units = units,units_2 = units_2, type = type, palette = palette)
  ggsave2(paste0("Plots/ES_maps_0209/", var, "_fordyn_rcp45.png"),m_ES, width = 13, height = 22, bg = "white")
  m_ES <- map_scenario_period(ES_period[ES_period$Model=="FORDYN",], model = "FORDYN",var = var, climate_scen = "RCP85", 
                             breaks = breaks, breaks_diff = breaks_diff, units = units,units_2 = units_2)
  ggsave2(paste0("Plots/ES_maps_0209/",var,"_fordyn_rcp85.png"),m_ES, width = 13, height = 22, bg = "white")
  if(draw_formes) {
    m_ES <- map_scenario_period(ES_period[ES_period$Model=="FORMES",], model = "FORMES",var = var, climate_scen = "RCP45",
                                breaks = breaks, breaks_diff = breaks_diff, units = units,units_2 = units_2)
    ggsave2(paste0("Plots/ES_maps_0209/",var,"_formes_rcp45.png"),m_ES, width = 13, height = 22, bg = "white")
    m_ES <- map_scenario_period(ES_period[ES_period$Model=="FORMES",], model = "FORMES", var = var, climate_scen = "RCP85",
                                breaks = breaks, breaks_diff = breaks_diff, units = units,units_2 = units_2)
    ggsave2(paste0("Plots/ES_maps_0209/",var,"_formes_rcp85.png"),m_ES, width = 13, height = 22, bg = "white")
  }
}


# ES1_VolumeStructure -----------------------------------------------------
d_ES <- plot_ES_state(ES_state, ES1_VolumeStructure, "Stock fusta estructural (m3/ha)", c(0,250), add_formes = TRUE)
ggsave2("Plots/ES_dynamics_0209/ES1_VolumeStructure.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_state$ES1_VolumeStructure)
map_scenario_states(ES_state, var = "ES1_VolumeStructure", 
                    breaks = c(0,10, 25,50,100,150, 200, 300,500, 1000), 
                    breaks_diff = c(-500, -200, -100,-50, -25, -10, 10, 25, 50, 100, 200, 500), 
                    units = "Stock fusta estructural (m3/ha)\n",
                    units_2 = "Diferència respecte el BAU \n(m3/ha)\n")

# ES1_VolumeAdultFirewood -------------------------------------------------
d_ES <- plot_ES_state(ES_state, ES1_VolumeAdultFirewood, "Stock llenyes (m3/ha)", c(0,100), add_formes = TRUE)
ggsave2("Plots/ES_dynamics_0209/ES1_VolumeAdultFirewood.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_state$ES1_VolumeAdultFirewood)
map_scenario_states(ES_state, var = "ES1_VolumeAdultFirewood", 
                    breaks = c(0,5, 10, 25, 50, 75, 100, 125, 150, 200), 
                    breaks_diff = c(-100,-75,-50, -25,-10, -5, 5, 10, 25, 50, 75, 100), 
                    units = "Stock llenyes (m3/ha)\n",
                    units_2 ="Diferència respecte el BAU \n(m3/ha)\n" )

# ES1_CutStructure --------------------------------------------------------
d_ES <- plot_ES_period(ES_period_new, ES1_CutStructure, "Provisió de fusta\nestructural (m3/ha/any)", ylim = c(0,3), 
                       outliers = c(-1,25), add_formes = TRUE)
ggsave2("Plots/ES_dynamics_0209/ES1_CutStructure.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_period_new$ES1_CutStructure)
map_scenario_periods(ES_period_new, var = "ES1_CutStructure", 
                    breaks = c(0,0.1,0.5,1,2,5,10, 20), 
                    breaks_diff = c(-20,-10,-5, -2, -1,1, 2,5, 10, 20), 
                    units = "Provisió de fusta estructural BAU \n(m3/ha/any)\n",
                    units_2 = "Diferència respecte el BAU \n(m3/ha/any)\n")

# ES1_CutAdultFirewood ----------------------------------------------------
d_ES <- plot_ES_period(ES_period_new, ES1_CutAdultFirewood, ylab= "Provisió de\nllenyes (m3/ha/any)", 
                ylim = c(0,2), outliers = c(-2,20), add_formes = TRUE)
ggsave2("Plots/ES_dynamics_0209/ES1_CutAdultFirewood.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_period_new$ES1_CutAdultFirewood)
map_scenario_periods(ES_period_new, var = "ES1_CutAdultFirewood", 
                     breaks = c(0,0.1,0.2, 0.5,1,2,5,10, 20), 
                     breaks_diff = c(-20, -5, -2, -1,-0.5, -0.2, 0.2, 0.5, 1, 2,5, 20), 
                     units = "Provisió de\nllenyes (m3/ha/any)\n",
                     units_2 = "Diferència respecte el BAU \n(m3/ha/any)\n")

# ES2_AdultTreeBiomass --------------------------------------------
d_ES <- plot_ES_state(ES_state, ES2_AdultTreeBiomass, ylab = "Stock de carboni\narbres (Mg C/ha)", ylim = c(100,700), 
                outliers = c(-1, 1000), add_formes = TRUE)
ggsave2("Plots/ES_dynamics_0209/ES2_AdultTreeBiomass.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_state$ES2_AdultTreeBiomass)
map_scenario_states(ES_state, var = "ES2_AdultTreeBiomass", 
                    breaks = c(0,10,20, 50,100,200, 500, 1000, 2000), 
                    breaks_diff = c(-1000, -500, -200, -100, -50, -10, 10, 50, 100 ,200, 500, 1000), 
                    units = "Stock de carboni\narbres (Mg C/ha)\n",
                    units_2 = "Diferència respecte el BAU \n(Mg C/ha)\n")

# ES2_AdultTreeBiomassChange --------------------------------------------
d_ES <- plot_ES_period(ES_period_new, ES2_AdultTreeBiomassChange, ylab = "Embornal de carboni\narbres (Mg C/ha/any)", 
                outliers = c(-10,10), ylim = c(-1,6), add_formes = TRUE)
ggsave2("Plots/ES_dynamics_0209/ES2_AdultTreeBiomassChange.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_period_new$ES2_AdultTreeBiomassChange)
map_scenario_periods(ES_period_new, var = "ES2_AdultTreeBiomassChange",
                     breaks = c(-10,-0.5, 0.5,1,2,5,10,20, 50), 
                     breaks_diff = c(-20, -10, -5, -2, -1,-0.5, 0.5, 1, 2,5, 10, 20), 
                     units = "Embornal de carboni\narbres (Mg C/ha/any)",
                     units_2 = "Diferència respecte el BAU \n(Mg C/ha/any)\n")

# ES2_CutBiomassStructure --------------------------------------------
d_ES <- plot_ES_period(ES_period_new, ES2_CutBiomassStructure, ylab = "Embornal de carboni\nfusta estructural (Mg C/ha/any)", 
                outlier = c(-1,50), ylim = c(0,5), add_formes = TRUE)
ggsave2("Plots/ES_dynamics_0209/ES2_CutBiomassStructure.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_period_new$ES2_CutBiomassStructure)
map_scenario_periods(ES_period_new, var = "ES2_CutBiomassStructure",
                     breaks = c(0.0,0.1, 0.2, 0.5,1,2,5,10,20, 50), 
                     breaks_diff = c(-50, -20, -5, -2, -1,-0.5, 0.5, 1, 2,5, 20,50), 
                     units = "Embornal de carboni\nfusta estructural (Mg C/ha/any)",
                     units_2 = "Diferència respecte el BAU \n(Mg C/ha/any)\n")


# ES2_SequestrPlusFirewood --------------------------------------------
d_ES <- plot_ES_period(ES_period_new, ES2_SequestrPlusFirewood, ylab = "Embornal de carboni\narbres + fusta + llenya (Mg C/ha/any)", 
                       outlier = c(-1,50), ylim = c(2,7.5), add_formes = TRUE)
ggsave2("Plots/ES_dynamics_0209/ES2_SequestrPlusFirewood.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_period_new$ES2_SequestrPlusFirewood)
# map_scenario_periods(ES_period_new, var = "ES2_SequestrPlusFirewood",
#                      breaks = c(0.0,0.1, 0.2, 0.5,1,2,5,10,20, 50), 
#                      breaks_diff = c(-50, -20, -5, -2, -1,-0.5, 0.5, 1, 2,5, 20,50), 
#                      units = "Embornal de carboni\nfusta estructural (Mg C/ha/any)",
#                      units_2 = "Diferència respecte el BAU \n(Mg C/ha/any)\n")

# ES2_CutBiomassAdultFirewood --------------------------------------------
d_ES <- plot_ES_period(ES_period_new, ES2_CutBiomassAdultFirewood, ylab = "Biomassa llenya (Mg C/ha/any)", 
                       outlier = c(-1,50), ylim = c(0,4), add_formes = TRUE)
ggsave2("Plots/ES_dynamics_0209/ES2_CutBiomassAdultFirewood.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_period_new$ES2_CutBiomassAdultFirewood)
# map_scenario_periods(ES_period_new, var = "ES2_SequestrPlusFirewood",
#                      breaks = c(0.0,0.1, 0.2, 0.5,1,2,5,10,20, 50), 
#                      breaks_diff = c(-50, -20, -5, -2, -1,-0.5, 0.5, 1, 2,5, 20,50), 
#                      units = "Embornal de carboni\nfusta estructural (Mg C/ha/any)",
#                      units_2 = "Diferència respecte el BAU \n(Mg C/ha/any)\n")


# ES2_AdultTreeBiomassSequestr --------------------------------------------
d_ES <- plot_ES_period(ES_period_new, ES2_AdultTreeBiomassSequestr, ylab = "Embornal de carboni\narbres+fusta (Mg C/ha/any)", 
                outlier = c(-50,50), ylim = c(0,6), add_formes = TRUE)
ggsave2("Plots/ES_dynamics_0209/ES2_AdultTreeBiomassSequestr.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_period_new$ES2_AdultTreeBiomassSequestr)
map_scenario_periods(ES_period_new, var = "ES2_AdultTreeBiomassSequestr",
                     breaks = c(-10,-0.5, 0.5,1,2,5,10,20,50), 
                     breaks_diff = c(-50, -20, -5, -2, -1,-0.5, 0.5, 1, 2,5, 20,50), 
                     units = "Embornal de carboni\narbres+fusta (Mg C/ha/any)",
                     units_2 = "Diferència respecte el BAU \n(Mg C/ha/any)\n")

# ES2_LiveBiomassSequestr --------------------------------------------
d_ES <- plot_ES_period(ES_period_new, ES2_LiveBiomassSequestr, ylab = "Embornal de carboni\ntotal (Mg C/ha/any)", 
                       outliers = c(-50,50), ylim = c(-1,6), add_formes = FALSE)
ggsave2("Plots/ES_dynamics_0209/ES2_LiveBiomassSequestr.png",d_ES, width = 10, height = 4, bg = "white")
summary(ES_period_new$ES2_LiveBiomassSequestr)
map_scenario_periods(ES_period_new, var = "ES2_LiveBiomassSequestr",
                     breaks = c(-10,-0.5, 0.5,1,2,5,10,20,50), 
                     breaks_diff = c(-20, -5, -2, -1,-0.5, -0.2, 0.2, 0.5, 1, 2,5, 20), 
                     units = "Embornal de carboni\ntotal (Mg C/ha/any)",
                     units_2 = "Diferència respecte el BAU \n(Mg C/ha/any)\n",
                     draw_formes = FALSE)

# ES3_LAI -------------------------------------------------------
d_ES <- plot_ES_period(ES_period_new, ES3_LAI, ylab = "Índex d'àrea foliar", ylim = c(2,4.5), add_formes = T)
ggsave2("Plots/ES_dynamics_0209/ES3_LAI.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_period_new$ES3_LAI)
map_scenario_periods(ES_period_new, var = "ES3_LAI", 
                     breaks = seq(0,4.5, by=0.5), 
                     breaks_diff = c(-2,-1.5,-1.0,-0.5,-0.25,0.25,0.5,1.0,1.5,2), 
                     units = "Índex d'àrea foliar (m2/m2)",
                     units_2 = "Diferència respecte el BAU \n(m2/m2)\n",
                     draw_formes = TRUE)

d_ES <- plot_ES_period(ES_period_new, ES3_Precipitation, ylab = "Precipitacio (mm/any)", ylim = c(0,1200), add_formes = F)
ggsave2("Plots/ES_dynamics_0209/ES3_Precipitation.png",d_ES, width = 10, height = 8, bg = "white")

# ES3_BlueWater -----------------------------------------------------------
d_ES <- plot_ES_period(ES_period_new, ES3_BlueWater, ylab = "Aigua blava (mm/any)", ylim = c(100,300), add_formes = T)
ggsave2("Plots/ES_dynamics_0209/ES3_BlueWater.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_period_new$ES3_BlueWater)
map_scenario_periods(ES_period_new, var = "ES3_BlueWater", 
                     breaks = seq(0,320, by=40), 
                     breaks_diff = c(-200, -160, -120, -80, -40, -20, 20, 40, 80, 120, 160, 200), 
                     units = "Aigua blava (mm/any)",
                     units_2 = "Diferència respecte el BAU \n(mm/any)\n",
                     draw_formes = TRUE)

# ES3_RunoffCoefficient ---------------------------------------------------
d_ES <- plot_ES_period(ES_period_new, ES3_RunoffCoefficient, ylab = "Coeficient d'escolament [%]", ylim = c(15,40), add_formes = T)
ggsave2("Plots/ES_dynamics_0209/ES3_RunoffCoefficient.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_period_new$ES3_RunoffCoefficient)
map_scenario_periods(ES_period_new, var = "ES3_RunoffCoefficient", 
                     breaks = c(0,5,10,20,40,60,80, 100), 
                     breaks_diff = c(-50,-10,-20,-5, -2,2,5,10,20,50), 
                     units = "Coeficient d'escolament [%]",
                     units_2 = "Diferència respecte el BAU \n[%]\n",
                     draw_formes = TRUE)

# ES4_ErosionMitigation ---------------------------------------------------
d_ES <- plot_ES_period(ES_period_new, ES4_ErosionMitigation, ylab = "Mitigació de l'erosió (Mg/ha/any)", ylim = c(100,170), # 
                outliers = c(-1,3000), add_formes = TRUE)
ggsave2("Plots/ES_dynamics_0209/ES4_ErosionMitigation.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_period_new$ES4_ErosionMitigation)
map_scenario_periods(ES_period_new, var = "ES4_ErosionMitigation", 
                     breaks = c(0,25, 50, 100, 150, 200, 300, 1000, 4000), 
                     breaks_diff = c(-100, -50,-20, -10,-5, 5,10,20, 50, 100), 
                     units = "Mitigació de l'erosió (Mg/ha/any)",
                     units_2 = "Diferència respecte el BAU \n(Mg/ha/any)\n",
                     draw_formes = TRUE)


# ES5_RecreationalValue ---------------------------------------------------
d_ES <- plot_ES_state(ES_state, ES5_RecreationalValue, ylab = "Valor recreatiu [0-1]", ylim = c(0.3,0.55), add_formes = TRUE)
ggsave2("Plots/ES_dynamics_0209/ES5_RecreationalValue.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_state$ES5_RecreationalValue)
map_scenario_states(ES_state, var = "ES5_RecreationalValue", 
                     breaks = c(0.1,0.2,0.4,0.5,0.6,0.7,0.8, 0.9, 1), 
                    breaks_diff = c(-0.5,-0.3, -0.2, -0.1, -0.05, 0.05, 0.1, 0.2, 0.3, 0.5),
                    units = "Valor recreatiu [0-1]",
                    units_2 = "Diferència respecte el BAU\n",
                    draw_formes = TRUE)


# ES6_SurfaceFirePotential  ---------------------------------------------------
d_ES <- plot_ES_period(ES_period_new, ES6_SurfaceFirePotential, ylab = "Risk d'incendi\nde superficie [0-9]", ylim = c(5,9), 
                       add_formes = TRUE)
ggsave2("Plots/ES_dynamics_0209/ES6_SurfaceFirePotential.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_period_new$ES6_SurfaceFirePotential)
map_scenario_periods(ES_period_new, var = "ES6_SurfaceFirePotential", 
                    breaks = seq(0,9, by=1), 
                    breaks_diff = c(-5,-4,-3,-2,-1,1,2,3,4,5), 
                    units = "Risk d'incendi\nde superficie [0-9]",
                    units_2 = "Diferència respecte el BAU\n",
                    draw_formes = TRUE)

# ES6_CrownFirePotential  ---------------------------------------------------
d_ES <- plot_ES_period(ES_period_new, ES6_CrownFirePotential, ylab = "Risk d'incendi\nde capçada [0-9]", ylim = c(2,7), 
                       add_formes = TRUE)
ggsave2("Plots/ES_dynamics_0209/ES6_CrownFirePotential.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_period_new$ES6_CrownFirePotential)
map_scenario_periods(ES_period_new, var = "ES6_CrownFirePotential", 
                     breaks = seq(0,9, by=1), 
                     breaks_diff = c(-5,-4,-3,-2,-1,1,2,3,4,5), 
                     units = "Risk d'incendi\nde capçada [0-9]",
                     units_2 = "Diferència respecte el BAU\n",
                     draw_formes = TRUE)








# TABLES ---------------------------------------------------

ES_period_grp <- ES_period %>% group_by(Period, Model, Management, Climate) %>%
  summarize(across(.cols = ES1_CutStructure:ES6_CrownFirePotential, .fns = list(mean= ~ mean(., na.rm = TRUE),
                                                                                 sum= ~ sum(., na.rm = TRUE))))

ES_period_grp_new <- ES_period_new %>% group_by(Period, Model, Management, Climate) %>%
  summarize(across(.cols = ES1_CutStructure:ES6_CrownFirePotential, .fns = list(mean= ~ mean(., na.rm = TRUE),
                                                                                sum= ~ sum(., na.rm = TRUE))))


saveRDS(ES_period_grp_new, "Rdata/ES_period_grp_0209.rds")

ES_state_grp <- ES_state |> mutate(Period = case_when((Year>=2000 & Year<=2020) ~ "2001-2020",
                                                      (Year>=2031 & Year<=2050) ~ "2031-2050",
                                                      (Year>=2081 & Year<=2100) ~ "2081-2100")) |> 
  group_by(Period, Model, Management, Climate) |> 
  summarize(across(.cols = ES1_VolumeStructure:ES5_RecreationalValue, .fns = list(mean= ~ mean(., na.rm = TRUE),
                                                                                 sum= ~ sum(., na.rm = TRUE))))
saveRDS(ES_state_grp, "Rdata/ES_state_grp_0810.rds")


## 
rm(list=ls())

ES_period_grp <- readRDS("Rdata/ES_period_grp_0209.rds")


# Data creation period
diff_data = vector(mode = "list")
diff_data_bau = vector(mode = "list")
full_data = NULL
full_data_bau = NULL
for(m in unique(ES_period_grp$Model)){
  for(clim in unique(ES_period_grp$Climate)){
    for(man in unique(ES_period_grp$Management)){
      aux <- NULL
      aux_bau <- NULL
      cat(paste("Processing",m, clim, man), "\n")
      for(period in unique(ES_period_grp$Period)){
        base_bau <- ES_period_grp %>% filter(Period == period,
                                             Model == m,
                                             Management == "BAU",
                                             Climate == clim) %>% 
          select(ES1_CutStructure_mean:ES6_CrownFirePotential_sum) %>% 
          st_drop_geometry()
        
        base <- ES_period_grp %>% filter(Period == "2001-2020",
                                         Model == m,
                                         Management == man,
                                         Climate == clim) %>% 
          select(ES1_CutStructure_mean:ES6_CrownFirePotential_sum) %>% 
          st_drop_geometry()
        comp_value <- ES_period_grp %>% filter(Period == period,
                                         Model == m,
                                         Management == man,
                                         Climate == clim)%>% 
          select(ES1_CutStructure_mean:ES6_CrownFirePotential_sum)%>% 
          st_drop_geometry()
        
        diff_perc <- ((comp_value-base)/base)*100
        t_diff <- as.data.frame(t(diff_perc)) ; colnames(t_diff) <- paste("diff",period, sep = "_")
        
        # BAU
        diff_perc_bau <- ((comp_value-base_bau)/base_bau)*100
        t_diff_bau <- as.data.frame(t(diff_perc_bau)) ; colnames(t_diff_bau) <- paste("diff",period, sep = "_")
        
        if(is.null(aux_bau)){
          aux_bau <- t_diff_bau
        } else {
          aux_bau <- cbind(aux_bau, t_diff_bau)
        }

        if(is.null(aux)){
          aux <- t_diff
        } else {
          aux <- cbind(aux, t_diff)
        }
  
      }
      # BAU
      aux_bau$Climate = clim
      aux_bau$Management = man
      aux_bau$Model = m
      aux_bau$ES=rownames(aux_bau)
      diff_data_bau[[paste(m, clim, man, sep = "_")]] <- aux_bau
      full_data_bau <- rbind(full_data_bau, aux_bau)
      
      # initial
      aux$Climate = clim
      aux$Management = man
      aux$Model = m
      aux$ES=rownames(aux)
      diff_data[[paste(m, clim, man, sep = "_")]] <- aux
      full_data <- rbind(full_data, aux)
    }
  }
}

# Data creation state
ES_state_grp <- readRDS("Rdata/ES_state_grp_0810.rds")

create_diff_bau_state <- function(){
  diff_data = vector(mode = "list")
  diff_data_bau = vector(mode = "list")
  full_data = NULL
  full_data_bau = NULL
  for(m in unique(ES_state_grp$Model)){
    for(clim in unique(ES_state_grp$Climate)){
      for(man in unique(ES_state_grp$Management)){
        aux_bau <- NULL
        cat(paste("Processing",m, clim, man), "\n")
        for(period in unique(ES_state_grp$Period)){
          if(is.na(period))next
          base_bau <- ES_state_grp %>% filter(Period == period,
                                              Model == m,
                                              Management == "BAU",
                                              Climate == clim) %>% 
            select(ES1_VolumeStructure_mean:ES5_RecreationalValue_sum) %>% 
            st_drop_geometry()

          comp_value <- ES_state_grp %>% filter(Period == period,
                                                Model == m,
                                                Management == man,
                                                Climate == clim)%>% 
            select(ES1_VolumeStructure_mean:ES5_RecreationalValue_sum)%>% 
            st_drop_geometry()
          
          # BAU
          diff_perc_bau <- ((comp_value-base_bau)/base_bau)*100
          t_diff_bau <- as.data.frame(t(diff_perc_bau)) ; colnames(t_diff_bau) <- paste("diff",period, sep = "_")
          
          if(is.null(aux_bau)){
            aux_bau <- t_diff_bau
          } else {
            aux_bau <- cbind(aux_bau, t_diff_bau)
          }
          
          
        }
        # BAU
        aux_bau$Climate = clim
        aux_bau$Management = man
        aux_bau$Model = m
        aux_bau$ES=rownames(aux_bau)
        diff_data_bau[[paste(m, clim, man, sep = "_")]] <- aux_bau
        full_data_bau <- rbind(full_data_bau, aux_bau)
        
      }
    }
  }
  return(full_data_bau)
}

full_data_bau_state <- create_diff_bau_state()

## Comparison in time ---------------------------------------------------

full_data_filt <- full_data %>% 
  select(Model, Climate, Management,ES, "diff_2001-2020", "diff_2031-2050", "diff_2081-2100") %>% 
  filter(rownames(full_data) %in% grep("_mean", rownames(full_data), value = T))

rownames(full_data_filt) <- NULL

full_data_filt$ES <- substr(full_data_filt$ES, 1, nchar(full_data_filt$ES)-5)

# filtered_diff_data = vector(mode = "list")
# for(i in 1:length(diff_data)){
#   diff_data_fil <- diff_data[[i]] %>%
#     select("diff_2001-2020", "diff_2031-2050", "diff_2081-2100") %>% # only period of interest
#     filter(rownames(diff_data[[i]]) %in% grep("_mean", rownames(diff_data[[i]]), value = T)) # only means
#   filtered_diff_data[[names(diff_data)[i]]] <- diff_data_fil
# }

full_data_filt
knitr::kable(full_data_filt)


full_data_filt <- full_data_filt %>% mutate(across(where(is.numeric),\(x) round(x, 1)))
a <- gt(full_data_filt) |>
  tab_header(title = md("**ES differences**"),
             subtitle = md("All the values are expressed in percentage and calculated as the difference respect to the initial period of reference (2001-2020)")) 

gtsave(a, "Tables/ES_time_0209.html")


## Comparison with BAU period ---------------------------------------------------
full_data_bau_filt <- full_data_bau %>% 
  select(Model, Climate, Management,ES, "diff_2001-2020", "diff_2031-2050", "diff_2081-2100") %>% 
  filter(rownames(full_data_bau) %in% grep("_mean", rownames(full_data_bau), value = T)) 
rownames(full_data_bau_filt) <- NULL

full_data_bau_filt$ES <- substr(full_data_bau_filt$ES, 1, nchar(full_data_bau_filt$ES)-5)
knitr::kable(full_data_bau_filt)


full_data_bau_filt <- full_data_bau_filt %>% mutate(across(where(is.numeric),\(x) round(x, 1)))
a <- gt(full_data_bau_filt) |>
  tab_header(title = md("**ES differences**"),
             subtitle = md("All the values are expressed in percentage and calculated as the difference respect the BAU value of reference")) 

gtsave(a, "Tables/ES_BAU_0209.html")


# Comparison with BAU state ---------------------------------------------------
full_data_bau_filt <- full_data_bau_state  %>% 
  select(Model, Climate, Management,ES, "diff_2001-2020", "diff_2031-2050", "diff_2081-2100") %>% 
  filter(rownames(full_data_bau_state ) %in% grep("_mean", rownames(full_data_bau_state ), value = T)) 
rownames(full_data_bau_filt) <- NULL

full_data_bau_filt$ES <- substr(full_data_bau_filt$ES, 1, nchar(full_data_bau_filt$ES)-5)
knitr::kable(full_data_bau_filt)


full_data_bau_filt <- full_data_bau_filt %>% mutate(across(where(is.numeric),\(x) round(x, 1)))
a <- gt(full_data_bau_filt) |>
  tab_header(title = md("**ES differences**"),
             subtitle = md("All the values are expressed in percentage and calculated as the difference respect the BAU value of reference")) 

gtsave(a, "Tables/ES_BAU_state_0810.html")

a_recreational <- full_data_bau_filt |> filter(ES == "ES5_RecreationalValue")
a_recreational_table <- gt(a_recreational) |>
  tab_header(title = md("**ES recreational values differences**"),
             subtitle = md("All the values are expressed in percentage and calculated as the difference respect the BAU value of reference")) 

gtsave(a_recreational_table, "Tables/ES_BAU_state_recreationalvalue_0810.html")












