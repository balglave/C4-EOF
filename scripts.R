############################
# Codes for the C4 EOF repos
############################

## Load libraries and functions
source("r/library_functions.R")

## Code configurations
source("r/config.R")

## Processing of the model outputs for figures
#---------------------------------------------
if(process_model_outputs){
  
  source("r/process_model_outputs.R")
  
}

source("r/save_or_load.r")

##----------------------------------------------------------------------------------------------------
##----------------------------------- Make plots/analysis --------------------------------------------
##----------------------------------------------------------------------------------------------------
source("r/plot_inputs_maps.R")

## Percentage of variance
#------------------------
source("r/perc_var.R")

## Prepare covariate dataframe to compare with EOF
#------------------------------------------------
# Timing of suitable temperature for reproduction
source("r/prepare_temp_df.R")

## Plot EOF maps and time series
#-------------------------------
## Sole
source("r/sole_eof_ts.R")

## Hake - BoB
source("r/hake_bob_eof_ts.R")

## Hake - CS
source("r/hake_cs_eof_ts.R")

## Seabass
source("r/seabass_eof_ts.R")

## Picture of all
source("r/picture_eof_ts.R")

## Average spatial pattern
source("r/mean_pattern.R")

## Clustering analysis
#---------------------
source("r/function_hcpc_plot.R")

## Sole
source("r/sole_clust.R")

## Hake - BoB
source("r/hake_bob_clust.R")

## Hake - CS
# source("r/hake_cs_clust.R")

## Sea bass
source("r/seabass_clust.R")

title_sole <- ggdraw() + draw_label("Sole",hjust = 0.5, fontface='bold.italic',color = "darkgrey",size = 18, angle = 90)

title_hake <- ggdraw() + draw_label("Hake",hjust = 0.5,fontface='bold.italic',color = "darkgrey",size = 18, angle = 90)

title_seabass <- ggdraw() + draw_label("Sea Bass",hjust = 0.5,fontface='bold.italic',color = "darkgrey",size = 18, angle = 90)

plot_clust <- cowplot::plot_grid(title_sole,proj_map_plot_sole,
                                 title_hake,clust_plot_hake_bob,
                                 title_seabass,clust_plot_seabass,nrow = 3,ncol = 2, 
                                 rel_widths = c(0.1,1),align = "hv")

ggsave("images/clust_full_plot.png",width = 13.5,height = 13.5)

## Seasonal patterns for sole
#----------------------------
source("r/sole_season_patt.R")

## Abundance-weighted average season
#-----------------------------------
source("r/ab_weigth_av.R")

## Maximum and Minimum of PC - Month vs. Year
#---------------------------------------------
source("r/pc_min_max.R")

## Analyse with covariates
#-------------------------
source("r/cov_vs_pc.R")