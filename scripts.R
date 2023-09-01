############################
# Codes for the C4 EOF repos
############################

## Load libraries and functions
source("r/library_functions.R")

## Code configurations
source("r/config.R")

## Processing of the model outputs for figures
#---------------------------------------------
source("r/process_model_outputs.R")

##----------------------------------------------------------------------------------------------------
##----------------------------------- Make plots/analysis --------------------------------------------
##----------------------------------------------------------------------------------------------------

load("res/S_x_list.RData")
load("res/EOF_maps.RData")
load("res/EOF_PC.RData")
load("res/PC_list.RData")
load("res/eigen_values.RData")
load("res/Zt_list.RData")
load("res/V_list.RData")
load("res/E_list.RData")
load("res/loc_x_list.RData")
load("res/cov_list.RData")


## Percentage of variance
#------------------------
source("r/perc_var.R")

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

## Clustering analysis
#---------------------
## Sole
source("r/sole_clust.R")

## Hake - BoB
source("r/hake_bob_clust.R")

## Hake - CS
source("r/hake_cs_clust.R")

## Sea bass
source("r/seabass_clust.R")

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