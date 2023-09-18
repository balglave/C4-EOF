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