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
load("res/minmax_PC_df.RData")

## Percentage of variance
#------------------------
source("r/perc_var.R")

## Prepare covariate dataframe to compare with EOF
#------------------------------------------------
cov_ts_df <- cov_list[[1]] %>%
  group_by(Year,Month) %>% 
  dplyr::summarise(bottomT = mean(bottomT,na.rm = T),
                   SST = mean(thetao,na.rm = T)) %>% 
  mutate(Year_Month = paste0(Year,"_",ifelse(Month < 10,paste0("0",Month),Month)))

Expected_repro_df <- cov_ts_df

## Range of temperature for reproduction
# Sole
Expected_repro_df$Expected_repro_sole = NA
Expected_repro_df$Expected_repro_sole[which(Expected_repro_df$SST > opt_temp_df[Sole,"min_range_T"] & Expected_repro_df$SST < opt_temp_df[Sole,"max_range_T"])] = 1

# Hake
Expected_repro_df$Expected_repro_hake = NA
Expected_repro_df$Expected_repro_hake[which(Expected_repro_df$SST > opt_temp_df[Hake,"min_range_T"] & Expected_repro_df$SST < opt_temp_df[Hake,"max_range_T"])] = 1

# Seabass
Expected_repro_df$Expected_repro_seabass = NA
Expected_repro_df$Expected_repro_seabass[which(Expected_repro_df$SST > opt_temp_df[Seabass,"min_range_T"] & Expected_repro_df$SST < opt_temp_df[Seabass,"max_range_T"])] = 1

## Optimal temperature for reproduction
step_vec <- Expected_repro_df$Year_Month

Sole = 1
Hake = 2
Sebass = 3

for(i in 1:length(step_vec)){
  
  # Criteria for being optimal temperature 
  ## Sole
  condition_sole_range_std <- Expected_repro_df$SST[i] > opt_temp_df[Sole,"min_optim_T"] & Expected_repro_df$SST[i] < opt_temp_df[Sole,"max_optim_T"]
  condition_sole_range_down <- i>1 & Expected_repro_df$SST[i-1] > opt_temp_df[Sole,"max_optim_T"] & Expected_repro_df$SST[i] < opt_temp_df[Sole,"min_optim_T"]
  condition_sole_range_up <- i<length(step_vec) & Expected_repro_df$SST[i+1] > opt_temp_df[Sole,"max_optim_T"] & Expected_repro_df$SST[i] < opt_temp_df[Sole,"min_optim_T"]
  if(condition_sole_range_std | condition_sole_range_down | condition_sole_range_up) Expected_repro_df$Expected_repro_hake_optim[i] <- NA
  
  
  
  Expected_repro_df$Expected_repro_hake_optim[i]
  
}


Expected_repro_df$Expected_repro_hake_optim[which(Expected_repro_df$SST > 10 & Expected_repro_df$SST < 12.5)] = 1


# ggplot(Expected_repro_df)+
#   geom_point(aes(x=Year_Month,y=SST))+
#   geom_hline(yintercept = 13) +
#   geom_hline(yintercept = 15)

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