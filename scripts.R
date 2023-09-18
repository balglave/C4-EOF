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
cov_ts_df <- cov_list[[1]] %>%
  group_by(Year,Month) %>% 
  dplyr::summarise(bottomT = mean(bottomT,na.rm = T),
                   SST = mean(thetao,na.rm = T),
                   # chl = mean(chl,na.rm = T),
                   # so = mean(so,na.rm=T),
                   # o2 = mean(o2,na.rm=T)
                   )  %>% 
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
Expected_repro_df$Expected_repro_sole_optim <- NA
Expected_repro_df$Expected_repro_hake_optim <- NA
Expected_repro_df$Expected_repro_seabass_optim <- NA

for(i in 1:length(step_vec)){
  
  # Criteria for being optimal temperature 
  ## Sole
  condition_sole_range_std <- Expected_repro_df$SST[i] > opt_temp_df[Sole,"min_optim_T"] & Expected_repro_df$SST[i] < opt_temp_df[Sole,"max_optim_T"]
  if(i>1){
    condition_sole_range_down <- Expected_repro_df$SST[i-1] > opt_temp_df[Sole,"max_optim_T"] & Expected_repro_df$SST[i] < opt_temp_df[Sole,"min_optim_T"]
  }else{condition_sole_range_down <- F}
  
  if(i<length(step_vec)){
    condition_sole_range_up <- Expected_repro_df$SST[i+1] > opt_temp_df[Sole,"max_optim_T"] & Expected_repro_df$SST[i] < opt_temp_df[Sole,"min_optim_T"]
  }else{condition_sole_range_up <- F}
  if(condition_sole_range_std | condition_sole_range_down | condition_sole_range_up) Expected_repro_df$Expected_repro_sole_optim[i] <- 1
  
  ## Hake
  condition_hake_range_std <- Expected_repro_df$SST[i] > opt_temp_df[Hake,"min_optim_T"] & Expected_repro_df$SST[i] < opt_temp_df[Hake,"max_optim_T"]
  if(i>1){
    condition_hake_range_down <- Expected_repro_df$SST[i-1] > opt_temp_df[Hake,"max_optim_T"] & Expected_repro_df$SST[i] < opt_temp_df[Hake,"min_optim_T"]
  }else{condition_hake_range_down <- F}
  if(i<length(step_vec)){
    condition_hake_range_up <- Expected_repro_df$SST[i+1] > opt_temp_df[Hake,"max_optim_T"] & Expected_repro_df$SST[i] < opt_temp_df[Hake,"min_optim_T"]
  }else{condition_hake_range_up <- F}
  if(condition_hake_range_std | condition_hake_range_down | condition_hake_range_up) Expected_repro_df$Expected_repro_hake_optim[i] <- 1
  
  ## Seabass
  condition_seabass_range_std <- Expected_repro_df$SST[i] > opt_temp_df[Seabass,"min_optim_T"] & Expected_repro_df$SST[i] < opt_temp_df[Seabass,"max_optim_T"]
  if(i>1){
    condition_seabass_range_down <- Expected_repro_df$SST[i-1] > opt_temp_df[Seabass,"max_optim_T"] & Expected_repro_df$SST[i] < opt_temp_df[Seabass,"min_optim_T"]
  }else{condition_seabass_range_down<-F}
  if(i<length(step_vec)){
    condition_seabass_range_up <- Expected_repro_df$SST[i+1] > opt_temp_df[Seabass,"max_optim_T"] & Expected_repro_df$SST[i] < opt_temp_df[Seabass,"min_optim_T"]
  }else{condition_seabass_range_up <- F}
  if(condition_seabass_range_std | condition_seabass_range_down | condition_seabass_range_up) Expected_repro_df$Expected_repro_seabass_optim[i] <- 1
  
}

# ggplot()+
#   geom_point(data=Expected_repro_df,aes(x=Year_Month,y=so),col="red")+
#   geom_vline(xintercept=Expected_repro_df$Year_Month[which(str_detect(Expected_repro_df$Year_Month,"_01"))],
#              linetype="dashed", color = "skyblue", linewidth = 1)+
#   geom_point(data=Expected_repro_df,aes(x=Year_Month,y=SST))+
#   geom_point(data=Expected_repro_df[which(Expected_repro_df$Expected_repro_sole_optim == 1),],
#              aes(x=Year_Month,y=SST),col="red")+
#   geom_hline(yintercept = 10) +
#   geom_hline(yintercept = 11)

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