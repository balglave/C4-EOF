###########################################################################
# -------------------------------------------------------------------------
# EXPLORATION PLOTS
# -------------------------------------------------------------------------
###########################################################################

library(dplyr)
library(tidyr)
library(ggplot2)


# 0- load outputs generated by Baptiste
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

load( "res/S_x_list.RData")
load( "res/EOF_maps.RData")
load( "res/EOF_PC.RData")
load( "res/eigen_values.RData")
load( "res/Zt_list.RData")
load("res/E_list.RData")
load("res/loc_x_list.RData")
load( "res/cov_list.RData")

# 1- Does Spawning phenology change during the time serie ?
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# -- Maximum and Minimum of PC - Month vs. Year
#---------------------------------------------------
load("res/minmax_PC_df.RData")

# --- Minimum for Sole PC1 / Hake PC1 during winter


month_name_winter = c("September(t)",
               "October(t)",
               "November(t)",
               "December(t)",
               "January(t+1)",
               "February(t+1)",
               "March(t+1)",
               "April(t+1)",
               "May(t+1)",
               "June(t+1)",
               "July(t+1)",
               "August(t+1)" )

month_name = c("January",
               "February",
               "March",
               "April",
               "May",
               "June",
               "July",
               "August",
               "September",
               "October",
               "November",
               "December")               

# --- Minimum for Sole PC1 / Hake PC1 during winter
head( minmax_PC_df)
unique(minmax_PC_df$Year)
minmax_PC_df_sole_hakebob <- minmax_PC_df %>% filter(PC==1,species %in% c("Hake_BoB","Sole","Seabass"),)

minmax_PC_df_sole_hakebob  %>% filter(species== "Seabass")

minmax_PC_df_sole_hakebob$Month = NA
minmax_PC_df_sole_hakebob$Month[which(minmax_PC_df_sole_hakebob$Month_min == 1)] = month_name_winter[5]
minmax_PC_df_sole_hakebob$Month[which(minmax_PC_df_sole_hakebob$Month_min == 2)] = month_name_winter[6]
minmax_PC_df_sole_hakebob$Month[which(minmax_PC_df_sole_hakebob$Month_min == 3)] = month_name_winter[7]
minmax_PC_df_sole_hakebob$Month[which(minmax_PC_df_sole_hakebob$Month_min == 4)] = month_name_winter[8]
minmax_PC_df_sole_hakebob$Month[which(minmax_PC_df_sole_hakebob$Month_min == 5)] = month_name_winter[9]
minmax_PC_df_sole_hakebob$Month[which(minmax_PC_df_sole_hakebob$Month_min == 6)] = month_name_winter[10]
minmax_PC_df_sole_hakebob$Month[which(minmax_PC_df_sole_hakebob$Month_min == 7)] = month_name_winter[11]
minmax_PC_df_sole_hakebob$Month[which(minmax_PC_df_sole_hakebob$Month_min == 8)] = month_name_winter[12]
minmax_PC_df_sole_hakebob$Month[which(minmax_PC_df_sole_hakebob$Month_min == 9)] = month_name_winter[1]
minmax_PC_df_sole_hakebob$Month[which(minmax_PC_df_sole_hakebob$Month_min == 10)] = month_name_winter[2]
minmax_PC_df_sole_hakebob$Month[which(minmax_PC_df_sole_hakebob$Month_min == 11)] = month_name_winter[3]
minmax_PC_df_sole_hakebob$Month[which(minmax_PC_df_sole_hakebob$Month_min == 12)] = month_name_winter[4]

# --- Maximum for Seabass PC1 during winter

minmax_PC_df_seabass <- minmax_PC_df %>% filter(PC==1,species %in% c("Seabass"))
head(minmax_PC_df_seabass)
minmax_PC_df_seabass$Month = NA
minmax_PC_df_seabass$Month[which(minmax_PC_df_seabass$Month_max == 1)] = month_name_winter[5]
minmax_PC_df_seabass$Month[which(minmax_PC_df_seabass$Month_max == 2)] = month_name_winter[6]
minmax_PC_df_seabass$Month[which(minmax_PC_df_seabass$Month_max == 3)] = month_name_winter[7]
minmax_PC_df_seabass$Month[which(minmax_PC_df_seabass$Month_max == 4)] = month_name_winter[8]
minmax_PC_df_seabass$Month[which(minmax_PC_df_seabass$Month_max == 5)] = month_name_winter[9]
minmax_PC_df_seabass$Month[which(minmax_PC_df_seabass$Month_max == 6)] = month_name_winter[10]
minmax_PC_df_seabass$Month[which(minmax_PC_df_seabass$Month_max == 7)] = month_name_winter[11]
minmax_PC_df_seabass$Month[which(minmax_PC_df_seabass$Month_max == 8)] = month_name_winter[12]
minmax_PC_df_seabass$Month[which(minmax_PC_df_seabass$Month_max == 9)] = month_name_winter[1]
minmax_PC_df_seabass$Month[which(minmax_PC_df_seabass$Month_max == 10)] = month_name_winter[2]
minmax_PC_df_seabass$Month[which(minmax_PC_df_seabass$Month_max == 11)] = month_name_winter[3]
minmax_PC_df_seabass$Month[which(minmax_PC_df_seabass$Month_max == 12)] = month_name_winter[4]


# -- Represent the Y during winter months
#---------------------------------------------------

head(minmax_PC_df)
Year_winter <- c("2007/2008","2008/2009","2009/2010","2010/2011","2011/2012","2012/2013","2013/2014",
            "2014/2015","2015/2016","2016/2017","2017/2018")
Year <- c(unique(minmax_PC_df$Year))
Year_table <- as.data.frame(cbind(Year_winter,Year))

# -- Merge the tibbles
#minmax_PC_df_sole_seabass <- bind_rows(minmax_PC_df_seabass,minmax_PC_df_sole_hakebob)
minmax_PC_df_sole_seabass <-minmax_PC_df_sole_hakebob
head(minmax_PC_df_sole_seabass)
minmax_PC_df_sole_seabass$Year <- as.numeric(minmax_PC_df_sole_seabass$Year) 

Year_table$Year <- as.numeric(Year_table$Year) 
minmax_PC_df_sole_seabass <- left_join(minmax_PC_df_sole_seabass,Year_table)
head(as_tibble(minmax_PC_df_sole_seabass))

minmax_PC_df_sole_seabass$Month = factor(minmax_PC_df_sole_seabass$Month,levels = (month_name_winter))
minmax_PC_df_sole_seabass$Year_winter = as.character(minmax_PC_df_sole_seabass$Year_winter)

minmax_PC_df_sole_seabass$species = factor(minmax_PC_df_sole_seabass$species,levels = c("Sole","Hake_BoB","Hake_CS","Seabass"))
head(minmax_PC_df_sole_seabass)

minmax_PC_df_sole_seabass_2 = minmax_PC_df_sole_seabass %>% 
  filter(PC %in% c(1))


min_month_plot = ggplot(minmax_PC_df_sole_seabass_2)+
  geom_point(aes(y=Month,x=Year_winter))+
  geom_line(aes(y=Month,x=Year_winter,group=1))+
  facet_grid(rows = vars(species))+
 # ggtitle("Minimum principal component value")+
  theme_bw()+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust = 0.5,face = "bold"),
        axis.text.x = element_text(angle = 90,vjust = 0.5))+
  ylab("")+xlab("")


ggsave(paste0("images/minmax_PC_plot.png"),
       width=10,height=6)


## Analyse with covariates
#-------------------------
load("res/cov_list.RData")
  loc_x_pred <- S_x_df %>%
    dplyr::select(cell,x,y) %>%
    group_by(cell,x,y) %>%
    slice(1) %>%
    arrange(cell) %>%
    as.data.frame


# Filtering threshold
filt_thresh = 1/nrow(loc_x_pred)

# Join EOF maps, covariate data frame, latent field dataframe, PC coefficients
PC_cov_list = list()
for(i in 1:4){
  
  EOF_cov = inner_join(EOF_maps[[i]],cov_list[[i]],multiple = "all")
  EOF_cov_S_x = inner_join(EOF_cov,S_x_list[[i]])
  PC_cov = EOF_cov_S_x %>%
    filter(ContribVar > filt_thresh) # I only keep in the analysis the data points that corresponds to strong spatial patterns (i.e. the one that strongly contribute to EOF maps)
  
  # Aggregate over time step (either standard mean or weithed mean - weights are given by biomass field values)
  PC_cov_standmean = PC_cov %>% 
    group_by(Year_Month) %>% 
    dplyr::summarise(so = mean(so,na.rm=T),
                     bottomT = mean(bottomT,na.rm=T),
                     chl = mean(chl,na.rm=T),
                     thetao = mean(thetao,na.rm=T),
                     o2 = mean(o2,na.rm=T))
  
  PC_cov_weightmean = PC_cov %>% 
    group_by(Year_Month) %>% 
    dplyr::summarise(so_pond = weighted.mean(so,S_x,na.rm=T),
                     bottomT_pond = weighted.mean(bottomT,S_x,na.rm=T),
                     chl_pond = weighted.mean(chl,S_x,na.rm=T),
                     thetao_pond = weighted.mean(thetao,S_x,na.rm=T),
                     o2_pond = weighted.mean(o2,S_x,na.rm=T))
  
  PC_cov = data.frame(PC_cov_standmean,
                      so_pond = PC_cov_weightmean$so_pond,
                      bottomT_pond = PC_cov_weightmean$bottomT_pond,
                      chl_pond = PC_cov_weightmean$chl_pond,
                      thetao_pond = PC_cov_weightmean$thetao_pond,
                      o2_pond = PC_cov_weightmean$o2_pond
                      ) %>% 
    inner_join(EOF_PC[[i]],multiple = "all")
    
  PC_cov_list[[i]] = PC_cov

}
n_EOF_2 = c(2,2,2,1)
cor_cov_PC_df = NULL
plot_corr = T
for(i in 1:4){
  
  for(j in 1:n_EOF[i]){
    
    to_plot = PC_cov_list[[1]] %>% 
      filter(PC == paste0("PC",1))

    x11()
    par(mfrow = c(2,5))
        
    for(var=c("bottomT")){
      
      index_i = which(!is.na(to_plot[,var]))
      PC_to_plot = to_plot$value[index_i]
      var_to_plot = unlist(to_plot[,var])[index_i]
      
      PC_to_plot_2 = (PC_to_plot - mean(PC_to_plot))/sd(PC_to_plot) 
      var_to_plot_2 = (var_to_plot - mean(var_to_plot))/sd(var_to_plot) 
      
      test_cor_spearman = cor.test(PC_to_plot_2,var_to_plot_2,method = "spearman")
      test_cor_pearson = cor.test(PC_to_plot_2,var_to_plot_2,method = "pearson")
      
      if(is.null(cor_cov_PC_df)){
        
        cor_cov_PC_df = data.frame(species = species_name[i],
                                   dimPC = j,
                                   covar = var,
                                   Rspearman = test_cor_spearman$estimate,
                                   p_val_spearman = test_cor_spearman$p.value,
                                   Rpearson = test_cor_pearson$estimate,
                                   p_val_pearson = test_cor_pearson$p.value
                                   )
        
      }else{
        
        test = data.frame(species = species_name[i],
                          dimPC = j,
                          covar = var,
                          Rspearman = test_cor_spearman$estimate,
                          p_val_spearman = test_cor_spearman$p.value,
                          Rpearson = test_cor_pearson$estimate,
                          p_val_pearson = test_cor_pearson$p.value
        )
        
        cor_cov_PC_df = rbind(test,cor_cov_PC_df)
        
      }
      
      if(plot_corr == T){
        
        plot(PC_to_plot_2,var_to_plot_2,main = paste("PC",j," vs. ",var))
        mtext(paste0("R = ",
                     round(test_cor_pearson$estimate,digits = 3),
                     " p-val = ",
                     round(test_cor_pearson$p.value,digits = 3),
                     side=3))
        
      }
    }
  }
}
 
plot(var_to_plot_2,col="red",ylim = c(-3,3))
lines(var_to_plot_2,col="red")
lines(-PC_to_plot_2)
