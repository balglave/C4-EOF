
load( "res/S_x_list.RData")
load( "res/EOF_maps.RData")
load( "res/EOF_PC.RData")
load( "res/eigen_values.RData")
load( "res/Zt_list.RData")
load( "res/E_list.RData")
load( "res/loc_x_list.RData")
load( "res/cov_list.RData")
load( "res/loc_x_pred.RData")

##----------------------------------------------------------------------------------------------------
##----------------------------------- Make plots/analysis --------------------------------------------
##----------------------------------------------------------------------------------------------------

#- Preliminary stuff
#------------------------------

library(cowplot)
library(colorspace)
library(FactoMineR)
library(raster)
library(rnaturalearth)
library(RNetCDF)
library(sf)
library(tidyverse)
library(patchwork)

ggplotColours <- function(n = 6, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

# Map
mapBase <- ne_countries(scale = "medium", returnclass = "sf")
grid_projection <- "+proj=longlat +datum=WGS84"

# 1st Merluccius_merluccius: BoB, 2nd Merluccius_merluccius: CS
main_c <- c("Solea_solea","Merluccius_merluccius","Merluccius_merluccius","Dicentrarchus_labrax")
species_name <- c("Sole","Hake_BoB","Hake_CS","Seabass")
raw_biomass_spp <- c("Dicentrarchus_labrax","Merluccius_merluccius") # for these species, S_x_df are in raw biomass, other are in relative biomass.
raw_biom <- T # conduct analysis on raw biomass
standardize <- "none" # standardize over time
n_EOF <- c(6,2,2,1) # number of EOF to look at for each species

# Covariates of interests
copernicus.var <- c("so","bottomT","chl","thetao","o2") # "so": salinity, "chl": chlorophyll A, "thetao": SST
# c("so","bottomT","thetao","fe","nppv","nh4","si","phyc","po4","no3","zeu","chl","o2")

extract_covariates <- F # Extract covariates


## Plot EOF maps and time series
#-------------------------------

# -- Represent the Y during winter months
#---------------------------------------------------

Year <- unique(S_x_list[4][[1]]$Year)
Year_winter <- c("2007/2008","2008/2009","2009/2010","2010/2011","2011/2012","2012/2013","2013/2014",
            "2014/2015","2015/2016","2016/2017","2017/2018")
Year_winter_1 <- c("2008/2009","2009/2010","2010/2011","2011/2012","2012/2013","2013/2014",
            "2014/2015","2015/2016","2016/2017","2017/2018","2018/2019")

Year_table <- as.data.frame(cbind(Year_winter,Year))
Year_table$Year <- as.integer(Year_table$Year)

Year_table_1 <- as.data.frame(cbind(Year_winter_1,Year))
Year_table_1$Year <- as.integer(Year_table_1$Year)



## Sole
#------
i=1
xlims <- range(pretty(EOF_maps[[i]]$x))
ylims <- range(pretty(EOF_maps[[i]]$y))

EOF_maps_spp <- EOF_maps[[i]] %>%
  filter(EOF %in% paste0("EOF",c(1:n_EOF[i])))
EOF_PC_spp <- EOF_PC[[i]] %>%
  filter(PC %in% paste0("PC",c(1:n_EOF[i])))

color_name <- ggplotColours(n = n_EOF[i])
list_EOF_map_sole <- list()
list_EOF_PC_sole <- list()
minmax_PC_df = NULL

for(j in 1:n_EOF[i]){
  EOF_maps_spp_2 <- EOF_maps_spp %>% filter(EOF == paste0("EOF",j))
  
  EOF_map_plot <- ggplot()+
    geom_point(data=EOF_maps_spp_2[which(abs(EOF_maps_spp_2$ContribVar) >= 1/nrow(loc_x_pred)),], # Only plot the points that most contribute to the dimension
               aes(x = x, y = y, col = value),size = 0.75,shape=16)+
    geom_point(data=EOF_maps_spp_2[which(abs(EOF_maps_spp_2$ContribVar) < 1/nrow(loc_x_pred)),],
               aes(x = x, y = y, col = value),size = 0.5,shape=16,alpha=0.2)+
    scale_color_continuous_divergingx(palette = "Spectral", mid = 0,rev = T)+ 
    theme_bw()+
    xlab("") + ylab("")+
    geom_sf(data = mapBase) +
    coord_sf(xlim = xlims, ylim = ylims, expand = FALSE)+
    theme(legend.title = element_blank(),
          panel.background = element_rect(fill = "white"),
          axis.text.x = element_text(angle = 90))+
    facet_wrap(.~EOF)
  list_EOF_map_sole[[j]] <- EOF_map_plot
  
  EOF_PC_spp_2 <- EOF_PC_spp %>% filter(PC == paste0("PC",j))
  EOF_time_series_plot <- ggplot(EOF_PC_spp_2,
                                 aes(x=Year_Month,y=value,group=PC))+
    geom_vline(xintercept=EOF_PC_spp_2$Year_Month[which(str_detect(EOF_PC_spp_2$Year_Month,"_01"))],
               linetype="dashed", color = "skyblue", linewidth = 1)+
    geom_hline(yintercept=0,linetype="dashed", color = "grey", linewidth = 1)+
    geom_line(linewidth=1,color=color_name[j])+
    theme_bw()+
    xlab("")+ylab("")+
    theme()+
    facet_wrap(.~PC,nrow = n_EOF[i]/2)+
    theme(axis.text.x = element_blank(),
          legend.position = "none",
          aspect.ratio = c(1/4),
          plot.margin = margin(2, 2, 2, 2, unit = "pt"),
          plot.title = element_text(hjust=0.5,face = "bold"))+
    geom_text(aes(x = -Inf, y = -Inf, label = "2008"),col= "black", hjust = -.10, vjust = 2)+
    geom_text(aes(x = max(Year_Month),
                  y = -Inf, label = "2018"),
              col= "black", hjust = 1, vjust = 2)+
    coord_cartesian(clip = "off")
  list_EOF_PC_sole[[j]] <- EOF_time_series_plot
  

  EOF_PC_spp_2_1 <- left_join(EOF_PC_spp_2 %>% filter(Month %in% c(1,2,3)),as_tibble(Year_table))
  EOF_PC_spp_2_2 <- left_join(EOF_PC_spp_2 %>% filter(Month %in% c(9,10,11,12)),as_tibble(Year_table_1))
  EOF_PC_spp_2_2 <- EOF_PC_spp_2_2  %>% mutate(Year_winter=Year_winter_1)
  EOF_PC_spp_3 <- bind_rows(EOF_PC_spp_2_1,EOF_PC_spp_2_2)
  EOF_PC_spp_4 <-EOF_PC_spp_3 %>% mutate(value=if_else( PC=="PC2", value ,-value))  

  for(year in 2008:2018){
    toto1 = EOF_PC_spp_4 %>% filter( (Year %in% c(year)) & (Month %in% c(1,2,3))) 
    toto2 = EOF_PC_spp_4 %>% 
    filter((Year %in% c(year-1))&(Month %in% c(9,10,11,12)))
    
    toto <- bind_rows(toto2,toto1)
    temp <- toto  %>% summarise(value=max(value))
    temp2 <- left_join(temp, toto[,c("Year_winter","value","Month","PC")])
   
   
    if(is.null(minmax_PC_df)){
     minmax_PC_df = temp2  %>% mutate(PC = j,species = species_name[i])
    }else{
      titi = temp2  %>% mutate(PC = j,species = species_name[i])
      minmax_PC_df = rbind(minmax_PC_df,titi)
    }
    
  }

}

EOF_sole <- plot_grid(list_EOF_PC_sole[[1]],list_EOF_map_sole[[1]],
                      list_EOF_PC_sole[[2]],list_EOF_map_sole[[2]],
                      list_EOF_PC_sole[[3]],list_EOF_map_sole[[3]],
                      list_EOF_PC_sole[[4]],list_EOF_map_sole[[4]],
                      list_EOF_PC_sole[[5]],list_EOF_map_sole[[5]],
                      list_EOF_PC_sole[[6]],list_EOF_map_sole[[6]],
                      nrow = n_EOF[i]/2,align = "v",
                      rel_widths = c(1.05,0.5,1.05,0.5))

ggsave(filename = "images/Solea_solea/EOF_map_plot.png",width = 30/1.5,height = 15/1.5)


EOF_sole_pres <- plot_grid(list_EOF_PC_sole[[1]],list_EOF_map_sole[[1]],
                           list_EOF_PC_sole[[2]],list_EOF_map_sole[[2]],
                           nrow = 2,align = "v",
                           rel_widths = c(1.05,0.5))

ggsave(filename = "images/Solea_solea/pres_EOF_map_plot.png",width = 30/(1.5*2),height = 15/(1.5 * 2) )


## Hake - BoB
#------------
i=2
xlims <- range(pretty(EOF_maps[[i]]$x))
ylims <- range(pretty(EOF_maps[[i]]$y))

EOF_maps_spp <- EOF_maps[[i]] %>%
  filter(EOF %in% paste0("EOF",c(1:n_EOF[i])))
EOF_PC_spp <- EOF_PC[[i]] %>%
  filter(PC %in% paste0("PC",c(1:n_EOF[i])))

color_name <- ggplotColours(n = n_EOF[i])
list_EOF_map_hake_bob <- list()
list_EOF_PC_hake_bob <- list()

for(j in 1:n_EOF[i]){
  EOF_maps_spp_2 <- EOF_maps_spp %>% filter(EOF == paste0("EOF",j))
  EOF_map_plot <- ggplot()+
    geom_point(data=EOF_maps_spp_2[which(abs(EOF_maps_spp_2$ContribVar) >= 1/nrow(loc_x_pred)),],  # Only plot the points that most contribute to the dimension
               aes(x = x, y = y, col = value),size = 0.75,shape=16)+
    geom_point(data=EOF_maps_spp_2[which(abs(EOF_maps_spp_2$ContribVar) < 1/nrow(loc_x_pred)),],
               aes(x = x, y = y, col = value),size = 0.5,shape=16,alpha=0.2)+
    scale_color_continuous_divergingx(palette = "Spectral", mid = 0,rev = T)+ 
    theme_bw()+
    xlab("") + ylab("")+
    geom_sf(data = mapBase) +
    coord_sf(xlim = xlims, ylim = ylims, expand = FALSE)+
    theme(legend.title = element_blank(),
          panel.background = element_rect(fill = "white"),
          axis.text.x = element_text(angle = 90))+
    facet_wrap(.~EOF)
  
  list_EOF_map_hake_bob[[j]] <- EOF_map_plot
  
  EOF_PC_spp_2 <- EOF_PC_spp %>% filter(PC == paste0("PC",j))
  EOF_time_series_plot <- ggplot(EOF_PC_spp_2,
                                 aes(x=Year_Month,y=value,group=PC))+
    geom_vline(xintercept=EOF_PC_spp_2$Year_Month[which(str_detect(EOF_PC_spp_2$Year_Month,"_01"))],
               linetype="dashed", color = "skyblue", linewidth = 1)+
    geom_hline(yintercept=0,linetype="dashed", color = "grey", linewidth = 1)+
    geom_line(linewidth=1,color=color_name[j])+
    theme_bw()+
    xlab("")+ylab("")+
    theme()+
    facet_wrap(.~PC,nrow = n_EOF[i]/2)+
    theme(axis.text.x = element_blank(),
          legend.position = "none",
          aspect.ratio = c(1/4),
          plot.margin = margin(2, 2, 2, 2, unit = "pt"),
          plot.title = element_text(hjust=0.5,face = "bold"))+
    geom_text(aes(x = -Inf, y = -Inf, label = "2008"),col= "black", hjust = -.10, vjust = 2)+
    geom_text(aes(x = max(Year_Month),
                  y = -Inf, label = "2018"),
              col= "black", hjust = 1, vjust = 2)+
    coord_cartesian(clip = "off")
  list_EOF_PC_hake_bob[[j]] <- EOF_time_series_plot
  
 
  EOF_PC_spp_2_1 <- left_join(EOF_PC_spp_2 %>% filter(Month %in% c(1,2,3)),as_tibble(Year_table))
  EOF_PC_spp_2_2 <- left_join(EOF_PC_spp_2 %>% filter(Month %in% c(9,10,11,12)),as_tibble(Year_table_1))
  EOF_PC_spp_2_2 <- EOF_PC_spp_2_2  %>% mutate(Year_winter=Year_winter_1)
  EOF_PC_spp_3 <- bind_rows(EOF_PC_spp_2_1,EOF_PC_spp_2_2)
  EOF_PC_spp_4 <-EOF_PC_spp_3 %>% mutate(value=-value)  
 


  for(year in 2008:2018){
    toto1 = EOF_PC_spp_4 %>% filter( (Year %in% c(year)) & (Month %in% c(1,2,3))) 
    toto2 = EOF_PC_spp_4 %>% 
    filter((Year %in% c(year-1))&(Month %in% c(9,10,11,12)))
    
    toto <- bind_rows(toto2,toto1)
    temp <- toto  %>% summarise(value=max(value))
    temp2 <- left_join(temp, toto[,c("Year_winter","value","Month","PC")])
   
    if(is.null(minmax_PC_df)){
     minmax_PC_df = temp2  %>% mutate(PC = j,species = species_name[i])
    }else{
      titi = temp2  %>% mutate(PC = j,species = species_name[i])
      minmax_PC_df = rbind(minmax_PC_df,titi)
    }
    
  }
  
}

EOF_hake_bob <- plot_grid(list_EOF_PC_hake_bob[[1]],list_EOF_map_hake_bob[[1]],
                          list_EOF_PC_hake_bob[[2]],list_EOF_map_hake_bob[[2]],
                          nrow = 2,align = "v",
                          rel_widths = c(1.05,0.5))

ggsave(filename = "images/Merluccius_merluccius_bob/EOF_map_plot.png",width = 30/(1.5*2),height = 15/(1.5 * 2) )

## Hake - CS
#------------
i=3
xlims <- range(pretty(EOF_maps[[i]]$x))
ylims <- range(pretty(EOF_maps[[i]]$y))

EOF_maps_spp <- EOF_maps[[i]] %>%
  filter(EOF %in% paste0("EOF",c(1:n_EOF[i])))
EOF_PC_spp <- EOF_PC[[i]] %>%
  filter(PC %in% paste0("PC",c(1:n_EOF[i])))

color_name <- ggplotColours(n = n_EOF[i])
list_EOF_map_hake_cs <- list()
list_EOF_PC_hake_cs <- list()
for(j in 1:n_EOF[i]){

  EOF_maps_spp_2 <- EOF_maps_spp %>% filter(EOF == paste0("EOF",j))
  EOF_map_plot <- ggplot()+
    geom_point(data=EOF_maps_spp_2[which(abs(EOF_maps_spp_2$ContribVar) >= 1/nrow(loc_x_pred)),],  # Only plot the points that most contribute to the dimension
               aes(x = x, y = y, col = value),size = 0.75,shape=16)+
    geom_point(data=EOF_maps_spp_2[which(abs(EOF_maps_spp_2$ContribVar) < 1/nrow(loc_x_pred)),],
               aes(x = x, y = y, col = value),size = 0.5,shape=16,alpha=0.2)+
    scale_color_continuous_divergingx(palette = "Spectral", mid = 0,rev = T)+ 
    theme_bw()+
    xlab("") + ylab("")+
    geom_sf(data = mapBase) +
    coord_sf(xlim = xlims, ylim = ylims, expand = FALSE)+
    theme(legend.title = element_blank(),
          panel.background = element_rect(fill = "white"),
          axis.text.x = element_text(angle = 90))+
    facet_wrap(.~EOF)
  
  list_EOF_map_hake_cs[[j]] <- EOF_map_plot
  
  EOF_PC_spp_2 <- EOF_PC_spp %>% filter(PC == paste0("PC",j))
  EOF_time_series_plot <- ggplot(EOF_PC_spp_2,
                                 aes(x=Year_Month,y=value,group=PC))+
    geom_vline(xintercept=EOF_PC_spp_2$Year_Month[which(str_detect(EOF_PC_spp_2$Year_Month,"_01"))],
               linetype="dashed", color = "skyblue", linewidth = 1)+
    geom_hline(yintercept=0,linetype="dashed", color = "grey", linewidth = 1)+
    geom_line(linewidth=1,color=color_name[j])+
    theme_bw()+
    xlab("")+ylab("")+
    theme()+
    facet_wrap(.~PC,nrow = n_EOF[i]/2)+
    theme(axis.text.x = element_blank(),
          legend.position = "none",
          aspect.ratio = c(1/4),
          plot.margin = margin(2, 2, 2, 2, unit = "pt"),
          plot.title = element_text(hjust=0.5,face = "bold"))+
    geom_text(aes(x = -Inf, y = -Inf, label = "2008"),col= "black", hjust = -.10, vjust = 2)+
    geom_text(aes(x = max(Year_Month),
                  y = -Inf, label = "2018"),
              col= "black", hjust = 1, vjust = 2)+
    coord_cartesian(clip = "off")
  list_EOF_PC_hake_cs[[j]] <- EOF_time_series_plot
  
  EOF_PC_spp_2_1 <- left_join(EOF_PC_spp_2 ,as_tibble(Year_table))
  EOF_PC_spp_4 <-EOF_PC_spp_2_1 %>% mutate(value=-value)  
 


  for(year in 2008:2018){
    toto <- EOF_PC_spp_4  %>% filter(Year %in% c(year))
    temp <- toto  %>% summarise(value=max(value))
    temp2 <- left_join(temp, toto[,c("Year_winter","value","Month","PC")])
   
    if(is.null(minmax_PC_df)){
     minmax_PC_df = temp2  %>% mutate(PC = j,species = species_name[i])
    }else{
      titi = temp2  %>% mutate(PC = j,species = species_name[i])
      minmax_PC_df = rbind(minmax_PC_df,titi)
    }
    
  }
  
}
minmax_PC_df %>% filter(species=="Hake_CS")
EOF_hake_cs <- plot_grid(list_EOF_PC_hake_cs[[1]],list_EOF_map_hake_cs[[1]],
                         list_EOF_PC_hake_cs[[2]],list_EOF_map_hake_cs[[2]],
                         nrow = 2,align = "v",
                         rel_widths = c(1.05,0.5))

ggsave(filename = "images/Merluccius_merluccius_CS/EOF_map_plot.png",width = 30/(1.5*2),height = 15/(1.5 * 2) )



## Seabass
#---------
i=4
xlims <- range(pretty(EOF_maps[[i]]$x))
ylims <- range(pretty(EOF_maps[[i]]$y))

EOF_maps_spp <- EOF_maps[[i]] %>%
  filter(EOF %in% paste0("EOF",c(1:n_EOF[i])))
EOF_PC_spp <- EOF_PC[[i]] %>%
  filter(PC %in% paste0("PC",c(1:n_EOF[i])))

color_name <- ggplotColours(n = n_EOF[i])
list_EOF_map_seabass <- list()
list_EOF_PC_seabass <- list()

for(j in 1:n_EOF[i]){
  EOF_maps_spp_2 <- EOF_maps_spp %>% filter(EOF == paste0("EOF",j))
  EOF_map_plot <- ggplot()+
    geom_point(data=EOF_maps_spp_2[which(abs(EOF_maps_spp_2$ContribVar) >= 1/nrow(loc_x_pred)),], # Only plot the points that most contribute to the dimension
               aes(x = x, y = y, col = -value),size = 0.75,shape=16)+
    geom_point(data=EOF_maps_spp_2[which(abs(EOF_maps_spp_2$ContribVar) < 1/nrow(loc_x_pred)),],
               aes(x = x, y = y, col = -value),size = 0.5,shape=16,alpha=0.2)+
    scale_color_continuous_divergingx(palette = "Spectral", mid = 0,rev = T)+ 
    theme_bw()+
    xlab("") + ylab("")+
    geom_sf(data = mapBase) +
    coord_sf(xlim = xlims, ylim = ylims, expand = FALSE)+
    theme(legend.title = element_blank(),
          panel.background = element_rect(fill = "white"),
          axis.text.x = element_text(angle = 90))+
    facet_wrap(.~EOF)
  list_EOF_map_seabass[[j]] <- EOF_map_plot
  
  EOF_PC_spp_2 <- EOF_PC_spp %>% filter(PC == paste0("PC",j))
  EOF_time_series_plot <- ggplot(EOF_PC_spp_2,
                                 aes(x=Year_Month,y=-value,group=PC))+
    geom_vline(xintercept=EOF_PC_spp_2$Year_Month[which(str_detect(EOF_PC_spp_2$Year_Month,"_01"))],
               linetype="dashed", color = "skyblue", linewidth = 1)+
    geom_hline(yintercept=0,linetype="dashed", color = "grey", linewidth = 1)+
    geom_line(linewidth=1,color=color_name[j])+
    theme_bw()+
    xlab("")+ylab("")+
    theme()+
    facet_wrap(.~PC,nrow = 1)+
    theme(axis.text.x = element_blank(),
          legend.position = "none",
          aspect.ratio = c(1/4),
          plot.margin = margin(2, 2, 2, 2, unit = "pt"),
          plot.title = element_text(hjust=0.5,face = "bold"))+
    geom_text(aes(x = -Inf, y = -Inf, label = "2008"),col= "black", hjust = -.10, vjust = 2)+
    geom_text(aes(x = max(Year_Month),
                  y = -Inf, label = "2018"),
              col= "black", hjust = 1, vjust = 2)+
    coord_cartesian(clip = "off")
  list_EOF_PC_seabass[[j]] <- EOF_time_series_plot
  
  EOF_PC_spp_2_1 <- left_join(EOF_PC_spp_2 %>% filter(Month %in% c(1,2,3)),as_tibble(Year_table))
  EOF_PC_spp_2_2 <- left_join(EOF_PC_spp_2 %>% filter(Month %in% c(9,10,11,12)),as_tibble(Year_table_1))
  EOF_PC_spp_2_2 <- EOF_PC_spp_2_2  %>% mutate(Year_winter=Year_winter_1)
  EOF_PC_spp_3 <- bind_rows(EOF_PC_spp_2_1,EOF_PC_spp_2_2)
  EOF_PC_spp_4 <-EOF_PC_spp_3 %>% mutate(value=-value)  
 


  for(year in 2008:2018){
    toto1 = EOF_PC_spp_4 %>% filter( (Year %in% c(year)) & (Month %in% c(1,2,3))) 
    toto2 = EOF_PC_spp_4 %>% 
    filter((Year %in% c(year-1))&(Month %in% c(9,10,11,12)))
    
    toto <- bind_rows(toto2,toto1)
    temp <- toto  %>% summarise(value=max(value))
    temp2 <- left_join(temp, toto[,c("Year_winter","value","Month","PC")])
   
    if(is.null(minmax_PC_df)){
     minmax_PC_df = temp2  %>% mutate(PC = j,species = species_name[i])
    }else{
      titi = temp2  %>% mutate(PC = j,species = species_name[i])
      minmax_PC_df = rbind(minmax_PC_df,titi)
    }
    
  }
}

EOF_seabass <- plot_grid(list_EOF_PC_seabass[[1]],list_EOF_map_seabass[[1]],
                         nrow = n_EOF[i],align = "v",
                         rel_widths = c(1.05,0.5))

ggsave(filename = "images/Dicentrarchus_Labrax/EOF_map_plot.png",width = 30/(1.5*2),height = 15/(1.5 * 3))

EOF_seabass_pres <- plot_grid(list_EOF_PC_seabass[[1]],list_EOF_map_seabass[[1]],
                              NULL,NULL,
                              nrow = 2,align = "v",
                              rel_widths = c(1.05,0.5))

ggsave(filename = "images/Dicentrarchus_Labrax/pres_EOF_map_plot.png",width = 30/(1.5*2),height = 15/(1.5 * 2) )

# Reproduction occurs during winter months
#---------------------------------------------------
minmax_PC_df_temp1.a <- minmax_PC_df %>% filter(species=="Sole" & PC  %in% c(1,2)) 
minmax_PC_df_temp1.b <-  minmax_PC_df %>% filter(species %in% c("Hake_BoB","Seabass") , PC ==1)
minmax_PC_df_temp1 <- bind_rows(minmax_PC_df_temp1.a,minmax_PC_df_temp1.b)
minmax_PC_df_temp2 <- minmax_PC_df %>% filter(species %in% c("Hake_CS"),PC ==2)

minmax_PC_df_temp3 <- bind_rows(minmax_PC_df_temp1,minmax_PC_df_temp2)

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

minmax_PC_df_temp3$Months = NA
minmax_PC_df_temp3$Months[which(minmax_PC_df_temp3$Month == 1)] = month_name_winter[5]
minmax_PC_df_temp3$Months[which(minmax_PC_df_temp3$Month == 2)] = month_name_winter[6]
minmax_PC_df_temp3$Months[which(minmax_PC_df_temp3$Month == 3)] = month_name_winter[7]
minmax_PC_df_temp3$Months[which(minmax_PC_df_temp3$Month == 4)] = month_name_winter[8]
minmax_PC_df_temp3$Months[which(minmax_PC_df_temp3$Month == 5)] = month_name_winter[9]
minmax_PC_df_temp3$Months[which(minmax_PC_df_temp3$Month == 6)] = month_name_winter[10]
minmax_PC_df_temp3$Months[which(minmax_PC_df_temp3$Month == 7)] = month_name_winter[11]
minmax_PC_df_temp3$Months[which(minmax_PC_df_temp3$Month == 8)] = month_name_winter[12]
minmax_PC_df_temp3$Months[which(minmax_PC_df_temp3$Month == 9)] = month_name_winter[1]
minmax_PC_df_temp3$Months[which(minmax_PC_df_temp3$Month == 10)] = month_name_winter[2]
minmax_PC_df_temp3$Months[which(minmax_PC_df_temp3$Month == 11)] = month_name_winter[3]
minmax_PC_df_temp3$Months[which(minmax_PC_df_temp3$Month == 12)] = month_name_winter[4]

minmax_PC_df_temp3$Months = factor(minmax_PC_df_temp3$Months,levels = (month_name_winter))
minmax_PC_df_temp3$Year_winter = as.character(minmax_PC_df_temp3$Year_winter)

minmax_PC_df_temp3$species = factor(minmax_PC_df_temp3$species,levels = c("Sole","Hake_BoB","Hake_CS","Seabass"))



plot1 <- bind_rows(minmax_PC_df_temp3 %>% filter(species %in% c("Sole")), minmax_PC_df_temp3 %>% filter(species %in% c("Hake_BoB","Seabass"),PC==1))
minmax_PC_df_plot = ggplot(plot1)+
  geom_point(aes(y=Months,x=Year_winter))+
  geom_line(aes(y=Months,x=Year_winter,group=1))+
  facet_grid(rows = vars(species,PC))+
 # ggtitle("Minimum principal component value")+
  theme_bw()+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust = 0.5,face = "bold"),
        axis.text.x = element_text(angle = 90,vjust = 0.5))+
  ylab("")+xlab("")


# -- Represent th
ggsave(paste0("images/minmax_PC_plot.png"),
       width=10,height=6)


save(minmax_PC_df_temp3, file="res/minmax_PC_df_temp3.RData")

##############################################################################################
## Analyse with covariates
#-------------------------
##############################################################################################

load("res/cov_list.RData")

# Filtering threshold
filt_thresh = 1/nrow(loc_x_pred)
head(filt_thresh)
head(S_x)

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
    
    to_plot = PC_cov_list[[i]] %>% 
      filter(PC == paste0("PC",j))

    x11()
    par(mfrow = c(2,5))
        
    for(var in c("so","bottomT","chl","thetao","o2",
                 "so_pond","bottomT_pond","chl_pond",
                 "thetao_pond","o2_pond")){
      
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

plot(var_to_plot_2,PC_to_plot_2)

cor_cov_PC_df_2 = cor_cov_PC_df %>% 
  filter(p_val_pearson < 0.05 & dimPC < 3) %>% 
  mutate(pond = ifelse(str_detect(covar,"pond"),"pond","no_pond")) %>% 
  mutate(covar=str_remove(covar,"_pond"))


cor_cov_PC_df_2$species = factor(cor_cov_PC_df_2$species,levels = rev(unique(cor_cov_PC_df_2$species)))

cor_cov_PC_plot = ggplot(cor_cov_PC_df_2)+
  geom_point(aes(x=covar,y=Rpearson,col=pond))+
  facet_wrap(~species+dimPC,ncol = 2)+theme_bw()+
  geom_hline(yintercept = 0)+
  ylim(-1,1)+theme(aspect.ratio = 1)+
  xlab("")

ggsave(paste0("images/cor_cov_PC_plot.png"),
       width=6,height=12)

