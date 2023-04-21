############################
# Codes for the C4 EOF repos
############################

library(cowplot)
library(colorspace)
library(FactoMineR)
library(raster)
library(rnaturalearth)
library(RNetCDF)
library(sf)
library(tidyverse)

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
cov_file <- "C:/Users/test/Desktop/phd_projects/phd_full_codes/data/raw/Envir.Data/CopernicusData/" # used only if extract_covariate == T, need to have covariates on the computer


## list outputs
S_x_list <- list()
cov_list <- list()
Zt_list <- list()
V_list  <- list()
E_list <- list()
PC_list <- list()
EOF_maps <- list()
EOF_PC <- list()
eigen_values <- list()
loc_x_list <- list()
mean_patt <- list()

for(i in 1:length(main_c)){
  
  print(species_name[i])
  
  ##----------------------------------------------------------------------------------------------------
  ##----------------------------------- Shape model outputs --------------------------------------------
  ##----------------------------------------------------------------------------------------------------

  # Load data
  load(paste0("data/",main_c[i],"/S_x_df_",main_c[i],"_standardise.RData"))
  
  # Shape data
  # standardize species if model outputs are raw or relative biomass
  if(main_c[i] %in% raw_biomass_spp & raw_biom == F){
    
    Biomass_df <- S_x_df %>%
      group_by(year,month) %>%
      dplyr::summarise(Ab=sum(S_x))
    
    S_x_df <- inner_join(S_x_df,Biomass_df)
    if(!"cell" %in% colnames(S_x_df)) S_x_df <- dplyr::rename(S_x_df,cell=key)
    
    S_x_df <- S_x_df %>%
      mutate(S_x = S_x / Ab)
    
  }
  
  if((!main_c[i] %in% raw_biomass_spp) & raw_biom == T){
    
    S_x_df <- S_x_df %>%
      mutate(S_x = S_x * Ab)
    
  }
  
  # standardize over years
  if(standardize == "year"){
    
    stand_df <- S_x_df %>%
      group_by(year) %>%
      dplyr::summarise(stand_const=sum(S_x))
    
    S_x_df <- inner_join(S_x_df,stand_df)
    if(!"cell" %in% colnames(S_x_df)) S_x_df <- dplyr::rename(S_x_df,cell=key)
    
    S_x_df <- S_x_df %>%
      mutate(S_x = S_x / stand_const)
    
  }
  
  if(i==2){
    S_x_df <- S_x_df %>%
      filter(str_detect(strata,"Gn|Gs"))
  }
  
  if(i==3){
    S_x_df <- S_x_df %>%
      filter(str_detect(strata,"Cc|Cn|Cs"))
  }
  
  S_x_df <- S_x_df %>%
    dplyr::rename(Month = month,
                  Year = year) %>%
    mutate(Year_Month = paste0(Year,"_",ifelse(Month<10,paste0("0",Month),Month))) %>%
    dplyr::select(-t)
  
  # Make time step dataframe
  year_start <- min(S_x_df$Year)
  year_end <- max(S_x_df$Year)
  year_vec <- unique(S_x_df$Year) # time period
  month_vec <- 1:12
  quarter_month_df <- data.frame(Month=1:12,Quarter=rep(1:4,each = 3))
  time.step_df <- expand.grid(1:12,year_start:year_end)
  colnames(time.step_df) <- c("Month","Year")
  time.step_df <- time.step_df %>%
    inner_join(quarter_month_df) %>%
    mutate(Year_Month = paste0(Year,"_",ifelse(Month<10,paste0("0",Month),Month)),
           t = 1:nrow(time.step_df))
  
  S_x_df <- S_x_df %>%
    inner_join(time.step_df)
  
  S_x_list[[i]] <- S_x_df
  
  # Location dataframe
  loc_x_pred <- S_x_df %>%
    dplyr::select(cell,x,y) %>%
    group_by(cell,x,y) %>%
    slice(1) %>%
    arrange(cell) %>%
    as.data.frame
  
  loc_x_list[[i]] <- loc_x_pred
  
  ##---------------------------------------------------------------------------------------------------
  ##----------------------------------- Extract covariates --------------------------------------------
  ##---------------------------------------------------------------------------------------------------
  
  if(extract_covariates){
    
    for(year in year_start:year_end){
      
      print(year)
      
      for(month in month_vec){
        
        print(month)
        
        cov_df <- loc_x_pred
        
        for(copernicus.var_j in copernicus.var){
          
          print(copernicus.var_j)
          
          gridpoint <- SpatialPointsDataFrame(coords=cov_df[,c("x","y")],
                                              data=data.frame(key = 1:nrow(cov_df)),
                                              proj4string=CRS(grid_projection))
          
          month_i <- ifelse(month>9,paste0(month),paste0("0",month))
          year_month <- paste0(year,"-",month_i)
          
          ## Read variable in nc table
          if(copernicus.var_j %in% c("so","bottomT","thetao")) nc_in <- open.nc(paste0(cov_file,"dataset-ibi-reanalysis-phys-005-002-monthly_1576082594192_0m.nc"))
          if(copernicus.var_j %in% c("fe","nppv","nh4","si","phyc","po4","no3","zeu","chl","o2")) nc_in <- open.nc(paste0(cov_file,"dataset-ibi-reanalysis-bio-005-003-monthly_1576085356161_0m.nc"))
          
          nc_lon <- var.get.nc(nc_in, "longitude")
          nc_lat <- var.get.nc(nc_in, "latitude")
          nc_var <- var.get.nc(nc_in, copernicus.var_j,unpack=TRUE)
          nc_t <- var.get.nc(nc_in, "time")
          nc_t <- as.POSIXct(nc_t*60*60, origin="1950-01-01",tz="GMT", format="%Y-%m-%d")
          close.nc(nc_in)
          
          ## Build raster with NetCDF data
          gt <- GridTopology(cellcentre.offset = c(nc_lon[1], nc_lat[1]),
                             cellsize = c((tail(nc_lon, 1) - nc_lon[1])/(length(nc_lon) - 1), (tail(nc_lat, 1) - nc_lat[1])/(length(nc_lat) - 1)),
                             cells.dim = c(length(nc_lon), length(nc_lat)))
          sg <- SpatialGrid(gt, CRS(grid_projection))
          sgdf <- SpatialGridDataFrame(sg, data.frame(var = as.vector(nc_var[,,which(str_detect(nc_t,year_month))])))
          raster_var <- flip(raster(sgdf),direction = 2)
          
          ## Extract covariate form raster at the 'gridpoint' level
          extract_raster_var <- raster::extract(stack(raster_var), coordinates(gridpoint))
          copernicuspoint.sp <- gridpoint
          copernicuspoint.sp@data <- as.data.frame(cbind(copernicuspoint.sp@data[,"key"],extract_raster_var))
          colnames(copernicuspoint.sp@data)[1] <- "key"
          colnames(copernicuspoint.sp@data)[2] <- copernicus.var_j
          cov_df = cbind(cov_df,copernicuspoint.sp@data[,2])
          colnames(cov_df)[ncol(cov_df)] = copernicus.var_j
          
        }
        
        cov_df <- cov_df %>% 
          mutate(Year = year,Month = month)
        
        if(year == year_start) cov_df_full = cov_df
        if(year != year_start) cov_df_full = rbind(cov_df,cov_df_full)
        
      }
      
    }
    
    cov_list[[i]] = cov_df_full
    
  }
  
  
  
  ##################################################
  
  ##-----------------------------------------------------------------------------------------
  ##----------------------------------- Make EOF --------------------------------------------
  ##-----------------------------------------------------------------------------------------

  ## Make the T x N matrix for conducting EOF
  EOF_mat <- S_x_df %>%
    dplyr::select(cell,Year_Month,S_x) %>%
    pivot_wider(names_from = Year_Month,values_from = S_x) %>%
    as.matrix() %>% t()
  EOF_mat <- EOF_mat[-1,]
  
  ## Decomposing EOF
  ##----------------
  ## Center EOF_mat
  spat_mean <- apply(EOF_mat, 2, mean)
  nT <- nrow(EOF_mat)
  Zspat_detrend <- EOF_mat - outer(rep(1, nT), spat_mean)
  Zt <- Zspat_detrend # 1/sqrt(nT - 1)*
  Zt_list[[i]] <- Zt
  
  mean_patt[[i]] <- cbind(loc_x_pred,spat_mean)

  ## Conduct svd (singular vector decomposition)
  E <- svd(Zt)
  E_list[[i]] <- E
  
  # Eigen values
  L=E$d*E$d
  pct=L/sum(L) # explained variance
  
  eigen_df <- data.frame(dim=1:length(L),
                         sg_val=L,
                         perc_var=pct,
                         cum_sum=cumsum(pct))
  
  eigen_values[[i]] <- eigen_df
  
  # x11()
  # plot(pct,ylim=c(0,max(pct)+0.01))
  # text(x = 1:length(pct),y=pct+0.005)
  # 
  # x11()
  # plot(cumsum(pct),ylim=c(0,max(cumsum(pct))+0.01))
  # text(x = 1:length(pct),y=cumsum(pct)+0.005)
  
  # Spatial patterns
  V <- E$v # %*% sqrt(diag(E$d))
  colnames(E$v) <- paste0("EOF", 1:nrow(EOF_mat))
  colnames(V) <- paste0("EOF", 1:nrow(EOF_mat)) # label columns
  V_list[[i]] <- V
  
  EOFs <- cbind(loc_x_pred[,c("x","y","cell")], V)
  
  EOFs_2 <- EOFs %>%
    select_at(c("x","y","cell",paste0("EOF",1:nT))) %>%
    pivot_longer(cols = starts_with("EOF")) %>%
    dplyr::rename(EOF = name) %>%
    filter(EOF %in% c(paste0("EOF",1:nT)))
  
  # Correlation of variables with dimensions
  c_sigma_Z <- apply(Zt, 2,var)
  mat_sigma_Z <- 1/sqrt(diag(c_sigma_Z))
  mat_sigma_Z[which(is.infinite(mat_sigma_Z))] <- 0
  corrPCVar <- mat_sigma_Z %*% V %*% sqrt(diag(E$d))
  
  colnames(corrPCVar) <- paste0("EOF", 1:nrow(EOF_mat)) # label columns
  corrPCVar_df <- cbind(loc_x_pred[,c("x","y","cell")], corrPCVar)
  corrPCVar_df_2 <- corrPCVar_df %>%
    select_at(c("x","y","cell",paste0("EOF",1:nT))) %>%
    pivot_longer(cols = starts_with("EOF")) %>%
    dplyr::rename(EOF = name,corrPCVar=value) %>%
    filter(EOF %in% c(paste0("EOF",1:nT)))
  
  EOFs_3 <- inner_join(EOFs_2,corrPCVar_df_2)
  
  
  # Contribution of variables to axis
  ContribVar <- E$v^2
  colnames(ContribVar) <- paste0("EOF", 1:nrow(EOF_mat)) # label columns
  ContribVar_df <- cbind(loc_x_pred[,c("x","y","cell")],ContribVar)
  
  ContribVar_df_2 <- ContribVar_df %>%
    select_at(c("x","y","cell",paste0("EOF",1:nT))) %>%
    pivot_longer(cols = starts_with("EOF")) %>%
    dplyr::rename(EOF = name,ContribVar=value) %>%
    filter(EOF %in% c(paste0("EOF",1:nT)))
  
  EOFs_4 <- inner_join(EOFs_3,ContribVar_df_2)
  
  EOF_maps[[i]] <- EOFs_4
  
  # Temporal indices
  EOFset1 <- E$u[1:(nT),]
  EOFset1 <- EOFset1 %>% data.frame
  
  PC_list[[i]] <- EOFset1
  
  colnames(EOFset1) <- paste0("PC",1:nT)
  EOFset1 <- EOFset1[,1:nT] %>% data.frame
  EOFset1_2 <- EOFset1 %>%
    mutate(t = 1:nrow(EOFset1)) %>%
    pivot_longer(cols = starts_with("PC")) %>%
    dplyr::rename(PC = name) %>%
    filter(PC %in% c(paste0("PC",1:nT))) %>%
    inner_join(time.step_df)
  
  EOF_PC[[i]] <- EOFset1_2
  
}

save(data = S_x_list,file = "res/S_x_list.RData")
save(data = EOF_maps,file = "res/EOF_maps.RData")
save(data = EOF_PC,file = "res/EOF_PC.RData")
save(data = eigen_values,file = "res/eigen_values.RData")
save(data = Zt_list,file = "res/Zt_list.RData")
save(data = E_list,file = "res/E_list.RData")
save(data = loc_x_list,file = "res/loc_x_list.RData")
save(data = cov_list,file = "res/cov_list.RData")

##----------------------------------------------------------------------------------------------------
##----------------------------------- Make plots/analysis --------------------------------------------
##----------------------------------------------------------------------------------------------------

## Percentage of variance
#------------------------
for(i in 1:length(main_c)){
  
  perc_var <- eigen_values[[i]]$perc_var
  eigen_vec <- perc_var[order(perc_var,decreasing = T)][1:15]
  eigen_df <- data.frame(perc_var = eigen_vec, dim = 1:15,spp = species_name[i])
  
  if(i==1) eigen_df_full <- eigen_df
  if(i!=1) eigen_df_full <- rbind(eigen_df,eigen_df_full)
  
}

eigen_df_full$spp <- factor(eigen_df_full$spp,levels = c(species_name))

vert_bar <- data.frame(dim = c(6.5,2.5,2.5,1.5), spp = species_name)
eigen_df_plot <- ggplot(eigen_df_full,
                        aes(x = dim, y = perc_var * 100))+
  geom_bar(stat = "identity",fill="steelblue")+
  facet_wrap(.~factor(spp),scales = "free_y")+
  theme_classic()+
  theme(aspect.ratio = 1)+
  ylab("Percentage of variance (%)")+xlab("Dimension")+
  geom_vline(data=vert_bar,aes(xintercept = dim),
             color = "red",linewidth=0.75,linetype="dashed")

ggsave(filename = "images/eigen_df_plot.png",width = 5,height = 5)

## Plot EOF maps and time series
#-------------------------------

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
  
  for(year in 2008:2018){
    
    toto = EOF_PC_spp_2 %>% 
      filter(Year == year)
    
    month_max = which(toto$value == max(toto$value))
    month_min = which(toto$value == min(toto$value))
    
    if(is.null(minmax_PC_df)){
      
      minmax_PC_df = data.frame(species = species_name[i],
                                PC = j,
                                Year = year,
                                Month_max = month_max,
                                Month_min = month_min)
      
    }else{
      
      
      titi = data.frame(species = species_name[i],
                        PC = j,
                        Year = year,
                        Month_max = month_max,
                        Month_min = month_min)
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
  
  for(year in 2008:2018){
    
    toto = EOF_PC_spp_2 %>% 
      filter(Year == year)
    
    month_max = which(toto$value == max(toto$value))
    month_min = which(toto$value == min(toto$value))
    
    if(is.null(minmax_PC_df)){
      
      minmax_PC_df = data.frame(species = species_name[i],
                                PC = j,
                                Year = year,
                                Month_max = month_max,
                                Month_min = month_min)
      
    }else{
      
      
      titi = data.frame(species = species_name[i],
                        PC = j,
                        Year = year,
                        Month_max = month_max,
                        Month_min = month_min)
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
  
  for(year in 2008:2018){
    
    toto = EOF_PC_spp_2 %>% 
      filter(Year == year)
    
    month_max = which(toto$value == max(toto$value))
    month_min = which(toto$value == min(toto$value))
    
    if(is.null(minmax_PC_df)){
      
      minmax_PC_df = data.frame(species = species_name[i],
                                PC = j,
                                Year = year,
                                Month_max = month_max,
                                Month_min = month_min)
      
    }else{
      
      
      titi = data.frame(species = species_name[i],
                        PC = j,
                        Year = year,
                        Month_max = month_max,
                        Month_min = month_min)
      minmax_PC_df = rbind(minmax_PC_df,titi)
      
    }
    
  }
  
}

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
  
  for(year in 2008:2018){
    
    toto = EOF_PC_spp_2 %>% 
      filter(Year == year)
    
    month_max = which(toto$value == max(toto$value))
    month_min = which(toto$value == min(toto$value))
    
    if(is.null(minmax_PC_df)){
      
      minmax_PC_df = data.frame(species = species_name[i],
                                PC = j,
                                Year = year,
                                Month_max = month_max,
                                Month_min = month_min)
      
    }else{
      
      
      titi = data.frame(species = species_name[i],
                        PC = j,
                        Year = year,
                        Month_max = month_max,
                        Month_min = month_min)
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


## Make full picture
title_sole <- ggdraw() + draw_label("Sole",
                                    hjust = 0.5,
                                    fontface='bold.italic',
                                    color = "darkgrey",
                                    size = 18)

title_hake_bob <- ggdraw() + draw_label("Hake - BoB",
                                        hjust = 0.5,
                                        fontface='bold.italic',
                                        color = "darkgrey",
                                        size = 18)

title_hake_cs <- ggdraw() + draw_label("Hake - CS",
                                       hjust = 0.5,
                                       fontface='bold.italic',
                                       color = "darkgrey",
                                       size = 18)

title_seabass <- ggdraw() + draw_label("Sea bass",
                                       hjust = 0.5,
                                       fontface='bold.italic',
                                       color = "darkgrey",
                                       size = 18)

EOF_sole <- plot_grid(list_EOF_PC_sole[[1]],list_EOF_map_sole[[1]],
                      list_EOF_PC_sole[[2]],list_EOF_map_sole[[2]],
                      nrow = 2,align = "v",
                      rel_widths = c(1,0.6))

EOF_sole <- plot_grid(title_sole,EOF_sole,rel_heights = c(0.1,1),ncol=1)

EOF_hake_bob <- plot_grid(list_EOF_PC_hake_bob[[1]],list_EOF_map_hake_bob[[1]],
                          list_EOF_PC_hake_bob[[2]],list_EOF_map_hake_bob[[2]],
                          nrow = 2,align = "v",
                          rel_widths = c(1,0.6))

EOF_hake_bob <- plot_grid(title_hake_bob,EOF_hake_bob,rel_heights = c(0.1,1),ncol=1)

EOF_hake_cs <- plot_grid(list_EOF_PC_hake_cs[[1]],list_EOF_map_hake_cs[[1]],
                         list_EOF_PC_hake_cs[[2]],list_EOF_map_hake_cs[[2]],
                         nrow = 2,align = "v",
                         rel_widths = c(1,0.6))

EOF_hake_cs <- plot_grid(title_hake_cs,EOF_hake_cs,rel_heights = c(0.1,1),ncol=1)


EOF_seabass <- plot_grid(list_EOF_PC_seabass[[1]],list_EOF_map_seabass[[1]],
                         NULL,NULL,
                         nrow = 2,align = "v",
                         rel_widths = c(1,0.6))

EOF_seabass <- plot_grid(title_seabass,EOF_seabass,rel_heights = c(0.1,1),ncol=1)

EOF_full <- plot_grid(EOF_sole,EOF_seabass,EOF_hake_bob,EOF_hake_cs,ncol=2,align="v")

ggsave(filename = "images/EOF_map_plot.png",width = 20,height = 15)


## Make clustering analysis
#--------------------------

# Sole
#-----
## Clustering on time
i=1
trimestre <- as.factor(rep(c(1,1,1,2,2,2,3,3,3,4,4,4),11))
mois <- as.factor(rep(c(1:12),11))
hcpc_data <- as.data.frame(as.matrix(PC_list[[i]]) %*% diag(E_list[[i]]$d)) # project time steps
res_t <- HCPC(hcpc_data,nb.clust = 3,graph = F)
clust_t_df <- data.frame(dim1=res_t$data.clust[,1],
                         dim2=res_t$data.clust[,2],
                         clust=res_t$data.clust$clust,
                         Year_Month=time.step_df$Year_Month)

var_dim_1 <- eigen_df_full$perc_var[which(eigen_df_full$dim == 1 & eigen_df_full$spp == "Sole")]
var_dim_2 <- eigen_df_full$perc_var[which(eigen_df_full$dim == 2 & eigen_df_full$spp == "Sole")]

clust_t_plot <- ggplot(data=clust_t_df,
                       aes(x=dim1,y=dim2,
                           col=clust,
                           # shape=trimestre,
                           label=Year_Month))+
  geom_hline(yintercept=0)+geom_vline(xintercept=0)+
  geom_point(size=1)+
  geom_text(nudge_y = 1.4,check_overlap = T)+
  theme_minimal()+scale_color_brewer(palette = "Set1")+
  ylab(paste0("Dim2 - ",round(var_dim_2*100,digits = 1)," %"))+
  xlab(paste0("Dim1 - ",round(var_dim_1*100,digits = 1)," %"))+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5),
        legend.title = element_blank())+
  ggtitle("Time steps projection")

ggsave(filename = "images/Solea_solea/EOF_clust_t_plot.png",width = 7.5,height = 7.5)

png(paste0("images/Solea_solea/EOF_t_tree_Sole.png"),res = 75,pointsize = 1/(6*75))

plot(res_t, axes=c(1,2), choice="tree", rect=TRUE, 
     draw.tree=TRUE, ind.names=TRUE, t.level="all", title=NULL,
     new.plot=FALSE, max.plot=15, tree.barplot=TRUE,
     centers.plot=FALSE)

dev.off()

## Clustering on space
hcpc_data <- as.data.frame(V_list[[i]] %*% diag(E_list[[i]]$d))  # project eigen vectors
res_x <- HCPC(hcpc_data,nb.clust = 6,graph = F) # 2 or 6

xlims <- range(pretty(loc_x_list[[i]]$x))
ylims <- range(pretty(loc_x_list[[i]]$y))

clust_x_df <- cbind(loc_x_list[[i]],
                    dim1=res_x$data.clust[,1],
                    dim2=res_x$data.clust[,2],
                    dim3=res_x$data.clust[,3],
                    clust=res_x$data.clust$clust)

clust_map_plot <- ggplot(data=clust_x_df)+
  geom_point(aes(x=x,y=y,col=factor(clust)))+
  theme_minimal()+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5))+
  xlab("Longitude (deg)") + ylab("Latitude (deg)") +
  geom_sf(data = mapBase) +
  coord_sf(xlim = xlims, ylim = ylims, expand = FALSE)+
  theme(plot.title = element_text(hjust=0.5),
        legend.title = element_blank())+
  ggtitle("Locations cluster")+
  scale_color_brewer(palette="Spectral")

clust_x_plot <- ggplot(data=clust_x_df,
                       aes(x=dim1,y=dim2,
                           col=clust))+
  geom_hline(yintercept=0)+geom_vline(xintercept=0)+
  geom_point()+
  theme_minimal()+
  xlab(paste0("Dim1 - ",round(var_dim_1*100,digits = 1)," %"))+
  ylab(paste0("Dim2 - ",round(var_dim_2*100,digits = 1)," %"))+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5),
        legend.title = element_blank())+
  ggtitle("Locations projection")+
  scale_color_brewer(palette="Spectral")

mean_patt_plot <- ggplot(data = mean_patt[[i]])+
  geom_point(aes(x=x,y=y,col=spat_mean))+
  theme_minimal()+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5))+
  xlab("Longitude (deg)") + ylab("Latitude (deg)") +
  geom_sf(data = mapBase) +
  coord_sf(xlim = xlims, ylim = ylims, expand = FALSE)+
  theme(plot.title = element_text(hjust=0.5),
        legend.title = element_blank())+
  ggtitle("Mean pattern")+
  scale_color_distiller(palette="Spectral")

clust_plot_sole <- plot_grid(clust_t_plot,
                             mean_patt_plot,
                             clust_x_plot,
                             clust_map_plot,
                             align = "hv",nrow = 2)

ggsave(paste0("images/Solea_solea/EOF_clust_x_sole.png"),
       width=10,height=10)


png(paste0("images/Solea_solea/EOF_x_tree_sole.png"),
    res = 75,pointsize = 1/(3*75))

plot(res_x, axes=c(1,2), choice="tree", rect=TRUE, 
     draw.tree=TRUE, ind.names=TRUE, t.level="all", title=NULL,
     new.plot=FALSE, max.plot=15, tree.barplot=TRUE,
     centers.plot=FALSE)

dev.off()


# Hake - BoB
#-----------
## Clustering on time
i=2
hcpc_data <- as.data.frame(as.matrix(PC_list[[i]]) %*% diag(E_list[[i]]$d))
res_t <- HCPC(hcpc_data,nb.clust = 2,graph = F)
clust_t_df <- data.frame(dim1=res_t$data.clust[,1],
                         dim2=res_t$data.clust[,2],
                         clust=res_t$data.clust$clust,
                         Year_Month=time.step_df$Year_Month)

var_dim_1 <- eigen_df_full$perc_var[which(eigen_df_full$dim == 1 & eigen_df_full$spp == "Hake_BoB")]
var_dim_2 <- eigen_df_full$perc_var[which(eigen_df_full$dim == 2 & eigen_df_full$spp == "Hake_BoB")]

clust_t_plot <- ggplot(data=clust_t_df,
                       aes(x=dim1,y=dim2,
                           col=clust,
                           # shape=trimestre,
                           label=Year_Month))+
  geom_hline(yintercept=0)+geom_vline(xintercept=0)+
  geom_point(size=1)+
  geom_text(nudge_y = 1.4,check_overlap = T)+
  theme_minimal()+scale_color_brewer(palette = "Set1")+
  ylab(paste0("Dim2 - ",round(var_dim_2*100,digits = 1)," %"))+
  xlab(paste0("Dim1 - ",round(var_dim_1*100,digits = 1)," %"))+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5),
        legend.title = element_blank())

ggsave(filename = "images/Merluccius_merluccius_bob/EOF_clust_t_plot_hake_bob.png",width = 7.5,height = 7.5)

png(paste0("images/Merluccius_merluccius_bob/EOF_t_tree_hake_bob.png"),res = 75,pointsize = 1/(6*75))

plot(res_t, axes=c(1,2), choice="tree", rect=TRUE, 
     draw.tree=TRUE, ind.names=TRUE, t.level="all", title=NULL,
     new.plot=FALSE, max.plot=15, tree.barplot=TRUE,
     centers.plot=FALSE)

dev.off()

## Clustering on space
hcpc_data <- as.data.frame(V_list[[i]] %*% diag(E_list[[i]]$d))
res_x <- HCPC(hcpc_data,nb.clust = 2,graph = F)

xlims <- range(pretty(loc_x_list[[i]]$x))
ylims <- range(pretty(loc_x_list[[i]]$y))

clust_x_df <- cbind(loc_x_list[[i]],
                    dim1=res_x$data.clust[,1],
                    dim2=res_x$data.clust[,2],
                    dim3=res_x$data.clust[,3],
                    clust=res_x$data.clust$clust)

clust_map_plot <- ggplot(data=clust_x_df)+
  geom_point(aes(x=x,y=y,col=factor(clust)))+
  theme_minimal()+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5))+
  xlab("Longitude (deg)") + ylab("Latitude (deg)")+
  geom_sf(data = mapBase) +
  coord_sf(xlim = xlims, ylim = ylims, expand = FALSE)+
  theme(plot.title = element_text(hjust=0.5),
        legend.title = element_blank())+
  scale_color_brewer(palette="Set2")

clust_x_plot <- ggplot(data=clust_x_df,
                       aes(x=dim1,y=dim2,
                           col=clust))+
  geom_hline(yintercept=0)+geom_vline(xintercept=0)+
  geom_point()+
  theme_minimal()+
  xlab(paste0("Dim1 - ",round(var_dim_1*100,digits = 1)," %"))+
  ylab(paste0("Dim2 - ",round(var_dim_2*100,digits = 1)," %"))+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5),
        legend.title = element_blank())+
  scale_color_brewer(palette="Set2")

mean_patt_plot <- ggplot(data = mean_patt[[i]])+
  geom_point(aes(x=x,y=y,col=spat_mean))+
  theme_minimal()+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5))+
  xlab("Longitude (deg)") + ylab("Latitude (deg)") +
  geom_sf(data = mapBase) +
  coord_sf(xlim = xlims, ylim = ylims, expand = FALSE)+
  theme(plot.title = element_text(hjust=0.5),
        legend.title = element_blank())+
  ggtitle("Mean pattern")+
  scale_color_distiller(palette="Spectral")

clust_plot_hake_bob <- plot_grid(clust_t_plot,
                                 mean_patt_plot,
                                 clust_x_plot,
                                 clust_map_plot,
                                 align = "hv",nrow = 2)

ggsave(paste0("images/Merluccius_merluccius_bob/EOF_clust_x_plot_hake_bob.png"),
       width=10,height=10)


png(paste0("images/Merluccius_merluccius_bob/EOF_x_tree_hake_bob.png"),
    res = 75,pointsize = 1/(3*75))

plot(res_x, axes=c(1,2), choice="tree", rect=TRUE, 
     draw.tree=TRUE, ind.names=TRUE, t.level="all", title=NULL,
     new.plot=FALSE, max.plot=15, tree.barplot=TRUE,
     centers.plot=FALSE)

dev.off()

# Hake - CS
#----------
## Clustering on time
i=3
hcpc_data <- as.data.frame(as.matrix(PC_list[[i]]) %*% diag(E_list[[i]]$d))
res_t <- HCPC(hcpc_data,nb.clust = 2,graph = F)
clust_t_df <- data.frame(dim1=res_t$data.clust[,1],
                         dim2=res_t$data.clust[,2],
                         clust=res_t$data.clust$clust,
                         Year_Month=time.step_df$Year_Month)

var_dim_1 <- eigen_df_full$perc_var[which(eigen_df_full$dim == 1 & eigen_df_full$spp == "Hake_CS")]
var_dim_2 <- eigen_df_full$perc_var[which(eigen_df_full$dim == 2 & eigen_df_full$spp == "Hake_CS")]

clust_t_plot <- ggplot(data=clust_t_df,
                       aes(x=dim1,y=dim2,
                           col=clust,
                           # shape=trimestre,
                           label=Year_Month))+
  geom_hline(yintercept=0)+geom_vline(xintercept=0)+
  geom_point(size=1)+
  geom_text(nudge_y = 1.4,check_overlap = T)+
  theme_minimal()+scale_color_brewer(palette = "Set1")+
  ylab(paste0("Dim2 - ",round(var_dim_2*100,digits = 1)," %"))+
  xlab(paste0("Dim1 - ",round(var_dim_1*100,digits = 1)," %"))+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5),
        legend.title = element_blank())

ggsave(filename = "images/Merluccius_merluccius_cs/EOF_clust_t_plot_hake_cs.png",width = 7.5,height = 7.5)

png(paste0("images/Merluccius_merluccius_cs/EOF_t_tree_hake_cs.png"),res = 75,pointsize = 1/(6*75))

plot(res_t, axes=c(1,2), choice="tree", rect=TRUE, 
     draw.tree=TRUE, ind.names=TRUE, t.level="all", title=NULL,
     new.plot=FALSE, max.plot=15, tree.barplot=TRUE,
     centers.plot=FALSE)

dev.off()

## Clustering on space
hcpc_data <- as.data.frame(V_list[[i]] %*% diag(E_list[[i]]$d))
res_x <- HCPC(hcpc_data,nb.clust = 3 + 3,graph = F)

xlims <- range(pretty(loc_x_list[[i]]$x))
ylims <- range(pretty(loc_x_list[[i]]$y))

clust_x_df <- cbind(loc_x_list[[i]],
                    dim1=res_x$data.clust[,1],
                    dim2=res_x$data.clust[,2],
                    dim3=res_x$data.clust[,3],
                    clust=res_x$data.clust$clust)

clust_map_plot <- ggplot(data=clust_x_df)+
  geom_point(aes(x=x,y=y,col=factor(clust)))+
  theme_minimal()+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5))+
  xlab("Longitude (deg)") + ylab("Latitude (deg)")+
  geom_sf(data = mapBase) +
  coord_sf(xlim = xlims, ylim = ylims, expand = FALSE)+
  theme(plot.title = element_text(hjust=0.5),
        legend.title = element_blank())+
  scale_color_brewer(palette="Set2")

clust_x_plot <- ggplot(data=clust_x_df,
                       aes(x=dim1,y=dim2,
                           col=clust))+
  geom_hline(yintercept=0)+geom_vline(xintercept=0)+
  geom_point()+
  theme_minimal()+
  xlab(paste0("Dim1 - ",round(var_dim_1*100,digits = 1)," %"))+
  ylab(paste0("Dim2 - ",round(var_dim_2*100,digits = 1)," %"))+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5),
        legend.title = element_blank())+
  scale_color_brewer(palette="Set2")

mean_patt_plot <- ggplot(data = mean_patt[[i]])+
  geom_point(aes(x=x,y=y,col=spat_mean))+
  theme_minimal()+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5))+
  xlab("Longitude (deg)") + ylab("Latitude (deg)") +
  geom_sf(data = mapBase) +
  coord_sf(xlim = xlims, ylim = ylims, expand = FALSE)+
  theme(plot.title = element_text(hjust=0.5),
        legend.title = element_blank())+
  ggtitle("Mean pattern")+
  scale_color_distiller(palette="Spectral")

clust_plot_hake_cs <- plot_grid(clust_t_plot,
                                mean_patt_plot,
                                clust_x_plot,
                                clust_map_plot,
                                align = "hv",nrow = 2)

ggsave(paste0("images/Merluccius_merluccius_cs/EOF_clust_x_hake_cs.png"),
       width=10,height=10)

png(paste0("images/Merluccius_merluccius_cs/EOF_x_tree_hake_cs.png"),
    res = 75,pointsize = 1/(3*75))

plot(res_x, axes=c(1,2), choice="tree", rect=TRUE, 
     draw.tree=TRUE, ind.names=TRUE, t.level="all", title=NULL,
     new.plot=FALSE, max.plot=15, tree.barplot=TRUE,
     centers.plot=FALSE)

dev.off()



# Sea bass
#---------
## Clustering on time
i=4
hcpc_data <- as.data.frame(as.matrix(PC_list[[i]]) %*% diag(E_list[[i]]$d))
res_t <- HCPC(hcpc_data,nb.clust = 2,graph = F)
clust_t_df <- data.frame(dim1=res_t$data.clust[,1],
                         dim2=res_t$data.clust[,2],
                         clust=res_t$data.clust$clust,
                         Year_Month=time.step_df$Year_Month)

var_dim_1 <- eigen_df_full$perc_var[which(eigen_df_full$dim == 1 & eigen_df_full$spp == "Seabass")]
var_dim_2 <- eigen_df_full$perc_var[which(eigen_df_full$dim == 2 & eigen_df_full$spp == "Seabass")]

clust_t_plot <- ggplot(data=clust_t_df,
                       aes(x=dim1,y=dim2,
                           col=clust,
                           # shape=trimestre,
                           label=Year_Month))+
  geom_hline(yintercept=0)+geom_vline(xintercept=0)+
  geom_point(size=1)+
  geom_text(nudge_y = 1.4,check_overlap = T)+
  theme_minimal()+scale_color_brewer(palette = "Set1")+
  ylab(paste0("Dim2 - ",round(var_dim_2*100,digits = 1)," %"))+
  xlab(paste0("Dim1 - ",round(var_dim_1*100,digits = 1)," %"))+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5),
        legend.title = element_blank())

ggsave(filename = "images/Dicentrarchus_Labrax/EOF_clust_t_plot_seabass.png",width = 7.5,height = 7.5)

png(paste0("images/Dicentrarchus_Labrax/EOF_t_tree_seabass.png"),res = 75,pointsize = 1/(6*75))

plot(res_t, axes=c(1,2), choice="tree", rect=TRUE, 
     draw.tree=TRUE, ind.names=TRUE, t.level="all", title=NULL,
     new.plot=FALSE, max.plot=15, tree.barplot=TRUE,
     centers.plot=FALSE)

dev.off()

## Clustering on space
hcpc_data <- as.data.frame(V_list[[i]] %*% diag(E_list[[i]]$d))
res_x <- HCPC(hcpc_data,nb.clust = 2 + 2,graph = F)

xlims <- range(pretty(loc_x_list[[i]]$x))
ylims <- range(pretty(loc_x_list[[i]]$y))

clust_x_df <- cbind(loc_x_list[[i]],
                    dim1=res_x$data.clust[,1],
                    dim2=res_x$data.clust[,2],
                    dim3=res_x$data.clust[,3],
                    clust=res_x$data.clust$clust)

clust_map_plot <- ggplot(data=clust_x_df)+
  geom_point(aes(x=x,y=y,col=factor(clust)))+
  theme_minimal()+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5))+
  xlab("Longitude (deg)") + ylab("Latitude (deg)")+
  geom_sf(data = mapBase) +
  coord_sf(xlim = xlims, ylim = ylims, expand = FALSE)+
  theme(plot.title = element_text(hjust=0.5),
        legend.title = element_blank())+
  scale_color_brewer(palette="Set2")

clust_x_plot <- ggplot(data=clust_x_df,
                       aes(x=dim1,y=dim2,
                           col=clust))+
  geom_hline(yintercept=0)+geom_vline(xintercept=0)+
  geom_point()+
  theme_minimal()+
  xlab(paste0("Dim1 - ",round(var_dim_1*100,digits = 1)," %"))+
  ylab(paste0("Dim2 - ",round(var_dim_2*100,digits = 1)," %"))+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5),
        legend.title = element_blank())+
  scale_color_brewer(palette="Set2")


mean_patt_plot <- ggplot(data = mean_patt[[i]])+
  geom_point(aes(x=x,y=y,col=spat_mean))+
  theme_minimal()+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5))+
  xlab("Longitude (deg)") + ylab("Latitude (deg)") +
  geom_sf(data = mapBase) +
  coord_sf(xlim = xlims, ylim = ylims, expand = FALSE)+
  theme(plot.title = element_text(hjust=0.5),
        legend.title = element_blank())+
  ggtitle("Mean pattern")+
  scale_color_distiller(palette="Spectral")


clust_plot_seabass <- plot_grid(clust_t_plot,
                                mean_patt_plot,
                                clust_x_plot,
                                clust_map_plot,
                                align = "hv",nrow = 2)

ggsave(paste0("images/Dicentrarchus_Labrax/EOF_clust_x_seabass.png"),
       width=10,height=10)


png(paste0("images/Dicentrarchus_Labrax/EOF_x_tree_seabass.png"),
    res = 75,pointsize = 1/(3*75))

plot(res_x, axes=c(1,2), choice="tree", rect=TRUE, 
     draw.tree=TRUE, ind.names=TRUE, t.level="all", title=NULL,
     new.plot=FALSE, max.plot=15, tree.barplot=TRUE,
     centers.plot=FALSE)

dev.off()

test <- plot_grid(title_sole,clust_plot_sole,
                  title_hake_bob,clust_plot_hake_bob,
                  title_hake_cs,clust_plot_hake_cs,
                  title_seabass,clust_plot_seabass,
                  nrow = 4,rel_widths = c(0.25,1))

ggsave(paste0("images/EOF_clust.png"),
       width=13.5,height=13.5)


## For sole - seasonal patterns
#------------------------------
S_x_df = S_x_list[[1]]
Season_df <- S_x_df
Season_df$season <- NA
Season_df$season[which(Season_df$Month %in% c(12,1,2))] <- "December - February"
Season_df$season[which(Season_df$Month %in% c(3,4,5,6))] <- "March - June"
Season_df$season[which(Season_df$Month %in% c(7:11))] <- "July - November"

Season_df$season <- factor(Season_df$season,levels = c("December - February",
                                                       "March - June",
                                                       "July - November"))

season_patt_df <- Season_df %>%
  dplyr::group_by(x,y,season) %>%
  dplyr::summarise(S_x = mean(S_x))

season_patt_plot = ggplot(data = season_patt_df)+
  geom_point(aes(x=x,y=y,col=S_x))+
  theme_minimal()+
  theme(plot.title = element_text(hjust=0.5))+
  xlab("Longitude (deg)") + ylab("Latitude (deg)") +
  geom_sf(data = mapBase) +
  coord_sf(xlim = xlims, ylim = ylims, expand = FALSE)+
  theme(plot.title = element_text(hjust=0.5),
        legend.title = element_blank(),
        panel.spacing.x = unit(10, "mm"))+
  scale_color_distiller(palette="Spectral")+
  facet_wrap(.~factor(season))

ggsave(paste0("images/Solea_solea/season_patt_plot.png"),
       width=10,height=6)

## Abundance-weighted average season
#-----------------------------------
x11()
for(i in 1:4){
  
  S_x_df = S_x_list[[i]]
  u_bar = c()
  for(year in 2008:2018){
    
    S_x_df_2 = S_x_df %>%
      filter(Year == year)
    D_y = sum(S_x_df_2$S_x)
    D_t = c()
    for(month in 1:12){
      S_x_df_3 = S_x_df_2 %>% 
        filter(Month == month)
      D_t = c(D_t,sum(S_x_df_3$S_x))
    }
    u_bar = c(u_bar,sum(D_t/D_y * c(1:12)))
  }
  
  if(i==1){
    plot(u_bar,main=species_name[i],ylim = c(0,12),col=i)
    lines(u_bar)
  }
  
  if(i>1) lines(u_bar,col=i)
  
}


## Maximum and Minimum of PC - Month vs. Year
#---------------------------------------------
load("res/minmax_PC_df.RData")

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

minmax_PC_df$Month_max_name = NA
minmax_PC_df$Month_max_name[which(minmax_PC_df$Month_max == 1)] = month_name[1]
minmax_PC_df$Month_max_name[which(minmax_PC_df$Month_max == 2)] = month_name[2]
minmax_PC_df$Month_max_name[which(minmax_PC_df$Month_max == 3)] = month_name[3]
minmax_PC_df$Month_max_name[which(minmax_PC_df$Month_max == 4)] = month_name[4]
minmax_PC_df$Month_max_name[which(minmax_PC_df$Month_max == 5)] = month_name[5]
minmax_PC_df$Month_max_name[which(minmax_PC_df$Month_max == 6)] = month_name[6]
minmax_PC_df$Month_max_name[which(minmax_PC_df$Month_max == 7)] = month_name[7]
minmax_PC_df$Month_max_name[which(minmax_PC_df$Month_max == 8)] = month_name[8]
minmax_PC_df$Month_max_name[which(minmax_PC_df$Month_max == 9)] = month_name[9]
minmax_PC_df$Month_max_name[which(minmax_PC_df$Month_max == 10)] = month_name[10]
minmax_PC_df$Month_max_name[which(minmax_PC_df$Month_max == 11)] = month_name[11]
minmax_PC_df$Month_max_name[which(minmax_PC_df$Month_max == 12)] = month_name[12]

minmax_PC_df$Month_min_name = NA
minmax_PC_df$Month_min_name[which(minmax_PC_df$Month_min == 1)] = month_name[1]
minmax_PC_df$Month_min_name[which(minmax_PC_df$Month_min == 2)] = month_name[2]
minmax_PC_df$Month_min_name[which(minmax_PC_df$Month_min == 3)] = month_name[3]
minmax_PC_df$Month_min_name[which(minmax_PC_df$Month_min == 4)] = month_name[4]
minmax_PC_df$Month_min_name[which(minmax_PC_df$Month_min == 5)] = month_name[5]
minmax_PC_df$Month_min_name[which(minmax_PC_df$Month_min == 6)] = month_name[6]
minmax_PC_df$Month_min_name[which(minmax_PC_df$Month_min == 7)] = month_name[7]
minmax_PC_df$Month_min_name[which(minmax_PC_df$Month_min == 8)] = month_name[8]
minmax_PC_df$Month_min_name[which(minmax_PC_df$Month_min == 9)] = month_name[9]
minmax_PC_df$Month_min_name[which(minmax_PC_df$Month_min == 10)] = month_name[10]
minmax_PC_df$Month_min_name[which(minmax_PC_df$Month_min == 11)] = month_name[11]
minmax_PC_df$Month_min_name[which(minmax_PC_df$Month_min == 12)] = month_name[12]

minmax_PC_df$Month_max_name = factor(minmax_PC_df$Month_max_name,levels = rev(month_name))
minmax_PC_df$Month_min_name = factor(minmax_PC_df$Month_min_name,levels = rev(month_name))
minmax_PC_df$Year = as.character(minmax_PC_df$Year)

minmax_PC_df$species = factor(minmax_PC_df$species,levels = c("Sole","Hake_BoB","Hake_CS","Seabass"))

minmax_PC_df_2 = minmax_PC_df %>% 
  filter(PC %in% c(1,2))

max_month_plot = ggplot(minmax_PC_df_2)+
  geom_point(aes(y=Month_max_name,x=Year))+
  facet_grid(~species+PC)+
  ggtitle("Maximum principal component value")+
  theme_bw()+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust = 0.5,face = "bold"),
        axis.text.x = element_text(angle = 90,vjust = 0.5))+
  ylab("")+xlab("")

min_month_plot = ggplot(minmax_PC_df_2)+
  geom_point(aes(y=Month_min_name,x=Year))+
  facet_grid(~species+PC)+
  ggtitle("Minimum principal component value")+
  theme_bw()+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust = 0.5,face = "bold"),
        axis.text.x = element_text(angle = 90,vjust = 0.5))+
  ylab("")+xlab("")

plot_grid(max_month_plot,min_month_plot,ncol = 1)

ggsave(paste0("images/minmax_PC_plot.png"),
       width=10,height=6)

## Analyse with covariates
#-------------------------
load("res/cov_list.RData")

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
      test_cor = cor.test(PC_to_plot,log(var_to_plot),method = "spearman")
      
      if(is.null(cor_cov_PC_df)){
        
        cor_cov_PC_df = data.frame(species = species_name[i],
                                   dimPC = j,
                                   covar = var,
                                   Rspearman = test_cor$estimate,
                                   p_val = test_cor$p.value
                                   )
        
      }else{
        
        test = data.frame(species = species_name[i],
                                   dimPC = j,
                                   covar = var,
                                   Rspearman = test_cor$estimate,
                                   p_val = test_cor$p.value
        )
        
        cor_cov_PC_df = rbind(test,cor_cov_PC_df)
        
      }
      
      if(plot_corr == T){
        
        plot(PC_to_plot,log(var_to_plot),main = paste("PC",j," vs. ",var))
        mtext(paste0("Rspearman = ",
                     round(test_cor$estimate,digits = 3),
                     " p-val = ",
                     round(test_cor$p.value,digits = 3),
                     side=3))
        
      }
    }
  }
}


cor_cov_PC_df_2 = cor_cov_PC_df %>% 
  filter(p_val < 0.05 & dimPC < 3) %>% 
  mutate(pond = ifelse(str_detect(covar,"pond"),"pond","no_pond")) %>% 
  mutate(covar=str_remove(covar,"_pond"))


cor_cov_PC_df_2$species = factor(cor_cov_PC_df_2$species,levels = rev(unique(cor_cov_PC_df_2$species)))

cor_cov_PC_plot = ggplot(cor_cov_PC_df_2)+
  geom_point(aes(x=covar,y=Rspearman,col=pond))+
  facet_wrap(~species+dimPC,ncol = 2)+theme_bw()+
  geom_hline(yintercept = 0)+
  ylim(-1,1)+theme(aspect.ratio = 1)+
  xlab("")

ggsave(paste0("images/cor_cov_PC_plot.png"),
       width=3,height=10)

