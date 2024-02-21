##############################################
## Processing of the model outputs for figures
##############################################
# B. Alglave

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
  
  S_x_df <- S_x_df %>%
    inner_join(time.step_df)
  
  S_x_list[[i]] <- S_x_df
  
  write.csv(S_x_df,file = paste0("data/",main_c[i],".csv"))
  
  # Location dataframe
  loc_x_pred <- S_x_df %>%
    dplyr::select(cell,x,y) %>%
    group_by(cell,x,y) %>%
    slice(1) %>%
    arrange(cell) %>%
    as.data.frame
  
  loc_x_list[[i]] <- loc_x_pred
 save(data = loc_x_pred,file = "res/loc_x_pred.RData")
 head(loc_x_pred)
 
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
        
        if(year == year_start & month == 1){
          
          cov_df_full = cov_df
          
        }else{
          
          cov_df_full = rbind(cov_df,cov_df_full)
          
        }
        
      }
      
    }
    
    cov_list[[i]] = cov_df_full
    
  }
  
  
 # cov_df_full %>%
 #   filter(Year == 2008 & Month == 1) %>%
 #   group_by(cell,x,y) %>%
 #   summarise(SST = mean(thetao,na.rm=T)) %>%
 #   ggplot()+
 #   geom_point(aes(x=x,y=y,col=SST))
   
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
  
  # ## Rotated EOF
  # test <- eof(Zt,n=3)
  # test$amplitude
  # test_2 <- test$REOF %>% 
  #   cbind(loc_x_pred)
  # 
  # ggplot(test_2)+
  #   geom_point(aes(x=x,y=y,
  #                  col=EOF2))+
  #   scale_color_distiller(palette = "Spectral")
  
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
