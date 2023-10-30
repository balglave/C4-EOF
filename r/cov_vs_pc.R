#############################
## Compare PC with covariates
#############################

# Join EOF maps, covariate data frame, latent field dataframe, PC coefficients
PC_cov_list = list()
for(i in 1:4){
  
  # Filtering threshold
  filt_thresh = 1/nrow(loc_x_list[[i]])
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
    
    for(var_i in c("so","bottomT","chl","thetao","o2",
                   "so_pond","bottomT_pond","chl_pond",
                   "thetao_pond","o2_pond")){
      
      index_i = which(!is.na(to_plot[,var_i]))
      PC_to_plot = to_plot$value[index_i]
      var_to_plot = unlist(to_plot[,var_i])[index_i]
      
      PC_to_plot_2 = (PC_to_plot - mean(PC_to_plot))/sd(PC_to_plot) 
      var_to_plot_2 = (var_to_plot - mean(var_to_plot))/sd(var_to_plot) 
      
      test_cor_spearman = cor.test(PC_to_plot_2,var_to_plot_2,method = "spearman")
      test_cor_pearson = cor.test(PC_to_plot_2,var_to_plot_2,method = "pearson")
      
      if(is.null(cor_cov_PC_df)){
        
        cor_cov_PC_df = data.frame(species = species_name[i],
                                   dimPC = j,
                                   covar = var_i,
                                   Rspearman = test_cor_spearman$estimate,
                                   p_val_spearman = test_cor_spearman$p.value,
                                   Rpearson = test_cor_pearson$estimate,
                                   p_val_pearson = test_cor_pearson$p.value
        )
        
      }else{
        
        test = data.frame(species = species_name[i],
                          dimPC = j,
                          covar = var_i,
                          Rspearman = test_cor_spearman$estimate,
                          p_val_spearman = test_cor_spearman$p.value,
                          Rpearson = test_cor_pearson$estimate,
                          p_val_pearson = test_cor_pearson$p.value
        )
        
        cor_cov_PC_df = rbind(test,cor_cov_PC_df)
        
      }
      
      if(plot_corr == T){
        
        plot(PC_to_plot_2,var_to_plot_2,main = paste("PC",j," vs. ",var_i))
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
