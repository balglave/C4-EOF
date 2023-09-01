
library(dplyr)
library(ggplot2)
library(reshape2)

##############################################################################################
## Analyse with covariates
#-------------------------
##############################################################################################

load("res/cov_list.RData")
load(file="res/minmax_PC_df_temp3.RData")

# 0-EXPLORATION OF COVARIATES TEMPORAL VARIABILITY 
##############################################################################################
head(EOF_maps[[i]])
test <- EOF_maps[[4]] %>% filter(EOF=="EOF1")
head(test)
ggplot(test) +geom_point(aes(x=x,y=y))

test2 <- S_x_list[[1]] %>% filter
head(test2)
ggplot(test2) +geom_point(aes(x=x,y=y))

PC_cov_list = list()
for(i in 1:4){
  EOF_cov = inner_join(EOF_maps[[i]],cov_list[[i]],multiple = "all")
  EOF_cov_S_x = inner_join(EOF_cov,S_x_list[[i]])
}


head(EOF_cov_S_x)
temp <- EOF_cov_S_x  %>% filter(EOF == "EOF3", Year==2014, Month==12)
head(temp)

head(PC_cov)
temp <- PC_cov  %>% filter(EOF == "EOF1", Year==2017, Month==12)
head(temp)

ggplot(temp)+ geom_point(aes(x=x, y=y))


# 1-ENV COVARIATES ARE DEFINED FOR THE ALL BOB
##############################################################################################
library(viridis)
PC_cov_standmean = EOF_cov_S_x %>% 
  group_by(Year_Month, Year, Month) %>% 
  dplyr::summarise(so = mean(so,na.rm=T),
                   bottomT = mean(bottomT,na.rm=T),
                   chl = mean(chl,na.rm=T),
                   thetao = mean(thetao,na.rm=T),
                   o2 = mean(o2,na.rm=T))
  
head(PC_cov_standmean)
cov_bob <- melt(PC_cov_standmean, id=c("Year_Month" ,"Year", "Month"))
head(cov_bob)
cov_bob$Year <- as.factor(cov_bob$Year)
cov_bob$Month <- as.factor(cov_bob$Month)
cov_bob <- cov_bob %>% mutate(YoI=ifelse(Year %in% c(2010), "early", "late"))


plot1 <- ggplot(cov_bob %>% filter(variable=="so"))+
 geom_line(aes(x=Month, y=value, group=Year, color=YoI))#+ 
#scale_colour_viridis_d(option = "plasma")



cov_bob <- cov_bob  %>% group_by(variable=="so") %>% mutate(var_st = (value-mean(value)/sd(value))
head(cov_bob)


plot1 <- ggplot(cov_bob)+ geom_line(aes(x=Month, y=value, group=Year, color=Year))

# 2- 
################################################################################################
# Filtering threshold
filt_thresh = 1/nrow(loc_x_pred)
head(filt_thresh)
head(S_x_list[[i]])
head(EOF_maps[[i]])
head(cov_list[[i]])


ggplot(cov_list[[i]] %>% filter(Year==2018, Month==12))+ geom_point(aes(x=x, y=y, color=bottomT))

# Join EOF maps, covariate data frame, latent field dataframe, PC coefficients
PC_cov_list = list()
for(i in 1:4){
  EOF_cov = inner_join(EOF_maps[[i]],cov_list[[i]],multiple = "all") # get contribVar
  EOF_cov_S_x = inner_join(EOF_cov,S_x_list[[i]]) # resolution temporale
  EOF_cov_S_x = inner_join(cov_list[[i]],S_x_list[[i]])
head(EOF_cov_S_x)
  PC_cov = EOF_cov_S_x %>%
    filter(ContribVar > filt_thresh) # I only keep in the analysis the data points that corresponds to strong spatial patterns (i.e. the one that strongly contribute to EOF maps)
  
  # Aggregate over time step (either standard mean or weithed mean - weights are given by biomass field values)
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

## Solea
Cov_solea <- PC_cov_list[[1]] %>% filter(PC=="PC1")%>% mutate(YoI=ifelse(Year %in% c(2009,2011,2015), "early", "late"))
head(Cov_solea)
unique(Cov_solea$PC)

ggplot(Cov_solea %>% filter(Year==2018, Month==12))+ geom_point(aes(x=x, y=y, color=bottomT))


Cov_seabass <- PC_cov_list[[4]] %>% filter(PC=="PC1")%>% mutate(YoI=ifelse(Year %in% c(2009,2011,2015), "early", "late"))
head(Cov_seabass)
unique(Cov_seabass$PC)


plot1 <- ggplot(Cov_seabass)+
 geom_line(aes(x=Month, y=bottomT, group=Year, color=YoI))#+ 
#scale_colour_viridis_d(option = "plasma")

Cov_Hake_BoB <- PC_cov_list[[2]] %>% filter(PC=="PC1")%>% mutate(YoI=ifelse(Year %in% c(2009,2011,2015), "early", "late"))
head(Cov_Hake_BoB)

plot1 <- ggplot(Cov_Hake_BoB)+
 geom_line(aes(x=Month, y=bottomT, group=Year, color=YoI))#+ 
#scale_colour_viridis_d(option = "plasma")










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

