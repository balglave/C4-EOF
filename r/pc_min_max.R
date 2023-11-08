##############################################
## Minimum and maximum of principal components
##############################################
# B. Alglave

## Build vectors and df to represent the Y during winter months
#--------------------------------------------------------------
Year <- unique(S_x_list[4][[1]]$Year)
Year_winter <- c("2007/2008","2008/2009","2009/2010","2010/2011","2011/2012","2012/2013","2013/2014",
                 "2014/2015","2015/2016","2016/2017","2017/2018")
Year_winter_1 <- c("2008/2009","2009/2010","2010/2011","2011/2012","2012/2013","2013/2014",
                   "2014/2015","2015/2016","2016/2017","2017/2018","2018/2019")

Year_table <- as.data.frame(cbind(Year_winter,Year))
Year_table$Year <- as.integer(Year_table$Year)

Year_table_1 <- as.data.frame(cbind(Year_winter_1,Year))
Year_table_1$Year <- as.integer(Year_table_1$Year)

## Compute minmax
#----------------
# i=1: Sole, i=2: Hake - BoB, i=3: Hake - CS, i=4: Seabass
minmax_PC_df = NULL
for(i in 1:4){
  
  print(species_name[i])
  
  xlims <- range(pretty(EOF_maps[[i]]$x))
  ylims <- range(pretty(EOF_maps[[i]]$y))
  
  EOF_maps_spp <- EOF_maps[[i]] %>%
    filter(EOF %in% paste0("EOF",c(1:n_EOF[i])))
  EOF_PC_spp <- EOF_PC[[i]] %>%
    filter(PC %in% paste0("PC",c(1:n_EOF[i])))
  
  color_name <- ggplotColours(n = n_EOF[i])
  
  for(j in 1:n_EOF[i]){
    
    EOF_maps_spp_2 <- EOF_maps_spp %>% filter(EOF == paste0("EOF",j))
    EOF_PC_spp_2 <- EOF_PC_spp %>% filter(PC == paste0("PC",j))
    EOF_PC_spp_2_1 <- left_join(EOF_PC_spp_2 %>% filter(Month %in% c(1,2,3,3,4,5,6,7,8)),as_tibble(Year_table),by="Year")
    EOF_PC_spp_2_2 <- left_join(EOF_PC_spp_2 %>% filter(Month %in% c(9,10,11,12)),as_tibble(Year_table_1),by="Year")
    # EOF_PC_spp_2_2 <- EOF_PC_spp_2_2  %>% mutate(Year_winter=Year_winter_1)
    EOF_PC_spp_3 <- bind_rows(EOF_PC_spp_2_1,EOF_PC_spp_2_2) %>% 
      mutate(Year_winter = ifelse(is.na(Year_winter_1),Year_winter,Year_winter_1)) %>% 
      dplyr::select(-Year_winter_1)
    EOF_PC_spp_4 <- EOF_PC_spp_3 %>% mutate(value=if_else( PC=="PC2", value ,-value))
    
    for(year in 2008:2018){
      toto1 = EOF_PC_spp_4 %>% filter( (Year %in% c(year)) & (Month %in% c(1,2,3,3,4,5,6,7,8)))
      toto2 = EOF_PC_spp_4 %>% 
        filter((Year %in% c(year-1))&(Month %in% c(9,10,11,12)))
      
      toto <- bind_rows(toto2,toto1)
      temp <- toto  %>% summarise(value=max(value))
      temp2 <- left_join(temp, toto[,c("Year","Year_winter","value","Month","PC")], by = "value")
      
      if(is.null(minmax_PC_df)){
        minmax_PC_df = temp2  %>% mutate(PC = j,species = species_name[i])
      }else{
        titi = temp2  %>% mutate(PC = j,species = species_name[i])
        minmax_PC_df = rbind(minmax_PC_df,titi)
      }
      
    }
    
  }
  
}

# Reproduction occurs during winter months
#---------------------------------------------------
minmax_PC_df_temp1.a <- minmax_PC_df %>% filter(species=="Sole" & PC  %in% c(1,2)) 
minmax_PC_df_temp1.b <-  minmax_PC_df %>% filter(species %in% c("Hake_BoB","Seabass") , PC ==1)
minmax_PC_df_temp1 <- bind_rows(minmax_PC_df_temp1.a,minmax_PC_df_temp1.b)
minmax_PC_df_temp2 <- minmax_PC_df %>% filter(species %in% c("Hake_CS"),PC ==2)

minmax_PC_df_temp3 <- bind_rows(minmax_PC_df_temp1,minmax_PC_df_temp2)

month_name_winter = c("01 - September(t)",
                      "02 - October(t)",
                      "03 - November(t)",
                      "04 - December(t)",
                      "05 - January(t+1)",
                      "06 - February(t+1)",
                      "07 - March(t+1)",
                      "08 - April(t+1)",
                      "09 - May(t+1)",
                      "10 - June(t+1)",
                      "11 _ July(t+1)",
                      "12 - August(t+1)" )

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

pheno_lag_df <- bind_rows(minmax_PC_df_temp3 %>% filter(species %in% c("Sole")), minmax_PC_df_temp3 %>% filter(species %in% c("Hake_BoB","Seabass"),PC==1))

pheno_lag_df$species <- as.character(pheno_lag_df$species)
pheno_lag_df$species[which(pheno_lag_df$species == "Hake_BoB")] <- "Hake"
pheno_lag_df$species <- factor(pheno_lag_df$species,levels = c("Sole","Hake","Seabass"))

pheno_lag_df_2 <- pheno_lag_df %>% 
  dplyr::select(-value) %>% 
  filter(!(species == "Sole" & PC == 1))

# Suitable range
Expected_repro_df_2 <- Expected_repro_df %>% 
  dplyr::select(-Expected_repro_sole_optim,
                -Expected_repro_hake_optim,
                -Expected_repro_seabass_optim,
                -bottomT,-SST) %>% 
  pivot_longer(Expected_repro_sole:Expected_repro_seabass) %>% 
  rename(species = name) %>% 
  mutate(species = ifelse(str_detect(species,"sole"),"Sole",species)) %>% 
  mutate(species = ifelse(str_detect(species,"hake"),"Hake",species)) %>% 
  mutate(species = ifelse(str_detect(species,"seabass"),"Seabass",species))

# # Optimum range
# Expected_repro_df_2 <- Expected_repro_df %>% 
#   dplyr::select(-Expected_repro_sole_optim,
#                 -Expected_repro_hake_optim,
#                 -Expected_repro_seabass_optim,
#                 -bottomT,-SST) %>% 
#   pivot_longer(Expected_repro_sole_optim:Expected_repro_seabass_optim) %>% 
#   rename(species = name) %>% 
#   mutate(species = ifelse(str_detect(species,"sole"),"Sole",species)) %>% 
#   mutate(species = ifelse(str_detect(species,"hake"),"Hake",species)) %>% 
#   mutate(species = ifelse(str_detect(species,"seabass"),"Seabass",species))

Month_df <- data.frame(Month = c(9,10,11,12,1,2,3,4,5,6,7,8),
                       Months = month_name_winter)

Expected_repro_df_3 <- Expected_repro_df_2 %>% 
  inner_join(Month_df) %>% 
  mutate(Year_winter = ifelse(Month<10,paste0(Year-1,"/",Year),paste0(Year,"/",Year+1))) %>% 
  filter(value == 1)

pheno_lag_df_2$Months <- factor(pheno_lag_df_2$Months,levels = month_name_winter)

Expected_repro_df_3$Months <- factor(Expected_repro_df_3$Months,levels = month_name_winter)
pheno_lag_df_2 <- pheno_lag_df_2 %>% mutate(indic_phen=1)
Expected_repro_df_3 <- Expected_repro_df_3 %>%  mutate(indic_max=1)

Expected_repro_df_4 <- Expected_repro_df_3 %>% 
  mutate(Year_Month = paste0(Year,"_",ifelse(Month>9,Month,paste0("0",Month))))

plot1 <- full_join(Expected_repro_df_4,pheno_lag_df_2)

plot1$species <- factor(plot1$species,levels=c("Sole","Hake","Seabass"))


# Problem with September of year 2011/2012
# In fact, reproduction occurs on February
plot1$indic_phen[which(plot1$species == "Hake" &
                         plot1$Year_winter == "2011/2012" &
                         plot1$Months == "01 - September(t)")] <- NA

plot1$indic_phen[which(plot1$species == "Hake" &
                         plot1$Year_winter == "2011/2012" &
                         plot1$Months == "06 - February(t+1)")] <- 1

minmax_PC_df_plot = ggplot()+
  facet_grid(rows = vars(species))+
  geom_point(data = plot1[which(plot1$indic_phen == 1),], aes(y=factor(Months),x=Year_winter))+
  geom_line(data = plot1[which(plot1$indic_phen == 1),], aes(y=factor(Months),x=Year_winter,group=1))+
  # ggtitle("Minimum principal component value")+
  theme_bw()+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust = 0.5,face = "bold"),
        axis.text.x = element_text(angle = 90,vjust = 0.5))+
  ylab("")+xlab("") +
  geom_point(data = plot1[which(plot1$indic_max == 1),],
             aes(y=Months,x=Year_winter),col="grey",size=3,alpha=0.5)


# -- Represent th
ggsave(paste0("images/minmax_PC_plot.png"),
       width=3,height=6)


save(minmax_PC_df_temp3, file="res/minmax_PC_df_temp3.RData")

# ## Compare median of the range to maximum of the PC
cor_measure = plot1 %>%
  filter(Year > 2008) %>%
  # filter(indic_phen == 1) %>%
  mutate(rk_month = as.numeric(str_sub(Months,1,2))) %>%
  group_by(Year_winter,species) %>%
  dplyr::summarise(indic_repro_period = min(rk_month,na.rm=T)) %>%
  inner_join(pheno_lag_df_2)

## Correlation measures
cor_measure %>%
  group_by(species) %>%
  summarise(r2 = cor(indic_repro_period,Month,method="spearman"),
            p_val = cor.test(indic_repro_period,Month,method="spearman")$p.value)

ggplot(cor_measure)+
  geom_point(aes(x=indic_repro_period,y=Month))+
  facet_wrap(.~species)

## V cramer
png(file="images/beginrepro_vs_repromonth.png",
    width=600, height=500)
par(mfrow = c(3,2))
for(sp_i in c("Sole","Hake","Seabass")){
  
  print("----------------------------------")
  print(paste0(sp_i," --------------------------"))
  cor_measure_2 <- cor_measure %>% 
    filter(species == sp_i)
  Begin_repro <- as.factor(cor_measure_2$indic_repro_period)
  Repro_month <- as.factor(cor_measure_2$Months)
  test <- data.frame(Repro_month = as.character(Repro_month),
                     Begin_repro = as.character(Begin_repro))
  
  ## Count matrix (quali variable)
  matrix_date <- as.data.frame(table(test)) %>% 
    pivot_wider(names_from = Begin_repro,values_from = Freq) %>% 
    dplyr::select(-Repro_month) %>% 
    as.matrix()
  col.vec <- rev(terrain.colors(1+max(matrix_date)))
  par(mar=c(5.1, 4.1, 4.1, 4.1)) # adapt margins
  plot(matrix_date,
       main = sp_i,
       ylab = "First favorable month", xlab = "Reproduction month",
       axis.col=list(side=1, las=2), axis.row = list(side=2, las=1),
       na.col="white",col=col.vec,breaks=0:length(col.vec))
  
  ## V of Cramer
  test_chisq <- chisq.test(Repro_month,Begin_repro)
  # print(test_chisq)
  print(cramersv(test_chisq))
  print(ci_cramersv(test_chisq,type = "bootstrap"))
  
  ## Quanti variable
  test2 <- data.frame(Repro_month2 = as.numeric(str_sub(Repro_month,1,2)),
                      Begin_repro2 = as.numeric(as.character(Begin_repro)))
  
  plot(y=test2$Repro_month2,x=test2$Begin_repro2)
  
  print(cor.test(test2$Repro_month2,test2$Begin_repro2,method="spearman")$estimate)
  print(cor.test(test2$Repro_month2,test2$Begin_repro2,method="spearman")$p.val)
  
}
dev.off()
