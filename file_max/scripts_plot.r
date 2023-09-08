
load( "res/S_x_list.RData")
load( "res/EOF_maps.RData")
load( "res/EOF_PC.RData")
load( "res/eigen_values.RData")
load( "res/Zt_list.RData")
load( "res/E_list.RData")
load( "res/loc_x_list.RData")
load( "res/cov_list.RData")
load( "res/loc_x_pred.RData")


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
  EOF_PC_spp_2 <- EOF_PC_spp %>% filter(PC == paste0("PC",j))

  EOF_PC_spp_2_1 <- left_join(EOF_PC_spp_2 %>% filter(Month %in% c(1,2,3)),as_tibble(Year_table))
  EOF_PC_spp_2_2 <- left_join(EOF_PC_spp_2 %>% filter(Month %in% c(9,10,11,12)),as_tibble(Year_table_1))
  EOF_PC_spp_2_2 <- EOF_PC_spp_2_2  %>% mutate(Year_winter=Year_winter_1)
  EOF_PC_spp_3 <- bind_rows(EOF_PC_spp_2_1,EOF_PC_spp_2_2)
  EOF_PC_spp_4 <- EOF_PC_spp_3 %>% mutate(value=if_else( PC=="PC2", value ,-value))  

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

## Hake - BoB
#------------
i=2
EOF_maps_spp <- EOF_maps[[i]] %>%
  filter(EOF %in% paste0("EOF",c(1:n_EOF[i])))
EOF_PC_spp <- EOF_PC[[i]] %>%
  filter(PC %in% paste0("PC",c(1:n_EOF[i])))

for(j in 1:n_EOF[i]){
  
  EOF_maps_spp_2 <- EOF_maps_spp %>% filter(EOF == paste0("EOF",j))
  EOF_PC_spp_2 <- EOF_PC_spp %>% filter(PC == paste0("PC",j))
  
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


## Hake - CS
#------------
i=3

EOF_maps_spp <- EOF_maps[[i]] %>%
  filter(EOF %in% paste0("EOF",c(1:n_EOF[i])))
EOF_PC_spp <- EOF_PC[[i]] %>%
  filter(PC %in% paste0("PC",c(1:n_EOF[i])))

for(j in 1:n_EOF[i]){

  EOF_maps_spp_2 <- EOF_maps_spp %>% filter(EOF == paste0("EOF",j))
  EOF_PC_spp_2 <- EOF_PC_spp %>% filter(PC == paste0("PC",j))
  
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


## Seabass
#---------
i=4

EOF_maps_spp <- EOF_maps[[i]] %>%
  filter(EOF %in% paste0("EOF",c(1:n_EOF[i])))
EOF_PC_spp <- EOF_PC[[i]] %>%
  filter(PC %in% paste0("PC",c(1:n_EOF[i])))

for(j in 1:n_EOF[i]){
  EOF_maps_spp_2 <- EOF_maps_spp %>% filter(EOF == paste0("EOF",j))
  EOF_PC_spp_2 <- EOF_PC_spp %>% filter(PC == paste0("PC",j))
  
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