##############################################
## Minimum and maximum of principal components
##############################################
# B. Alglave

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

test = minmax_PC_df_2 %>% 
  filter(PC == 2 & species == "Hake_BoB")

year_vs_month_plot = ggplot(test)+
  geom_point(aes(y=Month_max_name,x=Year))+
  ggtitle("Reproduction period")+
  theme_bw()+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust = 0.5,face = "bold"),
        axis.text.x = element_text(angle = 90,vjust = 0.5))+
  ylab("")+xlab("")

ggsave(filename = "C:/Users/test/Desktop/presentation/pres_vannes/images/year_vs_month_plot.png",
       width=3,height=3)



#-----------------------------------------
# Reproduction occurs during winter months
#-----------------------------------------
Year <- unique(S_x_list[4][[1]]$Year)
Year_winter <- c("2007/2008","2008/2009","2009/2010","2010/2011","2011/2012","2012/2013","2013/2014",
            "2014/2015","2015/2016","2016/2017","2017/2018")
Year_winter_1 <- c("2008/2009","2009/2010","2010/2011","2011/2012","2012/2013","2013/2014",
            "2014/2015","2015/2016","2016/2017","2017/2018","2018/2019")

Year_table <- as.data.frame(cbind(Year_winter,Year))
Year_table$Year <- as.integer(Year_table$Year)

Year_table_1 <- as.data.frame(cbind(Year_winter_1,Year))
Year_table_1$Year <- as.integer(Year_table_1$Year)

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


## Rename month for maximum
minmax_PC_df_temp3$Month_max[which(minmax_PC_df_temp3$Month_max == 1)] = month_name_winter[5]
minmax_PC_df_temp3$Month_max[which(minmax_PC_df_temp3$Month_max == 2)] = month_name_winter[6]
minmax_PC_df_temp3$Month_max[which(minmax_PC_df_temp3$Month_max == 3)] = month_name_winter[7]
minmax_PC_df_temp3$Month_max[which(minmax_PC_df_temp3$Month_max == 4)] = month_name_winter[8]
minmax_PC_df_temp3$Month_max[which(minmax_PC_df_temp3$Month_max == 5)] = month_name_winter[9]
minmax_PC_df_temp3$Month_max[which(minmax_PC_df_temp3$Month_max == 6)] = month_name_winter[10]
minmax_PC_df_temp3$Month_max[which(minmax_PC_df_temp3$Month_max == 7)] = month_name_winter[11]
minmax_PC_df_temp3$Month_max[which(minmax_PC_df_temp3$Month_max == 8)] = month_name_winter[12]
minmax_PC_df_temp3$Month_max[which(minmax_PC_df_temp3$Month_max == 9)] = month_name_winter[1]
minmax_PC_df_temp3$Month_max[which(minmax_PC_df_temp3$Month_max == 10)] = month_name_winter[2]
minmax_PC_df_temp3$Month_max[which(minmax_PC_df_temp3$Month_max == 11)] = month_name_winter[3]
minmax_PC_df_temp3$Month_max[which(minmax_PC_df_temp3$Month_max == 12)] = month_name_winter[4]

minmax_PC_df_temp3$Month_max = factor(minmax_PC_df_temp3$Month_max,levels = (month_name_winter))


## Rename month for minimum
minmax_PC_df_temp3$Month_min[which(minmax_PC_df_temp3$Month_min == 1)] = month_name_winter[5]
minmax_PC_df_temp3$Month_min[which(minmax_PC_df_temp3$Month_min == 2)] = month_name_winter[6]
minmax_PC_df_temp3$Month_min[which(minmax_PC_df_temp3$Month_min == 3)] = month_name_winter[7]
minmax_PC_df_temp3$Month_min[which(minmax_PC_df_temp3$Month_min == 4)] = month_name_winter[8]
minmax_PC_df_temp3$Month_min[which(minmax_PC_df_temp3$Month_min == 5)] = month_name_winter[9]
minmax_PC_df_temp3$Month_min[which(minmax_PC_df_temp3$Month_min == 6)] = month_name_winter[10]
minmax_PC_df_temp3$Month_min[which(minmax_PC_df_temp3$Month_min == 7)] = month_name_winter[11]
minmax_PC_df_temp3$Month_min[which(minmax_PC_df_temp3$Month_min == 8)] = month_name_winter[12]
minmax_PC_df_temp3$Month_min[which(minmax_PC_df_temp3$Month_min == 9)] = month_name_winter[1]
minmax_PC_df_temp3$Month_min[which(minmax_PC_df_temp3$Month_min == 10)] = month_name_winter[2]
minmax_PC_df_temp3$Month_min[which(minmax_PC_df_temp3$Month_min == 11)] = month_name_winter[3]
minmax_PC_df_temp3$Month_min[which(minmax_PC_df_temp3$Month_min == 12)] = month_name_winter[4]

minmax_PC_df_temp3$Month_min = factor(minmax_PC_df_temp3$Month_min,levels = (month_name_winter))

Year_winter_1

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

