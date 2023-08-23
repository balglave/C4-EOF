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
