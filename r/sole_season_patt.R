##############################
## Seasonnal patterns for sole
##############################
# B. Alglave

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