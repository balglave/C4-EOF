## Average spatial pattern
#-------------------------

mean_patt_df <- mean_patt[[1]] %>% mutate(species = species_name[1])
xlims <- range(pretty(mean_patt_df$x))
ylims <- range(pretty(mean_patt_df$y))

for(i in 2:4){
  
  toto <- mean_patt[[i]] %>% mutate(species = species_name[i])
  mean_patt_df <- rbind(mean_patt_df,toto)
  
}

mean_patt_df_2 <- mean_patt_df %>% 
  filter(species %in% c("Sole","Hake_BoB","Seabass")) %>% 
  mutate(species = ifelse(species == "Hake_BoB","Hake",species))

mean_patt_df_2$species <- factor(mean_patt_df_2$species,levels = c("Sole","Hake","Seabass"))
mean_patt_plot_sole <- ggplot(mean_patt_df_2 %>% filter(species == "Sole"))+
  geom_point(aes(x = x, y = y, col = spat_mean),shape=16)+
  scale_color_distiller(palette = "Spectral")+ 
  theme_bw()+
  xlab("") + ylab("")+
  geom_sf(data = mapBase) +
  coord_sf(xlim = xlims, ylim = ylims, expand = FALSE)+
  theme(plot.title = element_text(hjust=0.5),
               legend.title = element_blank(),
               panel.spacing.x = unit(10, "mm"))+
  ggtitle("Sole")

mean_patt_plot_hake <- ggplot(mean_patt_df_2 %>% filter(species == "Hake"))+
  geom_point(aes(x = x, y = y, col = spat_mean),shape=16)+
  scale_color_distiller(palette = "Spectral")+ 
  theme_bw()+
  xlab("") + ylab("")+
  geom_sf(data = mapBase) +
  coord_sf(xlim = xlims, ylim = ylims, expand = FALSE)+
  theme(plot.title = element_text(hjust=0.5),
        legend.title = element_blank(),
        panel.spacing.x = unit(10, "mm"))+
  ggtitle("Hake")

mean_patt_plot_seabass <- ggplot(mean_patt_df_2 %>% filter(species == "Seabass"))+
  geom_point(aes(x = x, y = y, col = spat_mean),shape=16)+
  scale_color_distiller(palette = "Spectral")+ 
  theme_bw()+
  xlab("") + ylab("")+
  geom_sf(data = mapBase) +
  coord_sf(xlim = xlims, ylim = ylims, expand = FALSE)+
  theme(plot.title = element_text(hjust=0.5),
        legend.title = element_blank(),
        panel.spacing.x = unit(10, "mm"))+
  ggtitle("Sea Bass")

mean_patt_plot <- cowplot::plot_grid(mean_patt_plot_sole,mean_patt_plot_hake,mean_patt_plot_seabass,ncol = 3,align = "hv")

ggsave(filename = "images/mean_pattern_plot.png",width=10,height=6)
