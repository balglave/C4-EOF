###########################
## Hake BoB EOF time series
###########################
i=2
loc_x_pred = loc_x_list[[i]]
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
               aes(x = x, y = y, col = -value),size = 0.75,shape=16)+
    geom_point(data=EOF_maps_spp_2[which(abs(EOF_maps_spp_2$ContribVar) < 1/nrow(loc_x_pred)),],
               aes(x = x, y = y, col = -value),size = 0.5,shape=16,alpha=0.2)+
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
  
  EOF_PC_spp_2 <- EOF_PC_spp %>%
    filter(PC == paste0("PC",j)) %>% 
    left_join(Expected_repro_df[which(Expected_repro_df$Expected_repro_hake == 1),])
  
  EOF_time_series_plot <- ggplot(EOF_PC_spp_2,
                                 aes(x=Year_Month,y=-value,group=PC))+
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

EOF_hake_bob_pres <- plot_grid(list_EOF_PC_hake_bob[[1]]+
                            geom_ribbon(aes(x = Year_Month,
                                            ymin = -Expected_repro_hake*0.1,
                                            ymax = Expected_repro_hake*0.6),
                                        fill = "grey70",alpha = 0.5),
                          list_EOF_map_hake_bob[[1]],
                          list_EOF_PC_hake_bob[[2]],
                          list_EOF_map_hake_bob[[2]],
                          nrow = 2,align = "v",
                          rel_widths = c(1.05,0.5))

ggsave(filename = "images/Merluccius_merluccius_bob/EOF_map_plot.png",width = 30/(1.5*2),height = 15/(1.5 * 2) )

hake_local_perc_var <- ggplot()+
  geom_point(data=EOF_maps_spp %>% filter(EOF %in% c("EOF1","EOF2")), # Only plot the points that most contribute to the dimension
             aes(x = x, y = y, col = ContribVar),size = 0.75,shape=16)+
  scale_color_continuous_divergingx(palette = "Spectral", mid = 0,rev = T,limits = c(0,0.005))+ 
  theme_bw()+
  xlab("") + ylab("")+
  geom_sf(data = mapBase) +
  coord_sf(xlim = c(-6,-1), ylim = c(43,48), expand = FALSE)+
  theme(legend.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90))+
  facet_wrap(.~EOF)+
  ggtitle("Hake")
