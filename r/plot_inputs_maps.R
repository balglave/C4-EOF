##################
## Plot input data
##################
# B. Alglave

S_x_month_plot <- list()
Zt_month_plot <- list()
species_name_bob <- c("Sole","Hake","Seabass","Seabass")
for(i in c(1,2,4)){
  
  S_x_df <- S_x_list[[i]] %>% 
    group_by(x,y,Month) %>% 
    summarise(S_x = mean(S_x)) %>% 
    mutate(species = species_name_bob[i])
  
  loc_x <- loc_x_list[[i]]
  
  Zt <- Zt_list[[i]] %>% 
    t()
  Zt_2 <- cbind(loc_x,Zt) %>% 
    data.frame() %>% 
    pivot_longer("X2008_01":"X2018_12") %>% 
    mutate(name = str_sub(name,2,-1),
           Month = as.numeric(str_sub(name,-2,-1))) %>% 
    group_by(x,y,Month) %>% 
    dplyr::summarise(value = mean(value)) %>% 
    mutate(species = species_name_bob[i])
  
  if(species_name_bob[i] == species_name_bob[1]){
    S_x_df_full <- S_x_df
    Zt_full <- Zt_2
    xlims <- range(S_x_df$x)
    ylims <- range(S_x_df$y)
  }else{
    
    S_x_df_full <- rbind(S_x_df_full,S_x_df)
    Zt_full <- rbind(Zt_full,Zt_2)
    
  }
  
  if(i==1) save(data = S_x_df, file = "/home/balglave/Desktop/Research/st-dimension-reduction/data/S_x_df_sole.RData")
  if(i==1) save(data = Zt_2, file = "/home/balglave/Desktop/Research/st-dimension-reduction/data/Zt_2_sole.RData")
  
  ## Plots
  S_x_month_plot[[i]] <- ggplot()+
    geom_point(data=S_x_df,
               aes(x = x, y = y, col = S_x),size = 1,shape=16)+
    scale_color_distiller(palette = "Spectral",trans = "log10")+ 
    theme_bw()+
    xlab("") + ylab("")+
    geom_sf(data = mapBase) +
    coord_sf(xlim = xlims, ylim = ylims, expand = FALSE)+
    theme(legend.title = element_blank(),
          panel.background = element_rect(fill = "white"),
          axis.text.x = element_text(angle = 90))+
    facet_wrap(.~species+Month)
  
  
  Zt_month_plot[[i]] <- ggplot()+
    geom_point(data=Zt_2,
               aes(x = x, y = y, col = value),size = 1,shape=16)+
    scale_color_continuous_divergingx(palette = "Spectral",mid=0,rev=T)+ 
    theme_bw()+
    xlab("") + ylab("")+
    geom_sf(data = mapBase) +
    coord_sf(xlim = xlims, ylim = ylims, expand = FALSE)+
    theme(legend.title = element_blank(),
          panel.background = element_rect(fill = "white"),
          axis.text.x = element_text(angle = 90))+
    facet_wrap(.~species+Month)
  
}

S_x_month_plot_full <- cowplot::plot_grid(S_x_month_plot[[1]],
                                          S_x_month_plot[[2]],
                                          S_x_month_plot[[4]],ncol = 1)

Zt_month_plot_full <- cowplot::plot_grid(Zt_month_plot[[1]],
                                         Zt_month_plot[[2]],
                                         Zt_month_plot[[4]],ncol = 1)

input_full_plot <- cowplot::plot_grid(S_x_month_plot_full,Zt_month_plot_full,ncol = 2)

ggsave("images/input_maps.png",width = 7.5*2,height = 7.5*3,bg = "white")

S_x_month_plot_full <- cowplot::plot_grid(S_x_month_plot[[1]],
                                          ncol = 1)

Zt_month_plot_full <- cowplot::plot_grid(Zt_month_plot[[1]],
                                         ncol = 1)

input_full_plot <- cowplot::plot_grid(S_x_month_plot_full,Zt_month_plot_full,ncol = 2)

ggsave("images/input_maps_sole.png",width = 7.5*2,height = 7.5,bg = "white")
