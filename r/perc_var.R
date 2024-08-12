#########################
## Percentage of variance
#########################
# B. Alglave

for(i in 1:length(main_c)){
  
  perc_var <- eigen_values[[i]]$perc_var
  eigen_vec <- perc_var[order(perc_var,decreasing = T)][1:15]
  eigen_df <- data.frame(perc_var = eigen_vec, dim = 1:15,spp = species_name[i])
  
  if(i==1) eigen_df_full <- eigen_df
  if(i!=1) eigen_df_full <- rbind(eigen_df,eigen_df_full)
  
}


eigen_df_full <- eigen_df_full %>% 
  filter(spp != "Hake_BoB") %>% 
  mutate(spp = ifelse(spp == "Hake_CS","Hake",spp))

eigen_df_full$spp <- factor(eigen_df_full$spp,levels = c(species_name_bob))

vert_bar <- data.frame(dim = c(6.5,2.5,1.5), spp = species_name_bob)
eigen_df_plot <- ggplot(eigen_df_full,
                        aes(x = dim, y = perc_var * 100))+
  geom_bar(stat = "identity",fill="steelblue")+
  facet_wrap(.~factor(spp),scales = "free_y")+
  theme_classic()+
  theme(aspect.ratio = 1)+
  ylab("Percentage of variance (%)")+xlab("Dimension")+
  geom_vline(data=vert_bar,aes(xintercept = dim),
             color = "red",linewidth=0.75,linetype="dashed")

ggsave(filename = "images/eigen_df_plot.png",width = 7.5,height = 3)

tiff("images/eigen_df_plot.tiff",width = 7.5,height = 3, units = 'in', res = 200)
print(eigen_df_plot)
dev.off()
