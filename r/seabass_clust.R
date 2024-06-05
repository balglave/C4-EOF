######################
## Sea bass clustering
######################
# B. Alglave

## Clustering on time
i=4
hcpc_data <- as.data.frame(as.matrix(PC_list[[i]]) %*% diag(E_list[[i]]$d))
res_t <- HCPC(-hcpc_data,nb.clust = 2,graph = F)
clust_t_df <- data.frame(dim1=res_t$data.clust[,1],
                         dim2=res_t$data.clust[,2],
                         clust=res_t$data.clust$clust,
                         Year_Month=time.step_df$Year_Month)


clust_t_df$dim1_stand <- clust_t_df$dim1 / (range(clust_t_df$dim1)[2] - range(clust_t_df$dim1)[1])
clust_t_df$dim2_stand <- clust_t_df$dim2 / (range(clust_t_df$dim2)[2] - range(clust_t_df$dim2)[1])

var_dim_1 <- eigen_df_full$perc_var[which(eigen_df_full$dim == 1 & eigen_df_full$spp == "Seabass")]
var_dim_2 <- eigen_df_full$perc_var[which(eigen_df_full$dim == 2 & eigen_df_full$spp == "Seabass")]


clust_t_plot <- ggplot(data=clust_t_df)+
  geom_hline(yintercept=0)+geom_vline(xintercept=0)+
  geom_point(aes(x=dim1,y=dim2,
                 col=clust,
                 # shape=trimestre,
                 label=Year_Month),size=0.3,alpha=0.4)+
  geom_text(data=clust_t_df, # [sample(x = 1:nrow(clust_t_df),size = 30,replace = F),]
            aes(x=dim1,y=dim2,label=Year_Month,col=clust),check_overlap = T)+
  theme_minimal()+scale_color_brewer(palette = "Set1")+
  ylab(paste0("Dim2 - ",round(var_dim_2*100,digits = 1)," %"))+
  xlab(paste0("Dim1 - ",round(var_dim_1*100,digits = 1)," %"))+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5),
        legend.title = element_blank())+
  ggtitle("Time steps clusters")

ggsave(filename = "images/Dicentrarchus_Labrax/EOF_clust_t_plot_seabass.png",width = 7.5,height = 7.5)

png(paste0("images/Dicentrarchus_Labrax/EOF_t_tree_seabass.png"),res = 75)

plot(res_t, axes=c(1,2), choice="tree", rect=TRUE, 
     draw.tree=TRUE, ind.names=TRUE, t.level="all", title=NULL,
     new.plot=FALSE, max.plot=15, tree.barplot=TRUE,
     centers.plot=FALSE)

dev.off()

## Clustering on space
hcpc_data <- as.data.frame(V_list[[i]] %*% diag(E_list[[i]]$d))
res_x <- HCPC(-hcpc_data,nb.clust = 2 ,graph = F)

xlims <- range(pretty(loc_x_list[[i]]$x))
ylims <- range(pretty(loc_x_list[[i]]$y))


clust_x_df <- cbind(loc_x_list[[i]],
                    dim1=res_x$data.clust[,1],
                    dim2=res_x$data.clust[,2],
                    dim3=res_x$data.clust[,3],
                    clust=res_x$data.clust$clust)

clust_x_df$dim1_stand <- clust_x_df$dim1/(range(clust_x_df$dim1)[2]-range(clust_x_df$dim1)[1])
clust_x_df$dim2_stand <- clust_x_df$dim2/(range(clust_x_df$dim2)[2]-range(clust_x_df$dim2)[1])
clust_x_df$dim3_stand <- clust_x_df$dim3/(range(clust_x_df$dim3)[2]-range(clust_x_df$dim3)[1])

clust_map_plot <- ggplot(data=clust_x_df)+
  geom_point(aes(x=x,y=y,col=factor(clust)))+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust=0.5))+
  xlab("Longitude (deg)") + ylab("Latitude (deg)") +
  geom_sf(data = mapBase) +
  coord_sf(xlim = xlims, ylim = ylims, expand = FALSE)+
  theme(plot.title = element_text(hjust=0.5),
        legend.title = element_blank())+
  ggtitle("Locations clusters")+
  scale_color_manual(values = c("#FC8D59","skyblue"))

  # scale_color_brewer(palette="Spectral")

clust_x_plot <- ggplot(data=clust_x_df,
                       aes(x=dim1,y=dim2,
                           col=clust))+
  geom_hline(yintercept=0)+geom_vline(xintercept=0)+
  geom_point()+
  theme_minimal()+
  xlab(paste0("Dim1 - ",round(var_dim_1*100,digits = 1)," %"))+
  ylab(paste0("Dim2 - ",round(var_dim_2*100,digits = 1)," %"))+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5),
        legend.title = element_blank())+
  ggtitle("Locations clusters")+
  scale_color_manual(values = c("#FC8D59","skyblue"))

  # scale_color_brewer(palette="Spectral")


mean_patt_plot <- ggplot(data = mean_patt[[i]])+
  geom_point(aes(x=x,y=y,col=spat_mean))+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust=0.5))+
  xlab("Longitude (deg)") + ylab("Latitude (deg)") +
  geom_sf(data = mapBase) +
  coord_sf(xlim = xlims, ylim = ylims, expand = FALSE)+
  theme(plot.title = element_text(hjust=0.5),
        legend.title = element_blank())+
  ggtitle("Mean pattern")+
  scale_color_distiller(palette="Spectral")


clust_plot_seabass <- plot_grid(clust_t_plot,
                                # mean_patt_plot,
                                clust_x_plot,
                                clust_map_plot,
                                ncol = 3)

ggsave(paste0("images/Dicentrarchus_Labrax/EOF_clust_x_seabass.png"),
       width=15,height=5)


png(paste0("images/Dicentrarchus_Labrax/EOF_x_tree_seabass.png"),
    res = 75)

plot(res_x, axes=c(1,2), choice="tree", rect=TRUE, 
     draw.tree=TRUE, ind.names=TRUE, t.level="all", title=NULL,
     new.plot=FALSE, max.plot=15, tree.barplot=TRUE,labels=F,
     centers.plot=FALSE)

dev.off()



## Plot both time steps and locations
colnames(clust_x_df)[7] <- "cluster_locations"
colnames(clust_t_df)[3] <- "cluster_time.steps"
plot1 <- ggplot()+
  geom_point(data=clust_x_df,
             aes(x=dim1_stand,y=dim2_stand,
                 col=cluster_locations),alpha=0.75)+
  scale_color_manual(values = c("#FC8D59","skyblue"))

clust_t_df$Quarter <- NA
clust_t_df <- clust_t_df %>% 
  mutate(month = as.numeric(str_sub(Year_Month,-2,-1))) %>% 
  mutate(Quarter = ifelse(month %in% 1:3,1,Quarter)) %>% 
  mutate(Quarter = ifelse(month %in% 4:6,2,Quarter)) %>% 
  mutate(Quarter = ifelse(month %in% 7:9,3,Quarter)) %>% 
  mutate(Quarter = ifelse(month %in% 10:12,4,Quarter))

clust_t_df$Quarter <- as.factor(clust_t_df$Quarter)

clust_tx_seabass <- plot1 + 
  new_scale_colour() +
  scale_shape_manual(values = c(21:24))+
  geom_point(data=clust_t_df,
             aes(x=dim1_stand,y=dim2_stand,
                 fill=cluster_time.steps,shape=Quarter),size=2.5)+
  geom_point(data=clust_t_df,
             aes(x=dim1_stand,y=dim2_stand,
                 col=cluster_time.steps),size=1.25)+
  scale_color_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")+
  # stat_ellipse(data=clust_t_df,
  #              aes(x=dim1_stand,y=dim2_stand,
  #                  col=cluster_time.steps))+
  # new_scale_colour() +
  # stat_ellipse(data=clust_t_df,
  #              aes(x=dim1_stand,y=dim2_stand,
  #                  col=Quarter))+
  # geom_text(data=clust_t_df, # [sample(x = 1:nrow(clust_t_df),size = 30,replace = F),]
  #           aes(x=dim1_stand - 0.11,y=dim2_stand,label=Year_Month),
  #           check_overlap = T,size=4,fontface = "bold")+
  geom_hline(yintercept=0)+geom_vline(xintercept=0)+
  theme_minimal()+
  xlab(paste0("Dim1 - ",round(var_dim_1*100,digits = 1)," %"))+
  ylab(paste0("Dim2 - ",round(var_dim_2*100,digits = 1)," %"))+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5))+
  ggtitle("Locations and time steps clusters")+
  guides(shape = guide_legend(order = 2))

clust_tx_seabass2 <- plot_grid(clust_map_plot+
                              theme(text = element_text(size=8)),
                            clust_tx_seabass,
                            ncol = 2,rel_widths = c(1/3,2/3))

ggsave(paste0("images/Dicentrarchus_Labrax/clust_tx.png"),
       width=8,height=8)

