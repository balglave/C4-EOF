######################
## Sea bass clustering
######################
# B. Alglave

## Clustering on time
i=4
hcpc_data <- as.data.frame(as.matrix(PC_list[[i]]) %*% diag(E_list[[i]]$d))
res_t <- HCPC(hcpc_data,nb.clust = 2,graph = F)
clust_t_df <- data.frame(dim1=res_t$data.clust[,1],
                         dim2=res_t$data.clust[,2],
                         clust=res_t$data.clust$clust,
                         Year_Month=time.step_df$Year_Month)

var_dim_1 <- eigen_df_full$perc_var[which(eigen_df_full$dim == 1 & eigen_df_full$spp == "Seabass")]
var_dim_2 <- eigen_df_full$perc_var[which(eigen_df_full$dim == 2 & eigen_df_full$spp == "Seabass")]

clust_t_plot <- ggplot(data=clust_t_df,
                       aes(x=dim1,y=dim2,
                           col=clust,
                           # shape=trimestre,
                           label=Year_Month))+
  geom_hline(yintercept=0)+geom_vline(xintercept=0)+
  geom_point(size=1)+
  geom_text(nudge_y = 1.4,check_overlap = T)+
  theme_minimal()+scale_color_brewer(palette = "Set1")+
  ylab(paste0("Dim2 - ",round(var_dim_2*100,digits = 1)," %"))+
  xlab(paste0("Dim1 - ",round(var_dim_1*100,digits = 1)," %"))+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5),
        legend.title = element_blank())

ggsave(filename = "images/Dicentrarchus_Labrax/EOF_clust_t_plot_seabass.png",width = 7.5,height = 7.5)

png(paste0("images/Dicentrarchus_Labrax/EOF_t_tree_seabass.png"),res = 75,pointsize = 1/(6*75))

plot(res_t, axes=c(1,2), choice="tree", rect=TRUE, 
     draw.tree=TRUE, ind.names=TRUE, t.level="all", title=NULL,
     new.plot=FALSE, max.plot=15, tree.barplot=TRUE,
     centers.plot=FALSE)

dev.off()

## Clustering on space
hcpc_data <- as.data.frame(V_list[[i]] %*% diag(E_list[[i]]$d))
res_x <- HCPC(hcpc_data,nb.clust = 2 + 2,graph = F)

xlims <- range(pretty(loc_x_list[[i]]$x))
ylims <- range(pretty(loc_x_list[[i]]$y))

clust_x_df <- cbind(loc_x_list[[i]],
                    dim1=res_x$data.clust[,1],
                    dim2=res_x$data.clust[,2],
                    dim3=res_x$data.clust[,3],
                    clust=res_x$data.clust$clust)

clust_map_plot <- ggplot(data=clust_x_df)+
  geom_point(aes(x=x,y=y,col=factor(clust)))+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust=0.5))+
  xlab("Longitude (deg)") + ylab("Latitude (deg)")+
  geom_sf(data = mapBase) +
  coord_sf(xlim = xlims, ylim = ylims, expand = FALSE)+
  theme(plot.title = element_text(hjust=0.5),
        legend.title = element_blank())+
  scale_color_brewer(palette="Set2")

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
  scale_color_brewer(palette="Set2")


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
                                mean_patt_plot,
                                clust_x_plot,
                                clust_map_plot,
                                align = "hv",nrow = 2)

ggsave(paste0("images/Dicentrarchus_Labrax/EOF_clust_x_seabass.png"),
       width=10,height=10)


png(paste0("images/Dicentrarchus_Labrax/EOF_x_tree_seabass.png"),
    res = 75,pointsize = 1/(3*75))

plot(res_x, axes=c(1,2), choice="tree", rect=TRUE, 
     draw.tree=TRUE, ind.names=TRUE, t.level="all", title=NULL,
     new.plot=FALSE, max.plot=15, tree.barplot=TRUE,
     centers.plot=FALSE)

dev.off()

test <- plot_grid(title_sole,clust_plot_sole,
                  title_hake_bob,clust_plot_hake_bob,
                  title_hake_cs,clust_plot_hake_cs,
                  title_seabass,clust_plot_seabass,
                  nrow = 4,rel_widths = c(0.25,1))

ggsave(paste0("images/EOF_clust.png"),
       width=13.5,height=13.5)
