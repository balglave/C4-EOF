#################
## Hake BoB Clust
#################
# B. Alglave

## Clustering on time
i=2
hcpc_data <- as.data.frame(as.matrix(PC_list[[i]]) %*% diag(E_list[[i]]$d))
res_t <- HCPC(-hcpc_data,nb.clust = 2,graph = F)
clust_t_df <- data.frame(dim1=res_t$data.clust[,1],
                         dim2=res_t$data.clust[,2],
                         clust=res_t$data.clust$clust,
                         Year_Month=time.step_df$Year_Month)

var_dim_1 <- eigen_df_full$perc_var[which(eigen_df_full$dim == 1 & eigen_df_full$spp == "Hake")]
var_dim_2 <- eigen_df_full$perc_var[which(eigen_df_full$dim == 2 & eigen_df_full$spp == "Hake")]

clust_t_plot <- ggplot(data=clust_t_df)+
  geom_hline(yintercept=0)+geom_vline(xintercept=0)+
  geom_point(aes(x=dim1,y=dim2,
                 col=clust,
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

ggsave(filename = "images/Merluccius_merluccius_bob/EOF_clust_t_plot_hake_bob.png",width = 7.5,height = 7.5)

png(paste0("images/Merluccius_merluccius_bob/EOF_t_tree_hake_bob.png"),res = 75)

plot(res_t, axes=c(1,2), choice="tree", rect=TRUE, 
     draw.tree=TRUE, ind.names=TRUE, t.level="all", title=NULL,
     new.plot=FALSE, max.plot=15, tree.barplot=TRUE,labels=F,
     centers.plot=FALSE)

dev.off()

## Clustering on space
hcpc_data <- as.data.frame(V_list[[i]] %*% diag(E_list[[i]]$d))
res_x <- HCPC(-hcpc_data,nb.clust = 2,graph = F)

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

clust_plot_hake_bob <- plot_grid(clust_t_plot,
                                 # mean_patt_plot,
                                 clust_x_plot,
                                 clust_map_plot,
                                 ncol = 3)

ggsave(paste0("images/Merluccius_merluccius_bob/EOF_clust_x_plot_hake_bob.png"),
       width=15,height=5)


png(paste0("images/Merluccius_merluccius_bob/EOF_x_tree_hake_bob.png"),
    res = 75)

plot(res_x, axes=c(1,2), choice="tree", rect=TRUE, 
     draw.tree=TRUE, ind.names=TRUE, t.level="all", title=NULL,
     new.plot=FALSE, max.plot=15, tree.barplot=TRUE,labels=F,
     centers.plot=FALSE)

dev.off()
