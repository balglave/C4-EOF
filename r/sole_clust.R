##################
## Sole Clustering
##################
## Clustering on time
i=1
trimestre <- as.factor(rep(c(1,1,1,2,2,2,3,3,3,4,4,4),11))
mois <- as.factor(rep(c(1:12),11))
hcpc_data <- as.data.frame(as.matrix(PC_list[[i]]) %*% diag(E_list[[i]]$d)) # project time steps
res_t <- HCPC(hcpc_data,nb.clust = 3,graph = F)
clust_t_df <- data.frame(dim1=res_t$data.clust[,1],
                         dim2=res_t$data.clust[,2],
                         clust=res_t$data.clust$clust,
                         Year_Month=time.step_df$Year_Month,
                         Quarter=time.step_df$Quarter,
                         dim1_stand=PC_list[[i]]$X1,
                         dim2_stand=PC_list[[i]]$X2)

var_dim_1 <- eigen_df_full$perc_var[which(eigen_df_full$dim == 1 & eigen_df_full$spp == "Sole")]
var_dim_2 <- eigen_df_full$perc_var[which(eigen_df_full$dim == 2 & eigen_df_full$spp == "Sole")]

clust_t_plot <- ggplot(data=clust_t_df,
                       aes(x=dim1,y=dim2,
                           col=clust,
                           # shape=trimestre,
                           label=Year_Month))+
  geom_text()+
  geom_hline(yintercept=0)+geom_vline(xintercept=0)+
  geom_point(size=1)+
  geom_label(nudge_y = 1.4)+
  theme_minimal()+scale_color_brewer(palette = "Set1")+
  ylab(paste0("Dim2 - ",round(var_dim_2*100,digits = 1)," %"))+
  xlab(paste0("Dim1 - ",round(var_dim_1*100,digits = 1)," %"))+
  stat_ellipse()+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5),
        legend.title = element_blank())+
  ggtitle("Time steps clusters")

ggsave(filename = "images/Solea_solea/EOF_clust_t_plot.png",width = 7.5,height = 7.5)

png(paste0("images/Solea_solea/EOF_t_tree_Sole.png"),res = 75)

plot(res_t, axes=c(1,2), choice="tree", rect=TRUE, 
     draw.tree=TRUE, ind.names=F, t.level="all", title=NULL,
     new.plot=FALSE, max.plot=15, tree.barplot=TRUE,labels=F,
     centers.plot=FALSE)

dev.off()

## Clustering on space
hcpc_data <- as.data.frame(V_list[[i]] %*% diag(E_list[[i]]$d))  # project eigen vectors
res_x <- HCPC(hcpc_data,nb.clust = 6,graph = F) # 2 or 6

xlims <- range(pretty(loc_x_list[[i]]$x))
ylims <- range(pretty(loc_x_list[[i]]$y))

clust_x_df <- cbind(loc_x_list[[i]],
                    dim1=res_x$data.clust[,1],
                    dim2=res_x$data.clust[,2],
                    dim3=res_x$data.clust[,3],
                    clust=res_x$data.clust$clust,
                    dim1_stand=V_list[[i]][,1],
                    dim2_stand=V_list[[i]][,2],
                    dim3_stand=V_list[[i]][,3])

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
  ggtitle("Spatial clusters")+
  scale_color_manual(values = c("#D53E4F","#FC8D59","#FEE08B","#E6F598","skyblue","#3288BD"))

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
  scale_color_manual(values = c("#D53E4F","#FC8D59","#FEE08B","#E6F598","skyblue","#3288BD"))

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

clust_plot_sole <- plot_grid(clust_t_plot,
                             mean_patt_plot,
                             clust_x_plot,
                             clust_map_plot,
                             align = "hv",nrow = 2)

ggsave(paste0("images/Solea_solea/EOF_clust_x_sole.png"),
       width=10,height=10)


png(paste0("images/Solea_solea/EOF_x_tree_sole.png"),
    res = 75)

plot.HCPC(res_x, axes=c(1,2), choice="tree", rect=TRUE, 
     draw.tree=TRUE, ind.names=TRUE, t.level="all", title=NULL,
     new.plot=FALSE, max.plot=15, tree.barplot=TRUE,labels=F,
     centers.plot=FALSE,col="black")

dev.off()

## Plot projections plans for time steps and locations
clust_x_df$dim1_stand <- clust_x_df$dim1/(range(clust_x_df$dim1)[2]-range(clust_x_df$dim1)[1])
clust_x_df$dim2_stand <- clust_x_df$dim2/(range(clust_x_df$dim2)[2]-range(clust_x_df$dim2)[1])
clust_x_df$dim3_stand <- clust_x_df$dim3/(range(clust_x_df$dim3)[2]-range(clust_x_df$dim3)[1])

clust_t_df$dim1_stand <- clust_t_df$dim1 / (range(clust_t_df$dim1)[2] - range(clust_t_df$dim1)[1])
clust_t_df$dim2_stand <- clust_t_df$dim2 / (range(clust_t_df$dim2)[2] - range(clust_t_df$dim2)[1])

proj_loc_plot <- ggplot()+
  geom_point(data=clust_x_df,
             aes(x=dim1,y=dim2,
                 col=clust),alpha=0.75)+
  geom_hline(yintercept=0)+geom_vline(xintercept=0)+
  theme_minimal()+
  xlab(paste0("Dim1 - ",round(var_dim_1*100,digits = 1)," %"))+
  ylab(paste0("Dim2 - ",round(var_dim_2*100,digits = 1)," %"))+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5),
        legend.title = element_blank())+
  scale_color_manual(values = c("#D53E4F","#FC8D59","#FEE08B","#E6F598","skyblue","#3288BD"))+
  ggtitle("Locations clusters")


proj_ts_plot <- ggplot()+
  geom_point(data=clust_t_df,
   aes(x=dim1,y=dim2,
       col=clust),size=0.3,alpha=0.35)+
  geom_text(data=clust_t_df, # [sample(x = 1:nrow(clust_t_df),size = 30,replace = F),]
                  aes(x=dim1,y=dim2,label=Year_Month,col=clust),check_overlap = T)+
  geom_hline(yintercept=0)+geom_vline(xintercept=0)+
  theme_minimal()+
  xlab(paste0("Dim1 - ",round(var_dim_1*100,digits = 1)," %"))+
  ylab(paste0("Dim2 - ",round(var_dim_2*100,digits = 1)," %"))+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5),
        legend.title = element_blank())+
  scale_color_brewer(palette = "Set1")+
  ggtitle("Time steps clusters")

proj_map_plot_sole <- plot_grid(proj_ts_plot,
                           proj_loc_plot,
                           clust_map_plot,
                           align = "v",ncol = 3)

ggsave(paste0("images/Solea_solea/proj_map_plot.png"),
       width=15,height=5)


## Plot evolution of biomass in each cluster
trend_clust <- inner_join(S_x_list[[i]],
                          clust_x_df[,c("cell","x","y","clust")]) %>% 
  group_by(clust,Year_Month,Year,Month) %>% 
  dplyr::summarise(S_x = sum(S_x))
  
evol_ab_clust <- ggplot(trend_clust,
       aes(x=Year_Month,y=S_x,col=clust,group=clust))+
  geom_line()+
  geom_vline(xintercept=trend_clust$Year_Month[which(str_detect(trend_clust$Year_Month,"_01"))],
                                     linetype="dashed", color = "skyblue", linewidth = 1)+
  geom_line(linewidth=1)+
  theme_bw()+
  xlab("")+ylab("")+
  theme()+
  theme(axis.text.x = element_blank(),
        legend.position = "bottom",
        aspect.ratio = c(1/4),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"),
        plot.title = element_text(hjust=0.5,face = "bold"),
        legend.title = element_blank())+
  geom_text(aes(x = -Inf, y = -Inf, label = "2008"),col= "black", hjust = -.10, vjust = 2)+
  geom_text(aes(x = max(Year_Month),
                y = -Inf, label = "2018"),
            col= "black", hjust = 1, vjust = 2)+
  coord_cartesian(clip = "off")+
  facet_wrap(.~clust,ncol=2,scales = "free_y",dir = "v")+
  scale_color_manual(values = c("#D53E4F","#FC8D59","#FEE08B","#E6F598","skyblue","#3288BD"))+
  ylim(0,NA)

ggsave(paste0("images/Solea_solea/evol_ab_clust.png"),
       width=10,height=6)


## Plot both time steps and locations
colnames(clust_x_df)[7] <- "Spatial_clusters"
colnames(clust_t_df)[3] <- "cluster_time.steps"

plot1 <- ggplot()+
  geom_point(data=clust_x_df,
             aes(x=dim1_stand,y=dim2_stand,
                 col=Spatial_clusters),alpha=0.75)+
  scale_color_manual(values = c("#D53E4F","#FC8D59","#FEE08B","#E6F598","skyblue","#3288BD"))
  # scale_color_manual(values = c("#D53E4F","#FC8D59","#FEE08B","#E6F598","skyblue","#3288BD"))

clust_t_df$Quarter <- as.factor(clust_t_df$Quarter)


clust_tx_sole <- plot1 + 
  geom_point(data=clust_t_df,
             aes(x=dim1_stand,y=dim2_stand,
                 fill=cluster_time.steps),shape = 22,size=2.5)+
  scale_fill_brewer(palette = "Set1")+
  geom_hline(yintercept=0)+geom_vline(xintercept=0)+
  theme_minimal()+
  xlab(paste0("Dim1 - ",round(var_dim_1*100,digits = 1)," %"))+
  ylab(paste0("Dim2 - ",round(var_dim_2*100,digits = 1)," %"))+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5))+
  ggtitle("Locations and time steps clusters")+
  guides(shape = guide_legend(order = 2))

clust_maptx_sole <- plot_grid(clust_map_plot+
                                 theme(text = element_text(size=8)),
                               clust_tx_sole,
                            ncol = 2,rel_widths = c(1/3,2/3))

ggsave(paste0("images/Solea_solea/clust_tx.png"),
       width=8,height=8)

##--------------------------------------------------------------------
# With polygons

for(i in 1:6){
  
  clust_x_df_2 <- clust_x_df %>% filter(Spatial_clusters == i)
  chull_res <- chull(clust_x_df_2$dim1,clust_x_df_2$dim2)
  
  if(i == 1) clust_x_df_3 <- clust_x_df_2[chull_res,]
  if(i != 1) clust_x_df_3 <- rbind(clust_x_df_3,clust_x_df_2[chull_res,])
  
}

plot2 <- ggplot()+
  geom_polygon(data=clust_x_df_3,
               aes(x=dim1_stand,y=dim2_stand,
                   fill=Spatial_clusters),alpha=0.65,col=NA)+
  # geom_point(data=clust_x_df,
  #            aes(x=dim1_stand,y=dim2_stand,
  #                col=Spatial_clusters),shape = 20,size=0.2)+
  scale_fill_manual(values = c("#D53E4F","#FC8D59","#FEE08B","#E6F598","skyblue","#3288BD"))+
  scale_color_manual(values = c("#D53E4F","#FC8D59","#FEE08B","#E6F598","skyblue","#3288BD"))

clust_t_df$Season <- NA
clust_t_df$Season[which(clust_t_df$cluster_time.steps == 1)] <- "1: winter"
clust_t_df$Season[which(clust_t_df$cluster_time.steps == 2)] <- "3: autumn"
clust_t_df$Season[which(clust_t_df$cluster_time.steps == 3)] <- "2: spring and summer"
clust_t_df$Season <- factor(clust_t_df$Season,levels = c("1: winter",
                                                         "2: spring and summer",
                                                         "3: autumn"))

clust_tx_sole2 <- plot2 + 
  ggnewscale::new_scale_fill()+
  geom_point(data=clust_t_df,
             aes(x=dim1_stand,y=dim2_stand,
                 fill=Season),shape = 22,size=2.5)+
  scale_fill_manual(values = c("#E41A1C","#4DAF4A","#377EB8"))+
  # scale_fill_brewer(palette = "Set1")+
  geom_hline(yintercept=0)+geom_vline(xintercept=0)+
  theme_minimal()+
  xlab(paste0("Dim1 - ",round(var_dim_1*100,digits = 1)," %"))+
  ylab(paste0("Dim2 - ",round(var_dim_2*100,digits = 1)," %"))+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5))+
  ggtitle("Spatial clusters and seasons")+
  guides(shape = guide_legend(order = 2))

clust_maptx_sole2 <- plot_grid(clust_map_plot+
                                 theme(text = element_text(size=8)),
                               clust_tx_sole2,
                               ncol = 2,rel_widths = c(1/3,2/3))

clust_t_sole3 <- ggplot()+
  geom_point(data=clust_t_df,
             aes(x=dim1_stand,y=dim2_stand,
                 fill=Season),shape = 22,size=2.5)+
  scale_fill_brewer(palette = "Set1")+
  geom_hline(yintercept=0)+geom_vline(xintercept=0)+
  theme_minimal()+
  xlab(paste0("Dim1 - ",round(var_dim_1*100,digits = 1)," %"))+
  ylab(paste0("Dim2 - ",round(var_dim_2*100,digits = 1)," %"))+
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust=0.5))+
  ggtitle("Sole")+
  guides(shape = guide_legend(order = 2))+
  geom_text(data=clust_t_df, # [sample(x = 1:nrow(clust_t_df),size = 30,replace = F),]
            aes(x=dim1_stand - 0.11,y=dim2_stand,label=Year_Month),
            check_overlap = T,size=4,fontface = "bold")
