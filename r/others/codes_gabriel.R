########################################
## EOFs on reproduction period (Sebass)
########################################

source("r/library_functions.R")

## Load data
load("data/Dicentrarchus_labrax/S_x_df_Dicentrarchus_labrax_standardise.RData")

reproduction_months <- c(1,2,3,12) # months of reproduction

center = "relative_to_full_ts"
## Two options to center:
# 1/ relatively to the temporal average over the full time series (i.e. the mean maps of all other maps of the time series)
# center = "relative_to_full_ts"
# Question adressed: what are the reproduction grounds of sea bass ?
#
# 2/ relatively to the temporal average over the reproduction period (i.e. the mean maps of the months belonging to reproduction)
# center = "relative_to_repro_period"
# Question adressed: How reproduction grounds vary ?

S_x_df <- S_x_df %>%
  dplyr::rename(Month = month,
                Year = year) %>%
  mutate(Year_Month = paste0(Year,"_",ifelse(Month<10,paste0("0",Month),Month))) %>%
  dplyr::select(-t)

## Make time step dataframe
year_start <- 2008
year_end <- 2018
year_vec <- year_start:year_end # time period
month_vec <- 1:12
quarter_month_df <- data.frame(Month=1:12,Quarter=rep(1:4,each = 3))
time.step_df <- expand.grid(1:12,year_start:year_end)
colnames(time.step_df) <- c("Month","Year")
time.step_df <- time.step_df %>%
  inner_join(quarter_month_df) %>%
  mutate(Year_Month = paste0(Year,"_",ifelse(Month<10,paste0("0",Month),Month)),
         t = 1:nrow(time.step_df))

S_x_df <- S_x_df %>%
  inner_join(time.step_df)

## Location dataframe
loc_x_pred <- S_x_df %>%
  dplyr::select(cell,x,y) %>%
  group_by(cell,x,y) %>%
  slice(1) %>%
  arrange(cell) %>%
  as.data.frame

## Make the T x N matrix for conducting EOF
if(center == "relative_to_repro_period"){
  S_x_df <- S_x_df %>% 
    filter(Month %in% c(1,2,3,12))
}

EOF_mat <- S_x_df %>%
  dplyr::select(cell,Year_Month,S_x) %>%
  pivot_wider(names_from = Year_Month,values_from = S_x) %>%
  as.matrix() %>% t()
EOF_mat <- EOF_mat[-1,]

## Decomposing EOF
##----------------
## Center EOF_mat
spat_mean <- apply(EOF_mat, 2, mean)
nT <- nrow(EOF_mat)
Zspat_detrend <- EOF_mat - outer(rep(1, nT), spat_mean)
Zt <- Zspat_detrend

xlims <- range(loc_x_pred$x)
ylims <- range(loc_x_pred$y)

mean_pat_df <- cbind(loc_x_pred,spat_mean)
mean_pat_plot <- ggplot(mean_pat_df)+
  geom_point(aes(x=x,y=y,col=spat_mean))+
  scale_color_distiller(palette = "Spectral")+
  geom_sf(data = mapBase) +
  theme_bw()+
  coord_sf(xlim = xlims, ylim = ylims, expand = FALSE)+
  theme(legend.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90))
  

plot(mean_pat_plot)

## Filter the months of reproduction
if(center == "relative_to_full_ts"){
  Zt_2 <- Zt[which(time.step_df$Month %in% c(1,2,3,12)),]
  time.step_df_2 <- time.step_df[which(time.step_df$Month %in% c(1,2,3,12)),]
}

if(center == "relative_to_repro_period"){
  Zt_2 <- Zt
}

## Conduct svd (singular vector decomposition)
Zt_2 <- Zt_2 * 1/sqrt(diag(var(t(Zt))))

E <- svd(Zt_2)

# Eigen values
L=E$d*E$d
pct=L/sum(L) # explained variance

plot_perc_var <- data.frame(dim = 1:length(pct),perc_var=pct) %>% 
  filter(dim < 16) %>% 
  ggplot()+
  geom_bar(aes(x=dim,y=perc_var),
           stat = "identity",fill="steelblue")+
  theme_classic()+
  theme(aspect.ratio = 1)

plot(plot_perc_var)


nT <- nrow(Zt_2)

# Spatial patterns
V <- E$v
colnames(E$v) <- paste0("EOF", 1:nT)
colnames(V) <- paste0("EOF", 1:nT) # label columns

EOFs <- cbind(loc_x_pred[,c("x","y","cell")], V)

n_dim <- 6

EOFs_2 <- EOFs %>%
  select_at(c("x","y","cell",paste0("EOF",1:nT))) %>%
  pivot_longer(cols = starts_with("EOF")) %>%
  dplyr::rename(EOF = name) %>%
  filter(EOF %in% c(paste0("EOF",1:n_dim))) # filter on dimension

EOF_map_plot <- ggplot()+
  geom_point(data=EOFs_2, # Only plot the points that most contribute to the dimension
             aes(x = x, y = y, col = -value),size = 0.75,shape=16)+
  scale_color_continuous_divergingx(palette = "Spectral", mid = 0,rev = T)+ 
  theme_bw()+
  xlab("") + ylab("")+
  geom_sf(data = mapBase) +
  coord_sf(xlim = xlims, ylim = ylims, expand = FALSE)+
  theme(legend.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90))+
  facet_wrap(.~EOF)

plot(EOF_map_plot)

# Temporal indices
EOFset1 <- E$u[1:(nT),]
EOFset1 <- EOFset1 %>% data.frame
colnames(EOFset1) <- paste0("PC",1:nT)
EOFset1 <- EOFset1[,1:nT] %>% data.frame
EOFset1_2 <- cbind(EOFset1,time.step_df_2) %>%
  pivot_longer(cols = starts_with("PC")) %>%
  dplyr::rename(PC = name) %>%
  filter(PC %in% c(paste0("PC",1:n_dim)))


EOF_time_series_plot <- ggplot(EOFset1_2,
                               aes(x=Year_Month,y=-value,group=PC,col=PC))+
  geom_vline(xintercept=EOFset1_2$Year_Month[which(str_detect(EOFset1_2$Year_Month,"_01"))],
             linetype="dashed", color = "skyblue", linewidth = 1)+
  geom_hline(yintercept=0,linetype="dashed", color = "grey", linewidth = 1)+
  geom_line(linewidth=1)+
  theme_bw()+
  xlab("")+ylab("")+
  theme()+
  facet_wrap(.~PC,nrow = n_dim/2)+
  theme(axis.text.x = element_text(angle=90,size=8),
        legend.position = "none",
        aspect.ratio = c(1/4),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"),
        plot.title = element_text(hjust=0.5,face = "bold"))+
  coord_cartesian(clip = "off")

plot(EOF_time_series_plot)

plot1 <- cowplot::plot_grid(mean_pat_plot,plot_perc_var,rel_widths = c(0.6,0.4))
plot2 <- cowplot::plot_grid(plot1,EOF_map_plot,EOF_time_series_plot,ncol = 1,rel_heights = c(0.25,0.4,0.35))

ggsave(filename = paste0("images/seabass_eofs_",center,".png"),width = 15,height = 15)
