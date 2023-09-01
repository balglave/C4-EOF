#############################
## Abundance weighted average
#############################
x11()
for(i in 1:4){
  
  S_x_df = S_x_list[[i]]
  u_bar = c()
  for(year in 2008:2018){
    
    S_x_df_2 = S_x_df %>%
      filter(Year == year)
    D_y = sum(S_x_df_2$S_x)
    D_t = c()
    for(month in 1:12){
      S_x_df_3 = S_x_df_2 %>% 
        filter(Month == month)
      D_t = c(D_t,sum(S_x_df_3$S_x))
    }
    u_bar = c(u_bar,sum(D_t/D_y * c(1:12)))
  }
  
  if(i==1){
    plot(u_bar,main=species_name[i],ylim = c(0,12),col=i)
    lines(u_bar)
  }
  
  if(i>1) lines(u_bar,col=i)
  
}
