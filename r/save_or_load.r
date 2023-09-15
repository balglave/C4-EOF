####################
## Save or load data
####################

file_name = c("res/S_x_list.RData",
              "res/EOF_maps.RData",
              "res/EOF_PC.RData",
              "res/eigen_values.RData",
              "res/Zt_list.RData",
              "res/V_list.RData",
              "res/E_list.RData",
              "res/loc_x_list.RData",
              "res/cov_list.RData",
              "res/E_list.RData")

for(f in 1:length(file_name)){
  
  if(file.exists(file_name[j])){
    
    load(file_name[j])
    
  }else{
    
    save(data = S_x_list,file = file_name[j])
    
  }

}
