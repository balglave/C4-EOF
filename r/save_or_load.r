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
              "res/minmax_PC_df.RData",
              "res/PC_list.RData",
              "res/mean_patt.RData")

for(f in 1:length(file_name)){
  
  if(file.exists(file_name[f])){
    
    load(file_name[f])
    
  }else{
    
    data_name <- str_sub(file_name[f],5,-7)
    request_text <- paste0("save(data=\"",data_name,"\",file=\"",file_name[f],"\")")
    expression_text <- expression(request_text)
    eval(expr = expression_text)

  }

}
