##########################
## Picture EOF time series
##########################

## Make full picture
title_sole <- ggdraw() + draw_label("Sole",
                                    hjust = 0.5,
                                    fontface='bold.italic',
                                    color = "darkgrey",
                                    size = 18)

title_hake_bob <- ggdraw() + draw_label("Hake",
                                        hjust = 0.5,
                                        fontface='bold.italic',
                                        color = "darkgrey",
                                        size = 18)

title_seabass <- ggdraw() + draw_label("Sea bass",
                                       hjust = 0.5,
                                       fontface='bold.italic',
                                       color = "darkgrey",
                                       size = 18)

# EOF_sole <- plot_grid(list_EOF_PC_sole[[1]],list_EOF_map_sole[[1]],
#                       list_EOF_PC_sole[[2]],list_EOF_map_sole[[2]],
#                       nrow = 2,align = "v",
#                       rel_widths = c(1,0.6))
# 
EOF_sole <- plot_grid(title_sole,EOF_sole_pres,rel_heights = c(0.1,1),ncol=1)
# 
# EOF_hake_bob <- plot_grid(list_EOF_PC_hake_bob[[1]],list_EOF_map_hake_bob[[1]],
#                           list_EOF_PC_hake_bob[[2]],list_EOF_map_hake_bob[[2]],
#                           nrow = 2,align = "v",
#                           rel_widths = c(1,0.6))
# 
EOF_hake_bob <- plot_grid(title_hake_bob,EOF_hake_bob_pres,rel_heights = c(0.1,1),ncol=1)

# EOF_hake_cs <- plot_grid(list_EOF_PC_hake_cs[[1]],list_EOF_map_hake_cs[[1]],
#                          list_EOF_PC_hake_cs[[2]],list_EOF_map_hake_cs[[2]],
#                          nrow = 2,align = "v",
#                          rel_widths = c(1,0.6))
# 
# EOF_hake_cs <- plot_grid(title_hake_cs,EOF_hake_cs,rel_heights = c(0.1,1),ncol=1)

# EOF_seabass <- plot_grid(list_EOF_PC_seabass[[1]],
#                          list_EOF_map_seabass[[1]],
#                          NULL,NULL,
#                          nrow = 2,align = "v",
#                          rel_widths = c(1,0.6))

EOF_seabass <- plot_grid(title_seabass,EOF_seabass_pres,rel_heights = c(0.1,1),ncol=1)

EOF_full <- plot_grid(EOF_sole,EOF_hake_bob,EOF_seabass,ncol=1,align="v")

ggsave(filename = "images/EOF_map_plot.png",width = 10,height = 15)

