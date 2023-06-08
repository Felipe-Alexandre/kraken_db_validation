top_panel <- plot_grid(plot3, plot4, plot2, ncol = 1)
top_panel
eco_legend <- cowplot::get_legend(plot2)

#Saave plot
ggsave("outputs/top_panel.png", width = 19, height = 18, units = "cm")

#Removing legend
plot2 <- plot2 + theme(legend.position = "none")

#Plotting panel without legend
top_panel_no_leg <- plot_grid(plot3, plot4, plot2, ncol = 1)

#Save plots
ggsave("outputs/valid_panel.png", width = 19, height = 18, units = "cm")
