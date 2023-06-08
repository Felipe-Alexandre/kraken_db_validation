#Plot richness
plot2 <- ggplot(richness_summary, aes(x = ecosystem, y = mean_richness, fill = ecosystem)) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_errorbar(aes(ymin = mean_richness - se_richness, ymax = mean_richness + se_richness), width = 0.2) +
    theme_pubr() +
    labs_pubr() +
    theme(axis.text.x = element_blank(),
    legend.position = "bottom")+
    labs(x = "Ecosystem", y = "CPR and DPANN \nrichness gain (%)", 
    fill = "Ecosystem")+
    scale_fill_manual(values = ecoColors)
plot2
