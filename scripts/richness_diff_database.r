#Comparing microbial richness through databases

#Plot the difference with standard error
plot4 <- ggplot(phyla_richness_eco_summ, aes(x = ecosystem, y = richness_diff_percentage, fill = ecosystem)) +
    geom_errorbar(aes(ymin = richness_diff_percentage - richness_se, ymax = richness_diff_percentage + richness_se), width = 0.3, colour = "black", alpha = 0.9, size = 0.5) +
    geom_bar(aes(x = ecosystem, y = richness_diff_percentage), stat = "identity", , width = 0.5) +
    labs(x = "", y = "Gain in richness (%)", fill = "Ecosystem") +
    theme_pubr() +
    labs_pubr() +
    theme(
        axis.text.x = element_blank(),
        legend.position = "none") +
        scale_fill_manual(values = ecoColors)

plot4
#Save plots
ggsave("outputs/richness_diff_barplot.png", width = 18.5, height = 10, units = "cm")
