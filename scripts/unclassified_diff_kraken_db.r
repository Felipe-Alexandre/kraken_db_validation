#Make a barplot with percentage
plot3 <- ggplot(annotated_samples_comparison_summ,
    aes(x = ecosystem, y = unclassified_diff, fill = ecosystem)) +
    geom_errorbar(aes(ymin = unclassified_diff - unclassified_diff_se, ymax = unclassified_diff + unclassified_diff_sd), width = 0.3, colour = "black", alpha = 0.9, linewidth = 0.5) +
    geom_bar(stat = "identity", width = 0.5) +
    theme_pubr(base_size = 12) +
    labs_pubr() +
    theme(axis.text.x = element_blank(), legend.position = "none") +
    labs(x = "", y = "Gain in \n classification (%)") +
    scale_fill_manual(values = ecoColors)

plot3
