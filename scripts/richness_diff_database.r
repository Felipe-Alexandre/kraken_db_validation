#Comparing microbial richness through databases

#Read annotated samples from custom db
phyla_custom <- read.csv("inputs/biome_phyla.general.relative.matrix.csv", header = T, sep = ",")

#Read standard db
phyla_std <- read.csv("inputs/stddb_phyla.general.relative.matrix.csv", header = T, sep = ",")

#Calculate richness for custom

phyla_custom$custom_richness <- rowSums(phyla_custom[,-1] > 0)

phyla_std$std_richness <- rowSums(phyla_std[,-1] > 0)

#Combine these two information
phyla_richness <- inner_join(phyla_std, phyla_custom, by = "samples") %>% select(samples, custom_richness, std_richness)

#Read curated_metadata with information about ecosystem
raw_metadata <- read.csv("inputs/curated_metadata_2023_03_04.csv") %>% 
    select(samples, ecosystem)

#Select unique samples
raw_metadata <- unique(raw_metadata)

#Combine richness with metadata
phyla_richness_eco <- inner_join(phyla_richness, raw_metadata, by = "samples")

#Melt for plotting boxplots with samples
rich_melted <- melt(phyla_richness_eco, id.vars = c("samples", "ecosystem")) %>% 
    mutate(ecosystem = str_replace_all(ecosystem, "animal_host-associated", "Animal host-associated") %>%
    str_replace_all("freshwater", "Freshwater") %>%
    str_replace_all("groundwater", "Groundwater") %>%
    str_replace_all("saline_water", "Saline water") %>%
    str_replace_all("human_host", "Human host") %>%
    str_replace_all("sediment", "Sediment") %>%
    str_replace_all("soil", "Soil") %>%
    str_replace_all("wastewater", "Wastewater") %>%
    str_replace_all("plant-associated", "Plant Associated"))


ggplot(rich_melted, aes(x = ecosystem, y = value, fill = variable)) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    labs(x = "Ecosystem", y = "Phyla richness", 
    title = "Difference in richness between database",
    fill = "Database")+
    scale_fill_manual(values = c("blue", "red"), labels = c("Custom", "Standard"))

#Save plots
ggsave("outputs/richness_diff_per_sample.png", width = 15, height = 10, units = "cm")


#Calculate delta for richness
phyla_richness_eco$richness_diff <- phyla_richness_eco$custom_richness - phyla_richness_eco$std_richness

#Summarize richness by ecosystem
phyla_richness_eco_summ <- phyla_richness_eco %>% 
    group_by(ecosystem) %>%
    summarise(richness_diff = mean(richness_diff),
    richness_sd = sd(phyla_richness_eco$richness_diff),
    richness_se = richness_sd/sqrt(n())
    ) %>% mutate(ecosystem = str_replace_all(ecosystem, "animal_host-associated", "Animal host-associated") %>%
    str_replace_all("freshwater", "Freshwater") %>%
    str_replace_all("groundwater", "Groundwater") %>%
    str_replace_all("saline_water", "Saline water") %>%
    str_replace_all("human_host", "Human host") %>%
    str_replace_all("sediment", "Sediment") %>%
    str_replace_all("soil", "Soil") %>%
    str_replace_all("wastewater", "Wastewater") %>%
    str_replace_all("plant-associated", "Plant Associated"))
    
#Now, plot the difference with standard error
ggplot(phyla_richness_eco_summ, aes(x = ecosystem, y = richness_diff)) +
    geom_errorbar(aes(ymin = richness_diff - richness_se, ymax = richness_diff + richness_se), width = 0.3, colour = "black", alpha = 0.9, size = 0.5) +
    geom_bar(aes(x = ecosystem, y = richness_diff), stat = "identity", width = .5) +
    labs(x = "", y = "Richness difference (custom - standard)") +
    theme_pubr() +
    theme(
        text = element_text(
            size = 10
        ),
        axis.text.x = element_text(
            angle = 45,
            hjust = 1
        )
    )

#Save plots
ggsave("outputs/richness_diff_barplot.png", width = 18.5, height = 10, units = "cm")
