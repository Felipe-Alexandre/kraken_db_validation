#Create figures to find differences between annotation with different databases

#Load data with custom db
annotated_customdb <- read.csv("inputs/biome_annotated_samples_unclassified.csv", header = TRUE, sep = ",")

#Count the classified sequences
annotated_customdb$clasified_sequences <- annotated_customdb$sequence_count_raw - annotated_customdb$unclassified_sequences
#Add sufix in colnames
colnames(annotated_customdb)[2:ncol(annotated_customdb)] <- paste0(colnames(annotated_customdb)[2:ncol(annotated_customdb)], "_customdb")

#Load data with standard db
annotated_stddb <- read.csv("inputs/stddb_annotated_samples_unclassified.csv", header = TRUE, sep = ",")

#Count the classified sequences
annotated_stddb$clasified_sequences <- annotated_stddb$sequence_count_raw - annotated_stddb$unclassified_sequences

#Add sufix in colnames
colnames(annotated_stddb)[2:ncol(annotated_stddb)] <- paste0(colnames(annotated_stddb)[2:ncol(annotated_stddb)], "_stddb")



#Now, join both dataframes based on samples
annotated_samples_comparison <- annotated_stddb %>% 
    inner_join(annotated_customdb, by = "samples")

annotated_samples_comparison <- annotated_samples_comparison %>% select(-ecosystem_customdb)

#Now, calculate the difference of anotated sequences between databases
annotated_samples_comparison$unclassified_diff <- annotated_samples_comparison$percentage_unclassified_stddb - annotated_samples_comparison$percentage_unclassified_customdb

#Now, plot the difference
hgd()
ggplot(annotated_samples_comparison, aes(x = ecosystem_stddb, y = unclassified_diff)) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    labs(x = "Ecosystem", y = "Classified sequences difference (%)", 
    title = "Unclassified sequences (standard database - custom database)")

#Save the plot
ggsave("outputs/classified_seq_delta.png", width = 10, height = 10, units = "cm")

#Calculate average value, with standard deviation and standard error
annotated_samples_comparison <- annotated_samples_comparison %>% 
    group_by(ecosystem_stddb) %>%
    summarise(unclassified_diff = mean(unclassified_diff),
    unclassified_diff_sd = sd(unclassified_diff),
    unclassified_diff_se = unclassified_diff_sd/sqrt(n()))

annotated_samples_comparison <- annotated_samples_comparison %>%
    mutate(ecosystem = str_replace_all(ecosystem_stddb, "animal_host-associated", "Animal host-associated") %>%
    str_replace_all("freshwater", "Freshwater") %>%
    str_replace_all("groundwater", "Groundwater") %>%
    str_replace_all("saline_water", "Saline water") %>%
    str_replace_all("human_host", "Human host") %>%
    str_replace_all("sediment", "Sediment") %>%
    str_replace_all("soil", "Soil") %>%
    str_replace_all("wastewater", "Wastewater") %>%
    str_replace_all("plant-associated", "Plant Associated"))

#Make a barplot with percentage
ggplot(annotated_samples_comparison, 
    aes(x = reorder(ecosystem, -unclassified_diff), y = unclassified_diff)) +
    geom_bar(stat = "identity", fill = "gray") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Ecosystem", y = "Average unclassified sequences difference (%)",
        title = "Unclassified sequences (standard database - custom database)")

#Save the plot
ggsave("outputs/unclassified_seq_delta.png", width = 15, height = 12, units = "cm")

#Select values that are less than 0
#To check samples
class_dif_less_zero <- annotated_samples_comparison %>% filter(unclassified_diff < 0)