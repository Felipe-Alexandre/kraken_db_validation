#Load and configure colors in plots

ecoColors <- c("Animal host-associated" = "#FFD700",
    "Freshwater" = "#87CEFA",
    "Groundwater" = "#3A5FCD",
    "Human host-associated" = "#DA70D6",
    "Plant host-associated" = "#228B22",
    "Saline water" = "#20B2AA",
    "Sediment" = "#F4A460",
    "Soil" = "#8B4513",
    "Wastewater" = "#000000"
)

#Read lists with cpr and dpann tax names
cpr_groups <- read_csv("inputs/cprPhyla.csv")

dpann_groups <- read_csv("inputs/dpannPhyla.csv")

#Read curated_metadata with information about ecosystem
raw_metadata <- read.csv("inputs/curated_metadata_2023_03_04.csv") %>% 
    select(samples, ecosystem)


#Data wrangling for richness difference
#Read annotated samples from custom db
phyla_custom <- read.csv("inputs/biome_phyla.general.relative.matrix.csv", header = T, sep = ",")

#Read standard db
phyla_std <- read.csv("inputs/stddb_phyla.general.relative.matrix.csv", header = T, sep = ",")

#Filter tables to samples present in both tables
phyla_custom <- phyla_custom %>% filter(samples %in% phyla_std$samples)

phyla_std <- phyla_std %>% filter(samples %in% phyla_custom$samples)

#Select unique samples
raw_metadata <- unique(raw_metadata)

#Filter metadata to samples present in both tables
raw_metadata <- raw_metadata %>% filter(samples %in% phyla_custom$samples)

#Calculate richness for each database

phyla_custom$custom_richness <- rowSums(phyla_custom[,-1] > 0)

phyla_std$std_richness <- rowSums(phyla_std[,-1] > 0)

#Combine these two information
phyla_richness <- inner_join(phyla_std, phyla_custom, by = "samples") %>% select(samples, custom_richness, std_richness)


#Combine richness with metadata
phyla_richness_eco <- inner_join(phyla_richness, raw_metadata, by = "samples")

#Calculate delta for richness
phyla_richness_eco$richness_diff <- phyla_richness_eco$custom_richness - phyla_richness_eco$std_richness

#Calculate the percentage of the difference
phyla_richness_eco$richness_diff_percentage <- phyla_richness_eco$richness_diff/phyla_richness_eco$std_richness*100

#Summarize richness by ecosystem
phyla_richness_eco_summ <- phyla_richness_eco %>% 
    group_by(ecosystem) %>%
    summarise(richness_diff_percentage = mean(richness_diff_percentage),
    richness_sd = sd(phyla_richness_eco$richness_diff_percentage),
    richness_se = richness_sd/sqrt(n())
    ) %>% mutate(ecosystem = str_replace_all(ecosystem, "animal_host-associated", "Animal host-associated") %>%
    str_replace_all("freshwater", "Freshwater") %>%
    str_replace_all("groundwater", "Groundwater") %>%
    str_replace_all("saline_water", "Saline water") %>%
    str_replace_all("human_host-associated", "Human host-associated") %>%
    str_replace_all("sediment", "Sediment") %>%
    str_replace_all("soil", "Soil") %>%
    str_replace_all("wastewater", "Wastewater") %>%
    str_replace_all("plant-associated", "Plant host-associated"))

#Reorder ecosystem factors
phyla_richness_eco_summ <- phyla_richness_eco_summ %>% 
    mutate(ecosystem = factor(ecosystem, levels = c("Human host-associated", "Animal host-associated", "Plant host-associated", "Groundwater", "Freshwater", "Saline water", "Wastewater", "Sediment", "Soil")))


#Data wrangling for unclassified sequences
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

annotated_samples_comparison <- annotated_samples_comparison %>% mutate(ecosystem = str_replace_all(ecosystem_stddb, "animal_host-associated", "Animal host-associated") %>%
    str_replace_all("freshwater", "Freshwater") %>%
    str_replace_all("groundwater", "Groundwater") %>%
    str_replace_all("saline_water", "Saline water") %>%
    str_replace_all("human_host-associated", "Human host-associated") %>%
    str_replace_all("sediment", "Sediment") %>%
    str_replace_all("soil", "Soil") %>%
    str_replace_all("wastewater", "Wastewater") %>%
    str_replace_all("plant-associated", "Plant host-associated"))

#Reorder ecosystem factors
annotated_samples_comparison <- annotated_samples_comparison %>% 
    mutate(ecosystem = factor(ecosystem, levels = c("Human host-associated", "Animal host-associated", "Plant host-associated", "Groundwater", "Freshwater", "Saline water", "Wastewater", "Sediment", "Soil")))


#Combine richness with metadata
annotated_samples_comparison <- inner_join(annotated_samples_comparison, raw_metadata, by = "samples")

#Calculate average value, with standard deviation and standard error
annotated_samples_comparison_summ <- annotated_samples_comparison %>% 
    group_by(ecosystem_stddb) %>%
    summarise(unclassified_diff = mean(unclassified_diff),
    unclassified_diff_sd = sd(annotated_samples_comparison$unclassified_diff),
    unclassified_diff_se = unclassified_diff_sd/sqrt(n()))

annotated_samples_comparison_summ <- annotated_samples_comparison_summ %>%
    mutate(ecosystem = str_replace_all(ecosystem_stddb, "animal_host-associated", "Animal host-associated") %>%
    str_replace_all("freshwater", "Freshwater") %>%
    str_replace_all("groundwater", "Groundwater") %>%
    str_replace_all("saline_water", "Saline water") %>%
    str_replace_all("human_host-associated", "Human host-associated") %>%
    str_replace_all("sediment", "Sediment") %>%
    str_replace_all("soil", "Soil") %>%
    str_replace_all("wastewater", "Wastewater") %>%
    str_replace_all("plant-associated", "Plant host-associated"))


#Reorder ecosystem factors
annotated_samples_comparison_summ <- annotated_samples_comparison_summ %>% 
    mutate(ecosystem = factor(ecosystem, levels = c("Human host-associated", "Animal host-associated", "Plant host-associated", "Groundwater", "Freshwater", "Saline water", "Wastewater", "Sediment", "Soil")))




###### Data Wrangling for cpr and dpann richness #######
#Now, select only the columns that have the names in cpr_groups
phyla_custom_cpr <- phyla_custom %>% select(contains(cpr_groups$Family))

phyla_std_cpr <- phyla_std %>% select(contains(cpr_groups$Family))

#Now, with dpann
phyla_custom_dpann <- phyla_custom %>% select(contains(dpann_groups$Family))

phyla_std_dpann <- phyla_std %>% select(contains(dpann_groups$Family))

#Join the tables with cpr and dpann

phyla_custom_candidate <- cbind("samples" = phyla_custom$samples, phyla_custom_cpr, phyla_custom_dpann)

phyla_std_candidate <- cbind("samples" = phyla_std$samples, phyla_std_cpr, phyla_std_dpann)

#Calculate richness for each sample
phyla_custom_candidate$custom_richness <- rowSums(phyla_custom_candidate[,-1] > 0)

phyla_std_candidate$std_richness <- rowSums(phyla_std_candidate[,-1] > 0)

#Create a new df with samples, richness, ecosystem and database
candidate_richness <- as.data.frame(cbind("samples" = phyla_custom_candidate$samples, "custom_richness" = phyla_custom_candidate$custom_richness,
    "std_richness" = phyla_std_candidate$std_richness, "ecosystem" = raw_metadata$ecosystem))

#Converting to numeric
candidate_richness$custom_richness <- as.numeric(candidate_richness$custom_richness)
candidate_richness$std_richness <- as.numeric(candidate_richness$std_richness)

#Calculate delta for richness
candidate_richness$richness_diff <- candidate_richness$custom_richness - candidate_richness$std_richness

#Calculate percentage of the difference
candidate_richness$richness_diff_percentage <- candidate_richness$richness_diff/candidate_richness$std_richness*100

#Summarize richness by ecosystem
richness_summary <- candidate_richness %>% group_by(ecosystem) %>% 
    summarize(mean_richness = mean(richness_diff), 
    sd_richness = sd(richness_diff),
    se_richness = sd_richness/sqrt(n())) %>% mutate(ecosystem = str_replace_all(ecosystem, "animal_host-associated", "Animal host-associated") %>%
    str_replace_all("freshwater", "Freshwater") %>%
    str_replace_all("groundwater", "Groundwater") %>%
    str_replace_all("saline_water", "Saline water") %>%
    str_replace_all("human_host-associated", "Human host-associated") %>%
    str_replace_all("sediment", "Sediment") %>%
    str_replace_all("soil", "Soil") %>%
    str_replace_all("wastewater", "Wastewater") %>%
    str_replace_all("plant-associated", "Plant host-associated"))

#Reorder ecosystem factors
richness_summary <- richness_summary %>% 
    mutate(ecosystem = factor(ecosystem, levels = c("Human host-associated", "Animal host-associated", "Plant host-associated", "Groundwater", "Freshwater", "Saline water", "Wastewater", "Sediment", "Soil")))

