#Comparing microbial community structure through databases

#First, read the data
phyla_custom <- read.csv("inputs/biome_phyla.general.relative.matrix.csv", header = T, sep = ",")

phyla_std <- read.csv("inputs/stddb_phyla.general.relative.matrix.csv", header = T, sep = ",")

#Filter tables to samples present in both tables
phyla_custom <- phyla_custom %>% filter(samples %in% phyla_std$samples)

phyla_std <- phyla_std %>% filter(samples %in% phyla_custom$samples)

#Read curated_metadata
raw_metadata <- read.csv("inputs/curated_metadata_2023_03_04.csv") %>% select(samples, ecosystem)

raw_metadata <- unique(raw_metadata) %>% select(samples, ecosystem)

#Add ecosystem information to each table
phyla_custom <- inner_join(phyla_custom, raw_metadata, by = "samples")
phyla_std <- inner_join(phyla_std, raw_metadata, by = "samples")

#Add sufix to each sample name
phyla_custom$samples <- paste0(phyla_custom$samples, "_custom")

phyla_std$samples <- paste0(phyla_std$samples, "_std")

#Create a new column in each table, with the database information
phyla_custom$database <- "custom"

phyla_std$database <- "standard"

#Check if the tables have columns in common
intersect(colnames(phyla_custom), colnames(phyla_std))

#Now, join both tables based on these columns
phyla_merged <- merge(phyla_custom, phyla_std, by = intersect(colnames(phyla_custom), colnames(phyla_std)), all = TRUE)

#Check
colnames(phyla_merged)

#Replace NA with 0
phyla_merged[is.na(phyla_merged)] <- 0

#Now, calculate the mds
phyla_merged_num <- phyla_merged %>% select(-samples, -database, -ecosystem)
print("Calculating MDS")
phyla_merged_mds <- metaMDS(phyla_merged_num, distance = "bray", trymax = 1000)
print("MDS calculated. Saving...")
#Save mds object in .Rdata format
save(phyla_merged_mds, file = "phyla_merged_mds.Rdata")
print("MDS saved")
#Extract the coordinates
phyla_merged_mds_df <- cbind(phyla_merged$samples, phyla_merged$database, phyla_merged$ecosystem, phyla_merged_mds$points)
colnames(phyla_merged_mds_df) <- c("samples", "database", "ecosystem", "MDS1", "MDS2")

attach(phyla_merged_mds_df)

#Running ANOSIM
print("Running ANOSIM for database")
database_anosim <- anosim(phyla_merged_num, phyla_merged$database, permutations = 999)

#Save ANOSIM results
save(database_anosim, eco_anosim, file = "ANOSIM_results_database.Rdata")

#Running ANOSIM for ecosystem
print("Running ANOSIM for ecosystem")
eco_anosim <- anosim(phyla_merged_num, phyla_merged$ecosystem, permutations = 999)

#Save ANOSIM results
save(eco_anosim, file = "ANOSIM_results_eco.Rdata")

print("Plotting results")
#Now, plot the MDS
plot_mds <- ggplot(phyla_merged_mds_df, aes(x = MDS1, y = MDS2, color = database, shape = ecosystem)) +
    geom_point(size = 4) +
    theme_bw() +
    theme(legend.position = "right") +
    labs(x = "MDS1", y = "MDS2", color = "Database", shape = "Ecosystem")+
    theme(panel.border = element_rect(colour = "black", size=1))+
    theme(legend.key=element_rect(fill='white'))+
    theme(legend.key = element_rect(colour = "white"))+
    scale_color_manual(values = c("blue", "red"), labels = c("Custom", "Standard"))

#Save the plot
print("Saving plot")
ggsave("MDS_database.png", width = 30, height = 24, units = "cm")