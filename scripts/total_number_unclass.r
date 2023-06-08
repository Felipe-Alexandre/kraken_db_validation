#Count the total number of unclassified sequences in
#our metagenomes database using standard and custom kraken databases
#This is just a investigatory script. None of these results are used in the manuscript

#Read the unclassified information from custom db
annotated_customdb <- read.csv("inputs/biome_annotated_samples_unclassified.csv", header = TRUE, sep = ",")

#Count the classified sequences
annotated_customdb$clasified_sequences <- annotated_customdb$sequence_count_raw - annotated_customdb$unclassified_sequences

#Load data with standard db
annotated_stddb <- read.csv("inputs/stddb_annotated_samples_unclassified.csv", header = TRUE, sep = ",")

#Count the classified sequences
annotated_stddb$clasified_sequences <- annotated_stddb$sequence_count_raw - annotated_stddb$unclassified_sequences

#Check if columns in both tables are the same
colnames(annotated_customdb) == intersect(colnames(annotated_customdb), colnames(annotated_stddb))

#Select samples that are present in both tables
annotated_customdb <- annotated_customdb %>% filter(samples %in% annotated_stddb$samples)

annotated_stddb <- annotated_stddb %>% filter(samples %in% annotated_customdb$samples)

#Remove duplicated samples
annotated_stddb <- annotated_stddb[!duplicated(annotated_stddb$samples),]

#Add sufix to each sample name
annotated_customdb$samples <- paste0(annotated_customdb$samples, "_custom")

annotated_stddb$samples <- paste0(annotated_stddb$samples, "_std")

#Create a new column in each table, with the database information
annotated_customdb$database <- "custom"

annotated_stddb$database <- "standard"

#Now, join both tables based on these columns
annotated_merged <- merge(annotated_customdb, annotated_stddb, by = intersect(colnames(annotated_customdb), colnames(annotated_stddb)), all = TRUE)

#Check
colnames(annotated_merged)

#Now, summarize data, sum all samples with the same database
annotated_merged_sum <- annotated_merged %>% 
    group_by(database) %>%
    summarise(unclassified_sequences = sum(unclassified_sequences),
    clasified_sequences = sum(clasified_sequences),
    sequence_count_raw = sum(sequence_count_raw))

annotated_merged_sum$percentage_unclassified <- annotated_merged_sum$unclassified_sequences/annotated_merged_sum$sequence_count_raw*100
annotated_merged_sum$percentage_classified <- annotated_merged_sum$clasified_sequences/annotated_merged_sum$sequence_count_raw*100

#Now, make a barplot with percentage of unclassified in each database
ggplot(annotated_merged_sum, aes(x = database, y = percentage_unclassified)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    labs(x = "Database", y = "Unclassified sequences (%)", 
    title = "Unclassified sequences in each database") +
    scale_y_continuous(breaks = seq(0, 100, by = 5))

#Save the plot
ggsave("outputs/total_unclassified.png", width = 10, height = 10, units = "cm")

#Now, make a barplot with the absolute number of unclassified in each database
ggplot(annotated_merged_sum, aes(x = database, y = unclassified_sequences)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    labs(x = "Database", y = "Number of unclassified sequences", 
    title = "Number of unclassified sequences in each database")

#Save the plot
ggsave("outputs/total_unclassified_absolute.png", width = 10, height = 10, units = "cm")