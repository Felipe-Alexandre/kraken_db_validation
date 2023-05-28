#This is the main script to perform all analysis
#related with kraken database validation

#Load libraries
source("scripts/loadlibraries.R")

#Create figures for richness
source("scripts/richness_diff_database.r")
#Create fifures for unclassified sequences
source("scripts/unclassified_diff_kraken_db.r")
#Create figures for community structure
source("scripts/community_structure_database_diff.R")