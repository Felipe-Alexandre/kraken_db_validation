#This is the main script to perform all analysis
#related with kraken database validation

#Load libraries
source("scripts/loadlibraries.R")

#Data wrangling
source("scripts/data_wrangling.r")
#Create figures for richness
source("scripts/richness_diff_database.r")
#Create fifures for unclassified sequences
source("scripts/unclassified_diff_kraken_db.r")
#Create figures for cpr and dpann difference
source("scripts/cpr_dpann_diff.r")
#Create figures for community structure
source("scripts/plot_panel.r")
