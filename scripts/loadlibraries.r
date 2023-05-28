# Here you will find the libraries that are used in the scripts.
#If the libraries are not installed, they will be installed automatically.

#Check if libraries are already installed
if (!requireNamespace("tidyverse", quietly = TRUE))
    install.packages("tidyverse")
if (!requireNamespace("reshape2", quietly = TRUE))
    install.packages("reshape2")
if (!requireNamespace("httpgd", quietly = TRUE))
    install.packages("httpgd")
if (!requireNamespace("ggpubr", quietly = TRUE))
    install.packages("ggpubr")

#Load libraries
library(tidyverse)
library(reshape2)
library(httpgd)
library(ggpubr)