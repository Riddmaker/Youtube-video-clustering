## Small youtube video clustering algorithm ##
#Load necessary libraries
library(tidyverse)
library(ggplot2)
library(readxl)
library(openxlsx)
library(cluster)
library(dplyr)
library(hms)
library(factoextra)

# Clean the workspace
rm(list = ls())

# Set the working directory to where your file is located
setwd("C:/Git/Youtube video clustering")

# Read the data
read.data <- read_excel("Fantasy_ASMR_last_90_days_videos.xlsx")
#str(read.data)

# Test the data for Na's and remove them
nrow(read.data)
read.data <- na.omit(read.data)
nrow(read.data)
# Removed rows are live-streams, that's ok

# Delete spaces in column-names
colnames(read.data) <- gsub("\\s", "_", colnames(read.data))

# delete the first row
read.data <- slice(read.data, -1)

# Create data-wrangling data-set
raw.data <- read.data

# delete unnecessary attributes for clustering
raw.data <- raw.data[, -c(1, 2, 3)]

# Convert chr attribute holding average play duration
raw.data$`Durch­schnitt­li­che_Wie­der­ga­be­dau­er` <- as.numeric(as_hms(raw.data$`Durch­schnitt­li­che_Wie­der­ga­be­dau­er`))

# Create final data-set
data <- raw.data

# Function for wss plot
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}

# Call function
wssplot(data)

# Run again with optimal k-value
opt_k <- 4
set.seed(123)
clusters <- kmeans(data, centers = opt_k)

# Create new attribute with cluster numbers
data$cluster <- clusters$cluster

# Add the three attributes again to the clustered data-set
data$`Vi­de­os` <- read.data$`Vi­de­os`
data$`Vi­deo­ti­tel` <- read.data$`Vi­deo­ti­tel`
data$`Ver­öf­fent­li­chungs­zeit­punkt_des_Vi­de­os` <- read.data$`Ver­öf­fent­li­chungs­zeit­punkt_des_Vi­de­os`

# Export the data frame
write.xlsx(data, file = "90_day_labeled.xlsx", sheetName = "Sheet1", rowNames = FALSE)
