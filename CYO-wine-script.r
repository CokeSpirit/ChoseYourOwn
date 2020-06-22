# Check that packagesare checked for with require
# What libraries do we need?
library(tidyverse)
#library(stringr)
library(corrplot)
library(caret)

#We are using the Wine Data set
# Why? I like wine, I live close to a wine region, and I am a chemist, so at least some of the molecules/names ring a bell

#what can we do with it? Examine
# Completeness
# tidy? no N/A, 
# Min-Max
# Histograms of the data
#correlation matrix
#dist matrix?
#knn

#Webpage
#https://archive.ics.uci.edu/ml/index.php


# Weblink
#https://archive.ics.uci.edu/ml/datasets/wine



# Source:
#   
#   Original Owners:
#   
#   Forina, M. et al, PARVUS -
#   An Extendible Package for Data Exploration, Classification and Correlation.
# Institute of Pharmaceutical and Food Analysis and Technologies, Via Brigata Salerno,
# 16147 Genoa, Italy.
# 
# Donor:
#   
#   Stefan Aeberhard, email: stefan '@' coral.cs.jcu.edu.au




# get and load the data from the web...
# skip if already existing?

wine_data_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
wine_names_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.names"
download.file(wine_data_url, ".\\wine-data.csv")
download.file(wine_names_url, ".\\wine-names.txt")

#Creating a data frame from wine.data
wines <- read.csv(".\\wine-data.csv", header=FALSE, sep=",")

#Adding column names - for the ease of life, this is done manually
colnames(wines) <- c("Winery", "Alcohol", "Malic acid",
                     "Ash", "Alcalinity of ash", "Magnesium",
                     "Total phenols", "Flavanoids",
                     "Nonflavanoid phenols", "Proanthocyanins",
                     "Color intensity", "Hue",
                     "OD280/OD315 of diluted wines", "Proline")

features <- wines %>% subset(select=-Winery)
corrplot(cor(wines %>% subset(select=-Winery)))

model_knn <-  train(Winery ~ ., method="knn", data=wines)



