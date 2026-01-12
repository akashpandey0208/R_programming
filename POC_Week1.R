data <- read.csv("healthcare_dataset.csv")
#install.packages("readr")
#library(readr)
#data <- read_csv("healthcare_dataset.csv")
#spec(data)
# getwd()
# setwd("C:/Users/akpandey/Documents/practice_BioStatistics/R_Practice_self/healthcare_dataset.csv")
summary(data)
colnames(data)
str(data)

#Basic Data Exploration
head(data)  # first 6 rows
ncol(data) # number of columns
tail(data) # last 6 rows
dim(data)
