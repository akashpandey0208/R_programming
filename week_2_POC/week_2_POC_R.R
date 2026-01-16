library(dplyr)
library(readr)

#1. Create data frame "dfCountries", load countries population file in it.
dfCountries <- read_csv("Countries Population.csv")

#Displaying first few rows
head(dfCountries)
colnames(dfCountries)

print(colSums(is.na(dfCountries)))

#2. Sort countries according to their population in ascending and descending order 

countries_ascending <- dfCountries %>% arrange(`Total Population 2017`)
print(countries_ascending)

countries_descending <- dfCountries %>% arrange(desc(`Total Population 2017`))
print(countries_descending)

# 3. A vector of countries with population more than 10000000

population_10M <- dfCountries %>% filter(`Total Population 2017` > 10000000) %>% pull(`Country Name`)
print(population_10M)

#4. Create a dataframe "dfBigAndSmall" that has countries with population greater than 10M and less than 2M.
dfBigAndSmall <- dfCountries %>% filter(`Total Population 2017` > 10000000 | `Total Population 2017` < 2000000)
print(dfBigAndSmall)