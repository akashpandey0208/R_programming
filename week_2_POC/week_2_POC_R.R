library(dplyr)
library(readr)

#1. Create data frame "dfCountries", load countries population file in it.
dfCountries <- read_csv("Countries Population.csv")

#Displaying first few rows
head(dfCountries)
colnames(dfCountries)

#Removing the NA columns .(Cleaning the dataset)
print(colSums(is.na(dfCountries)))
dfCountries[c("...4","...5")] <- NULL
dfCountries

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

#Reading the other two csv and excel sheet. then merging into one.

library(readxl)   # for reading Excel files
library(ggplot2)  # for plotting

# region mapping Excel file
region_data <- read_excel("Countries Region Mapping.xlsx")

# indicators CSV file
indicators_data <- read_csv("Countries Indicators.csv", na = c("NA", ""))
indicators_data[c("...4","...5")] <- NULL

#  Make sure the column names match for joining
colnames(dfCountries)[colnames(dfCountries) == "Country Code"] <- "Country.Code"
colnames(dfCountries)[colnames(dfCountries) == "Country Name"] <- "Country.Name"


#6.  Merge the 3 datasets attached into 1 dataframe : "dfCountryMaster"
dfCountryMaster <- dfCountries %>%
  left_join(region_data, by = "Country.Code") %>%
  left_join(indicators_data, by = "Country.Code")
colnames(dfCountryMaster)[colnames(dfCountryMaster) == "IncomeGroup"] <- "Income.Group"

print(colnames(dfCountryMaster))

#5. Create levels of income group from dataset "Countries region mapping" levels: Low, Lower mid, Upper mid, High

dfCountryMaster$Income.Group <- factor(
  dfCountryMaster$Income.Group,
  levels = c("Low income", "Lower middle income", "Upper middle income", "High income"),
  labels = c("Low", "Lower mid", "Upper mid", "High")
)

#  7 and 9 Summarize dfCountryMaster countries by region.
region_summary <- dfCountryMaster %>%
  group_by(Region) %>%
  summarise(
    Number_of_countries = n(),
    Total_population_millions = sum(`Total Population 2017`, na.rm = TRUE) / 1e6,
    Avg_GDP_per_capita = mean(as.numeric(gsub(",", "", GDP.per.capita.2017)), na.rm = TRUE),
    Countries_with_low_income = sum(Income.Group == "Low", na.rm = TRUE),
    Median_GDP_per_capita = median(as.numeric(gsub(",", "", GDP.per.capita.2017)), na.rm = TRUE),
    Min_U5_Mortality = min(Under.5.Mortality.Rate.2017, na.rm = TRUE),
    Max_U5_Mortality = max(Under.5.Mortality.Rate.2017, na.rm = TRUE)
  )

# 10. Write the above result in csv
write_csv(region_summary, "summary_by_region.csv")



# 11. Histogram of GDP per capita
ggplot(dfCountryMaster, aes(x = as.numeric(gsub(",", "", GDP.per.capita.2017)))) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Histogram of GDP per Capita", x = "GDP per Capita (USD)", y = "Count")

# 12. Plot of income group by region.
ggplot(dfCountryMaster, aes(x = Region, fill = Income.Group)) +
  geom_bar(position = "dodge") +
  labs(title = "Income Group by Region", x = "Region", y = "Number of Countries") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 13. Plot of mortality rate under 5 by region
ggplot(dfCountryMaster, aes(x = Region, y = Under.5.Mortality.Rate.2017)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Under 5 Mortality Rate by Region", x = "Region", y = "Under 5 Mortality Rate (per 1000)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 14.Scatter plot of mortality rate under 5 against GDP per capita.
ggplot(dfCountryMaster, aes(x = as.numeric(gsub(",", "", GDP.per.capita.2017)), y = Under.5.Mortality.Rate.2017)) +
  geom_point(aes(color = Region)) +
  labs(title = "Under 5 Mortality Rate vs GDP per Capita", x = "GDP per Capita (USD)", y = "Under 5 Mortality Rate (per 1000)")

#15. Plot of mortality rate under 5 against GDP and region.
ggplot(dfCountryMaster, aes(x = as.numeric(gsub(",", "", GDP.per.capita.2017)), y = Under.5.Mortality.Rate.2017, color = Region)) +
  geom_point() +
  labs(title = "Under 5 Mortality Rate vs GDP per Capita by Region", x = "GDP per Capita (USD)", y = "Under 5 Mortality Rate (per 1000)")
