# Wealth Unveiled: A Journey into the Lives of Billionaires Worldwide using R Visualizations
# Advance Visualization in R Final Project
# Nurdan Besli
# Maciej Lorens

#### IDEAS ####
# self-made vs. position in company vs. gender vs. age
# distribution of billionaires citizenship in the world vs gdp of the country vs tax rate


#### INITIAL PREPROCESSING ####

library(tidyverse)
library(ggplot2)

# Load the dataset
billio <- read.csv("data/Billionaires Statistics Dataset.csv")

# Get useful information about the dataset
str(billio)

# Modify gdp_country column, so that it contains numeric values
billio$gdp_country <- as.numeric(gsub("\\$", "", gsub(",", "", billio$gdp_country)))

# Check number of NAs
billio %>% summarise(across(everything(), ~sum(is.na(.))))

# Get only complete cases
billio <- billio[complete.cases(billio),]

#### MAPS ####
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")

billio_ctr_count <- billio %>%
  group_by(countryOfCitizenship, total_tax_rate_country, latitude_country, longitude_country) %>%
  count()
  
country_sf <- st_as_sf(billio_ctr_count, coords = c("longitude_country", "latitude_country"), crs = 4326)

world <- merge(world, billio_ctr_count, by.x="name", by.y="countryOfCitizenship", all.x = TRUE)

ggplot() + 
  geom_sf(data = world, aes(fill = total_tax_rate_country), color = "black") +
  geom_sf(data = country_sf, aes(size=n))