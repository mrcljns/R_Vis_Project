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
billio <- read.csv("Billionaires Statistics Dataset.csv")

# Get useful information about the dataset
str(billio)

# Data Preprocessing
billio <- billio %>% 
  rename(rank = `?..rank`)

billio <- billio %>%
  mutate(
    selfMade = case_when(
      selfMade == TRUE ~ "Founders/Entrepreneurs",
      selfMade == FALSE ~ "Inherited/Unearned",
      TRUE ~ as.character(selfMade) 
    ),
    gender = case_when(
      gender == "F" ~ "Female",
      gender == "M" ~ "Male",
      TRUE ~ gender
    )
  )

billio <- billio %>%
  mutate(rankCategory = case_when(
    rank <500 ~ "<500",
    rank <1000 ~ "<1000",
    rank <1500 ~ "<1500",
    rank <2000 ~ "<2000",
    TRUE ~ ">2000"
  ))

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


# Dotchart
library(ggpubr)
library(RColorBrewer)

# Prepare the data
top_30_billionaires <- billio %>%
  arrange(rank) %>%
  head(30) %>%
  mutate(personName = as.factor(personName),
         category = as.factor(category),
         finalWorthBillions = finalWorth / 1000) 

# Create the dot chart with a built-in color palette
ggdotchart(top_30_billionaires, x = "personName", y = "finalWorthBillions", color = "category",
           palette = brewer.pal(n = length(unique(top_30_billionaires$category)), name = "Set1"),
           sorting = "descending",
           rotate = TRUE,
           dot.size = 2,
           y.text.col = TRUE) +
  theme_cleveland() +
  labs(x = "Billionaire", y = "Net Worth (in billions USD)", title = "Top 30 Billionaires' Net Worth")

# Create the dot chart with a built-in color palette

top_20_billionaires <- billio %>%
  arrange(rank) %>%
  head(20) %>%
  mutate(personName = as.factor(personName),
         country = as.factor(country),
         finalWorthBillions = finalWorth / 1000) 

ggdotchart(top_20_billionaires, x = "personName", y = "finalWorthBillions", color = "country",
           palette = brewer.pal(n = length(unique(top_20_billionaires$country)), name = "Set1"),
           sorting = "descending",
           rotate = TRUE,
           dot.size = 2,
           y.text.col = TRUE) +
  theme_cleveland() +
  labs(x = "Billionaire", y = "Net Worth (in billions USD)", title = "Top 30 Billionaires' Net Worth")

#Heatmap

# Aggregating the data
billionaires_agg <- billio %>%
  count(category, country) 

# Define breaks and corresponding colors
breaks <- c(30, 50, 100, 150, Inf)
colors <- c("lightblue", "royalblue", "navy", "darkblue", "black")
labels <- c("20", "50", "100", "150", "150+")

# Heatmap with custom color scale
ggplot(billionaires_agg, aes(x = category, y = country)) +
  geom_tile(aes(fill = n), color = 'white') +
  scale_fill_gradientn(
    colours = colors,
    breaks = breaks,
    labels = labels
  ) +
  theme_minimal() +
  labs(title = 'Distribution of Billionaires by Industry and Country') +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Parallel chart (for discrete variables) 
library(ggparallel)

ggparallel(names(billio)[c(14, 12, 36, 14)], data = billio, order = 0) +
  scale_fill_brewer(palette = "Paired", guide = "none") +
  scale_colour_brewer(palette = "Paired", guide = "none") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(face = "bold", color = "black"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(face = "bold")
  ) +
  scale_x_discrete(labels = c("Gender", "Source", "Rank Scale", "Gender"))


# Histogram
# Calculate the combined total finalWorth for each industry
industry_totals <- billio %>%
  group_by(industries) %>%
  summarize(totalWorth = sum(finalWorth)) %>%
  arrange(desc(totalWorth))

# Join the totals back to the main dataframe and reorder the industry factor
billio <- billio %>%
  mutate(industries = factor(industries, levels = rev(industry_totals$industries)))

# Create a horizontal stacked bar plot
ggplot(billio, aes(x = industries, y = finalWorth, fill = gender)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Female" = "pink", "Male" = "blue")) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Total Final Net Worth by Industry and Gender",
       x = "Total Final Net Worth (U.S. dollars)",
       y = "Industry") +
  theme(axis.text.y = element_text(angle = 0))


# Barplot with variable width

# Create age groups
billio$age_group <- cut(
  billio$age, 
  breaks = c(-Inf, 40, 50, 60, 70, 80, 90, 101), 
  labels = c("<40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-101")
)

# Calculate the sum of finalWorth for each age group
age_group_sums <- billio %>%
  group_by(age_group) %>%
  summarise(sum_finalWorth = sum(finalWorth))

# Print the results
print(age_group_sums)

# Create age groups
clusters <- data.frame(
  xmin = c(0, 40, 50, 60, 70, 80, 90), 
  ymin = c(0, 0, 0, 0, 0,0,0),
  xmax = c(40, 50, 60, 70, 80,90,101), 
  ymax = c(360, 854, 2804, 2758, 2662,1648,320),
  label = factor(c('1st cluster', '2nd cluster', '3rd cluster', '4th cluster', '5th cluster','6th cluster','7th cluster'))
)


ggplot(clusters) + 
  geom_rect(aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax,
                fill = (xmax - xmin)*(ymax - ymin)),
            color = 'grey78', alpha = .7) + 
  theme_minimal() + theme(legend.position = 'top') +
  geom_label(aes(y = ymax, label = paste0((xmax - xmin)*(ymax - ymin), " mln"), 
                 x = xmin + (xmax - xmin)/2), size = 8) +
  geom_label(aes(y = 1, x = xmin + (xmax - xmin)/2,
                 label = paste0('Segment: ', 1:7))) +
  labs(title = 'finalWorth by age group', x = 'Age [mln]', 
       y = 'Total finalworth', fill = 'finalWorth [mln]') +
  scale_fill_viridis_c(guide = guide_colorbar(barwidth = 20))
