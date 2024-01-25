# Wealth Unveiled: A Journey into the Lives of Billionaires Worldwide using R Visualizations
# Advanced Visualizations in R Final Project
# Nurdan Besli 457945
# Maciej Lorens 419763

# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(scales)
library(GGally)
library(RColorBrewer)
library(ggpubr)
library(ggparallel)
library(readxl)
library(tidygeocoder)
library(maps)
library(ggrepel)
library(viridis)

#### INITIAL PREPROCESSING ####

# Load the dataset
billio <- read.csv("data/Billionaires Statistics Dataset.csv")

# Rename a column with duplicate information
billio <- billio %>% 
  rename(source2 = source)

billio <- billio %>% 
  rename(source = selfMade)

# billio <- billio %>% rename(rank = ï..rank)

# Create labels for "source" and "gender" columns
billio <- billio %>%
  mutate(
    source = case_when(
      source == TRUE ~ "Founders/Entrepreneurs",
      source == FALSE ~ "Inherited",
      TRUE ~ as.character(source) 
    ),
    gender = case_when(
      gender == "F" ~ "Female",
      gender == "M" ~ "Male",
      TRUE ~ gender
    )
  )

# Create buckets of "rank"
billio <- billio %>%
  mutate(rankCategory = case_when(
    rank <500 ~ "<500",
    rank <1000 ~ "<1000",
    rank <1500 ~ "<1500",
    rank <2000 ~ "<2000",
    TRUE ~ ">2000"
  ))

# Create buckets of "age"
billio$age_group2 <- cut(
  billio$age, 
  breaks = c(-Inf, 10, 20, 30, 40, 50, 60, 70, 80, 90, 101), 
  labels = c("<10","10-19","20-29","30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90-101")
)

# Hong Kong is inconsistently assigned either as country or city
# We fixed this by assigning every appearance of Hong Kong to the "city" column and 
# the "country" column to mainland China
billio$city <- ifelse(billio$country == "Hong Kong", "Hong Kong", billio$city)
billio$country <- ifelse(billio$city == "Hong Kong", "China", billio$country)


#### GEOGRAPHY ####

# Get unique country-city combinations
unique_ctr_cty <- billio %>%
  select(country, city) %>%
  distinct()

# Get the geocode of the cities Billionaires reside in
# city_lon_lat <- unique_ctr_cty %>%
#   geocode(city = city, country = country, method = "osm")

# It's more convenient to save the ouput for later
# write.csv(city_lon_lat, "data/city_lon_lat.csv")

# Load the coordinates of cities for plotting
city_lon_lat <- read.csv("data/city_lon_lat.csv")
city_lon_lat <- city_lon_lat %>%
  rename(longitude_city = long, latitude_city = lat)

billio <- billio %>%
  left_join(city_lon_lat, by=c("country", "city"))

# If a city's geocode is not available, we take the country's geocode
billio$longitude_city <- ifelse(is.na(billio$longitude_city), billio$longitude_country, billio$longitude_city)
billio$latitude_city <- ifelse(is.na(billio$latitude_city), billio$latitude_country, billio$latitude_city)

# Get the count in each city
billio_city_count <- billio %>%
  group_by(country, city, latitude_city, longitude_city) %>%
  summarise(count=n(), .groups="drop") %>% 
  arrange(count)

# Get average worth of billionaires in each country
billio_country_worth <- billio %>%
  group_by(country) %>%
  summarise(avg_worth=mean(finalWorth), .groups="drop") %>% 
  arrange(avg_worth)

# Get only complete cases
billio_city_count <- billio_city_count[complete.cases(billio_city_count),]
billio_country_worth <- billio_country_worth[complete.cases(billio_country_worth),]

# Load the map of the world
world_map <- map_data("world")
# In map_data "USA" and "UK" is used, which is not consistent with the Billionaires dataset
world_map$region <- ifelse(world_map$region == "USA", "United States", world_map$region)
world_map$region <- ifelse(world_map$region == "UK", "United Kingdom", world_map$region)

# Connect the data on average worth to the world map
world_map <- world_map %>%
  left_join(billio_country_worth %>% select(country, avg_worth) %>% distinct(), by=join_by("region" == "country"))

# Create breaks for the color scale
mybreaks <- c(1, 5, 50)

# Build the map of the world
main_map <- world_map %>%
  ggplot() +
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group, fill=avg_worth), alpha=0.8) + 
  geom_point(data=billio_city_count, mapping=aes(x=longitude_city, y=latitude_city, color=count, alpha=count, size=count), stroke=FALSE) + 
  geom_text_repel(data=tail(billio_city_count, 10), mapping=aes(x=longitude_city, y=latitude_city, label=paste(city, "-", count)), 
                  size=5, color="white", fontface = "bold", bg.colour = "black", bg.r = .2) + 
  scale_size_continuous(name="Number of billionaires", trans="log", range=c(2, 7), breaks=mybreaks) +
  scale_alpha_continuous(name="Number of billionaires", trans="log", range=c(.5, .9), breaks=mybreaks) +
  scale_color_viridis(trans="log", name="Number of billionaires", breaks=mybreaks, direction=-1) + 
  scale_fill_continuous(name="Average worth of a billionaire") + 
  theme_void() + 
  guides(colour = guide_legend()) +
  ggtitle("Count of billionaires in the cities of the world") +
  theme(
    legend.position = c(0.15, 0.3),
    text = element_text(color = "#22211d", size=14),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size=22, face="bold"),
  )
main_map


# Europe is a very dense area, so it will get a separate plot
europe_zoom <- world_map %>%
  ggplot() +
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group, fill=avg_worth), alpha=0.8) +
  geom_point(data=billio_city_count, mapping=aes(x=longitude_city, y=latitude_city, color=count, alpha=count, size=count), stroke=FALSE) +
  geom_text_repel(data=tail(billio_city_count %>% filter(between(longitude_city, -20, 40), 
                                                         between(latitude_city, 35, 70)), 10), 
                  mapping=aes(x=longitude_city, y=latitude_city, label=paste(city, "-", count)), 
                  size=5, color="white", fontface = "bold", bg.colour = "black", bg.r = .2) + 
  theme_void() + 
  scale_size_continuous(name="Number of billionaires", trans="log", range=c(2, 7), breaks=mybreaks) +
  scale_alpha_continuous(name="Number of billionaires", trans="log", range=c(.5, 1), breaks=mybreaks) +
  scale_color_viridis(trans="log", name="Number of billionaires", breaks=mybreaks, direction=-1) + 
  scale_fill_continuous(name="Average worth of a billionaire") +
  guides(colour = guide_legend()) +
  ggtitle("Count of billionaires in the cities of Europe") + 
  theme(
    legend.position = c(0.15, 0.3),
    text = element_text(color = "#22211d", size=14),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size=22, face = "bold")
  ) + 
  xlim(c(-20, 40)) +
  ylim(c(35, 70))
europe_zoom


# HEATMAP - country vs. industry count
# Aggregating the data
billionaires_agg <- billio %>% 
  group_by(country, category) %>% 
  count()

# Top 30 countries in terms of count of billionaires
top_30_countries <- billio %>% 
  filter(country != "") %>%
  group_by(country) %>% 
  summarise(agg_count = n()) %>% 
  arrange(desc(agg_count)) %>% 
  head(30)

# Take only the top 30
billionaires_agg <- billionaires_agg %>% 
  inner_join(top_30_countries, by="country")

# Define breaks and corresponding colors
breaks <- c(30, 50, 100, 150, Inf)
colors <- c("lightblue", "royalblue", "navy", "darkblue", "black")
labels <- c("20", "50", "100", "150", "150+")

# Heatmap with custom color scale
# Order by the aggregated count
ggplot(billionaires_agg, aes(y = reorder(country, agg_count), x = category)) +
  geom_tile(aes(fill = n), color = 'white') +
  scale_fill_gradientn(
    colours = colors,
    breaks = breaks,
    labels = labels
  ) +
  theme_minimal() +
  labs(title = 'Distribution of Billionaires by Industry and Country', 
       x = "Industry", 
       y = "Country") +
  theme(
    axis.text.x = element_text(size=16, angle = 45, hjust = 1), 
    axis.title.x = element_text(size=18),
    axis.text.y = element_text(size=16), 
    axis.title.y = element_text(size=18),
    plot.title = element_text(size=22, face = "bold")
  )


#### DEEP DIVE ####

# DOTCHART - Top 30 Billioners with finalWorth, Industry and Gender
# Get top 30 billionaires
top_30_billionaires <- billio %>%
  arrange(rank) %>%
  head(30) %>%
  mutate(personName = as.factor(personName),
         industries = as.factor(industries),
         gender = as.factor(gender), 
         finalWorthBillions = finalWorth / 1000)

# Define a palette for the industries
industry_levels <- levels(top_30_billionaires$industries)

# Define shapes for gender: 16 for circle, 17 for triangle
shapes <- c("Female" = 16, "Male" = 17)

# Plot the dotchart
ggdotchart(top_30_billionaires, x = "personName", y = "finalWorthBillions",
           color = "industries", 
           shape = "gender", 
           sorting = "descending",
           rotate = TRUE,
           dot.size = 5,
           y.text.col = TRUE) +
  scale_shape_manual(values = shapes) +
  theme_cleveland() +
  labs(x = "Billionaire", y = "Net Worth (in billions USD)",
       title = "Top 30 Billionaires' Net Worth by Gender and Industry") +
  theme(plot.title = element_text(face = "bold", size = 22),
        legend.position = "right",
        axis.text.x = element_text(size=12, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        axis.title.x = element_text(size=12,face = "bold"))


# PARALLEL CHART (FOR DISCRETE VARIABLES)
# Plot billionaires by gender, source and rank
ggparallel(names(billio)[c(14, 12, 36, 14)], data = billio, order = 0) +
  scale_fill_brewer(palette = "Paired", guide = "none") +
  scale_colour_brewer(palette = "Paired", guide = "none") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 16, face = "bold", color = "black"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_x_discrete(labels = c("Gender", "Source", "Rank Scale", "Gender"))


# BARPLOT - Distribution of Billionaires by Industry and Gender
# Calculate the total number of billionaires for each industry and gender
industry_totals <- billio %>%
  group_by(industries, gender) %>%
  summarize(total_billionaires = n(), .groups = 'drop') 

# Calculate the cumulative sum for each gender within each industry
industry_totals <- industry_totals %>%
  arrange(industries, desc(gender)) %>%
  group_by(industries) %>%
  mutate(label_position = cumsum(total_billionaires) - (0.5 * total_billionaires))

# Calculate the grand total for each industry
industry_grand_totals <- industry_totals %>%
  summarize(grand_total = sum(total_billionaires)) %>%
  arrange(desc(grand_total))

# Join the grand totals back to the industry totals
industry_totals <- industry_totals %>%
  left_join(industry_grand_totals, by = "industries")

# Reorder industries based on the grand total
industry_totals$industries <- factor(industry_totals$industries, levels = industry_grand_totals$industries)

# Plot the distribution
ggplot(industry_totals, aes(x = industries, y = total_billionaires, fill = gender)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Male" = "#363062", "Female" = "#176B87")) +
  coord_flip() +
  geom_label(aes(label = total_billionaires, y = label_position), 
             fill = "white", color = "black", fontface = "bold", size = 3.5,
             label.size = 0.25, label.padding = unit(0.2, "lines"), 
             label.r = unit(0.15, "lines"), show.legend = FALSE) +
  geom_text(aes(label = grand_total, y = grand_total), 
            color = "black", size = 3.5, fontface = "bold", 
            show.legend = FALSE, hjust = -0.5) + 
  theme_minimal() +
  labs(title = "Distribution of Billionaires by Industry and Gender",
       x = "",
       y = "Number of Billionaires") +
  theme(plot.title = element_text(face = "bold", size = 20), 
        axis.title.x = element_text(size = 12, face = "bold"), 
        axis.text.y = element_text(size = 12, face = "bold"), 
        legend.text = element_text(face = "bold"), 
        legend.title = element_text(face = "bold")) 


# BARPLOT WITH VARIABLE WIDTH - Billionaire Count and Average Worth by Age Group
# Create age groups
billio$age_group <- cut(
  billio$age, 
  breaks = c(-Inf, 40, 50, 60, 70, 80, 90, 101), 
  labels = c("<40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-101")
)

# Calculate count of billionaires and mean of finalWorth for each age group
age_group_stats <- billio %>%
  group_by(age_group) %>%
  summarise(
    count = n(),
    meanWorth = mean(finalWorth)
  )

# Print the results
print(age_group_stats)

# Adjust clusters data frame to include count and average worth
clusters <- data.frame(
  xmin = c(0, 40, 50, 60, 70, 80, 90), 
  xmax = c(40, 50, 60, 70, 80, 90, 101), 
  count = c(85, 239, 679, 660, 573, 279, 60),
  meanWorth = c(4462, 3620, 4298, 4333, 4991, 6169, 7425), 
  segment = c("0-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90-101")
)

# Plot the barplot
ggplot(clusters) + 
  geom_rect(aes(xmin = xmin, ymin = 0, xmax = xmax, ymax = count, fill = meanWorth),
            color = 'grey78', alpha = .7) + 
  theme_minimal() +
  theme(legend.position = 'top',
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 22, face = "bold"), 
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 18))+
  geom_label(aes(x = (xmin + xmax)/2, y = count, label = meanWorth), 
             label.size = 0.5, size = 10, vjust = 0.6) +
  geom_label(aes(y = 5, x = xmin + (xmax - xmin)/2,
                 label = segment), size = 8) + # Set size here
  labs(title = 'Number of Billionaires and Average Worth by Age Group', 
       x = 'Age Group', 
       y = 'Number of Billionaires', 
       fill = 'Average Final Worth (mln USD)') +
  scale_fill_viridis_c(guide = guide_colorbar(barwidth = 20)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-25, 700))


# BUBBLE CHART - Number of Billionaires in Industries by Source
ggplot(billio, aes(y=industries, x=..count.., color=source)) + 
  geom_point(stat="count", alpha=0.7, size=4) + 
  labs(
    title="Number of Billionaires in Industries by Source",
    y="", 
    x="Number of Billionaires",
    color="Source"  
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face="bold", size=20),
    axis.title.x = element_text(face="bold", size=14),
    axis.text = element_text(face="bold"),
    legend.title = element_text(face="bold", size=14),  
    legend.text = element_text(size=12),
    axis.text.y = element_text(size = 12, angle = 0, hjust = 1, face="bold"),
    axis.text.x = element_text(size=12, face="bold"),
    legend.position = "top",  
    legend.direction = "horizontal",
    panel.grid.major = element_line(color="darkgray"),  
    panel.grid.minor = element_line(color="darkgray")  
  )


# BUMPCHART - Top 10 Billionaires by Year
# Read the Excel file
billiobyyears <- read_excel("data/billionaires2.xlsx")

# Filter and arrange the data
billiobyyears <- billiobyyears %>%
  arrange(Year, Rank)

# Plot the bumpchart
p <- ggplot(data = billiobyyears, aes(x = Year, y = Rank, group = Name)) +
  geom_line(aes(color = Name), linewidth = 2) +
  geom_point(aes(color = Name, fill = Name), shape = 21, size = 3) +
  scale_color_manual(values = rainbow(length(unique(billiobyyears$Name)))) +
  scale_fill_manual(values = rainbow(length(unique(billiobyyears$Name)), alpha = 0.5)) +
  scale_x_continuous(breaks = unique(billiobyyears$Year), expand = c(0.05, 0.05)) +
  scale_y_reverse(breaks = 1:10) +  
  labs(title = "Top 10 Billionaires by Year",
       x = "Year",
       y = "Rank") +
  theme_minimal() +
  theme(legend.position = "none", 
        plot.title = element_text(size = 22, face = "bold"), 
        axis.title.x = element_text(size = 16, face = "bold"), 
        axis.title.y = element_text(size = 16, face = "bold"),  
        axis.text.x = element_text(size = 14),  
        axis.text.y = element_text(size = 14), 
        panel.grid = element_blank(), 
        axis.text = element_text(face = "bold", size = 16))  

# Add labels to the points
p + geom_text(data = billiobyyears %>% filter(Rank == 1 | Year %in% c(2017, 2023)), aes(label = Name),
              hjust = -0.1, vjust = 0.5, color = "black", size = 5) + coord_cartesian(xlim = c(2017, 2024)) 


# MULTI-HISTOGRAMS WITH FACET_WRAP - Distribution of Billionaires by Age Group and Gender-source Category
# Create a combined gender-source column
billio$Gender_Source <- with(billio, paste(gender, source))

# Overall histogram data
overall_hist_data <- billio %>%
  filter(!is.na(age_group2)) %>%  
  group_by(age_group2) %>%
  summarize(overall_count = n(), .groups = 'drop')

# Specific histogram data
specific_hist_data <- billio %>%
  group_by(Gender_Source, age_group2) %>%
  summarize(specific_count = n(), .groups = 'drop')

# Combine data
combined_hist_data <- merge(overall_hist_data, specific_hist_data, by = "age_group2", all.x = TRUE)
combined_hist_data$specific_count[is.na(combined_hist_data$specific_count)] <- 0

# Plot the multi-histogram
ggplot(combined_hist_data, aes(x = age_group2)) +
  geom_bar(aes(y = overall_count), stat = "identity", fill = "grey", color = "black") +
  geom_bar(data = subset(combined_hist_data, Gender_Source %in% c("Male Founders/Entrepreneurs", "Male Inherited", "Female Founders/Entrepreneurs", "Female Inherited")), 
           aes(y = specific_count, fill = Gender_Source), stat = "identity", color = "black") +
  facet_wrap(~ Gender_Source, scales = "free_y", ncol = 2) +
  labs(title = "Distribution of Billionaires by Age Group,Gender and Source",
       x = "Age Group",
       y = "Number of Billionaires") +
  scale_fill_manual(values = c("Male Founders/Entrepreneurs" = "blue", "Male Inherited" = "green", "Female Founders/Entrepreneurs" = "red", "Female Inherited" = "purple")) +
  theme_minimal() +
  theme(  plot.title = element_text(face = "bold", size = 20),
          legend.title = element_text(face = "bold", size = 12),
          legend.text = element_text(face = "bold"),
          axis.text.x = element_text(face = "bold", size = 12,angle = 45, hjust = 1),
          axis.title = element_text(face = "bold", size = 14),
          strip.text = element_text(face = "bold", size = 12)
  )


#### ZODIAC SIGNS ####

# Assign billionaires to zodiac signs based on birthdays
billio <- billio %>%
  mutate(zodiac_sign = case_when(
    (birthMonth == 3 & birthDay >= 21) | (birthMonth == 4 & birthDay <= 19) ~ "Aries",
    (birthMonth == 4 & birthDay >= 20) | (birthMonth == 5 & birthDay <= 20) ~ "Taurus",
    (birthMonth == 5 & birthDay >= 21) | (birthMonth == 6 & birthDay <= 20) ~ "Gemini",
    (birthMonth == 6 & birthDay >= 21) | (birthMonth == 7 & birthDay <= 22) ~ "Cancer",
    (birthMonth == 7 & birthDay >= 23) | (birthMonth == 8 & birthDay <= 22) ~ "Leo",
    (birthMonth == 8 & birthDay >= 23) | (birthMonth == 9 & birthDay <= 22) ~ "Virgo",
    (birthMonth == 9 & birthDay >= 23) | (birthMonth == 10 & birthDay <= 22) ~ "Libra",
    (birthMonth == 10 & birthDay >= 23) | (birthMonth == 11 & birthDay <= 21) ~ "Scorpio",
    (birthMonth == 11 & birthDay >= 22) | (birthMonth == 12 & birthDay <= 21) ~ "Sagittarius",
    (birthMonth == 12 & birthDay >= 22) | (birthMonth == 1 & birthDay <= 19) ~ "Capricorn",
    (birthMonth == 1 & birthDay >= 20) | (birthMonth == 2 & birthDay <= 18) ~ "Aquarius",
    (birthMonth == 2 & birthDay >= 19) | (birthMonth == 3 & birthDay <= 20) ~ "Pisces",
    TRUE ~ NA_character_ 
  ))

# Filter out billionaires with NA in zodiac_sign
billio_filtered <- billio %>%
  filter(!is.na(zodiac_sign))

# Manually set the colors for the source categories
source_colors <- c("Founders/Entrepreneurs" = "#FFC0D9", "Inherited" = "#7FC7D9")

# Plot the barplot of billionaires by zodiac sign
ggplot(billio_filtered, aes(x = zodiac_sign, fill = source)) +
  geom_bar(color = "black") + 
  scale_fill_manual(values = source_colors) +  
  theme_minimal() +
  labs(title = "Billionaires by Zodiac Sign and Source",
       x = "Zodiac Sign",
       y = "Number of Billionaires") +
  theme(plot.title = element_text(face = "bold", size = 22),
        axis.text.x = element_text(angle = 90, hjust = 1, face ="bold", size = 16),
        axis.text.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold", size =16),
        axis.title.y = element_text(face = "bold", size =16),
        legend.title = element_text(face = "bold")
  )


# Filter out billionaires born on January 1st and those with NA in zodiac_sign
billio_filtered_excluding_jan1 <- billio %>%
  filter(!(birthMonth == 1 & birthDay == 1), !is.na(zodiac_sign))

# Plot (Jan1 excluded)
ggplot(billio_filtered_excluding_jan1, aes(x = zodiac_sign, fill = source)) +
  geom_bar(color = "black") + 
  scale_fill_manual(values = source_colors) +  
  theme_minimal() +
  labs(title = "Billionaires by Zodiac Sign and Source (Jan1 excluded)",
       x = "Zodiac Sign",
       y = "Number of Billionaires") +
  theme(plot.title = element_text(face = "bold", size = 22),
        axis.text.x = element_text(angle = 90, hjust = 1, face ="bold", size = 16),
        axis.text.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold", size =16),
        axis.title.y = element_text(face = "bold", size =16),
        legend.title = element_text(face = "bold")
  )


#### ANALYSIS OF BILLIONES BORN IN JAN ####

# Create a flag for billionaires born on January 1st
billio_flagged <- billio_filtered %>%
  mutate(birthday_flag = if_else(birthMonth == 1 & birthDay == 1, "Born on Jan 1st", "Not Born on Jan 1st"))

# Define colors for the bar plot
colors_birthday_flag <- c("Born on Jan 1st" = "#363062", "Not Born on Jan 1st" = "darkslategray4")

# Plot the pieplot of Billionaires born on January 1sr
ggplot(billio_flagged, aes(x = "", fill = birthday_flag)) +
  geom_bar(width = 1, color = "black", stat = "count") +
  scale_fill_manual(values = colors_birthday_flag) +
  coord_polar(theta = "y") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(face = "bold", size = 16), 
    legend.title = element_blank(),
    legend.direction = "horizontal", 
    legend.margin = margin(t = 0, unit = "pt")
  ) +
  labs(title = "Billionaires Born on Jan1 vs. Other Dates",
       x = "",
       y = "") +
  guides(fill = guide_legend(title = "Category"))


# Filter the dataset for billionaires born on January 1st
billio_jan1 <- billio %>%
  filter(birthMonth == 1, birthDay == 1)

# Plot the barplot of billionaires born on January 1st
ggplot(billio_jan1, aes(x = countryOfCitizenship)) +
  geom_bar(fill = "#363062", color = "black") +
  theme_minimal() +
  labs(title = "Number of Billionaires Born on January 1st by Country",
       x = "",
       y = "Number of Billionaires") +
  theme(plot.title = element_text(size = 22, face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1, face = "bold", color = "black", size = 12),
        axis.text.y = element_text(face = "bold", color = "black", size = 10),
        axis.title.y = element_text(size = 16, face = "bold"))
