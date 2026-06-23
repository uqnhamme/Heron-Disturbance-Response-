# Set the working directory
setwd("/Users/nick/Desktop/Heron_project/")
rm(list=ls())

# Load necessary libraries
library(tidyverse)
library(mgcv)
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)
library(cowplot)

# Load the data
df <- read.csv('reefcloud-point-summary-MEML_HeronReef_Coral-2026-01-14-filtered.csv')

# Delete sites we do not want
unique_sites <- unique(df$site)
print(unique_sites)

df <- subset(df, !grepl("LEI", site))

df <- subset(df, !grepl("Wistari", site))

df <- subset(df, !grepl("Tiny's island", site))

df <- subset(df, !grepl("Stanford", site))

# Columns to delete
colnames(df)

df <- subset(df, select = -c(DO.NOT.USE..Soft.Coral.Other.Bleached,	
                             DO.NOT.USE..Crustose.Coralline.Algae.on.Rubble,	DO.NOT.USE..Gorgonian.Bleached,
                             DO.NOT.USE..Lobophora.spp.,	DO.NOT.USE..Sargassum.spp.,	
                             DO.NOT.USE..Massive.Encrusting.Other,	DO.NOT.USE..Massive.Encrusting.Other.Bleached,	
                             DO.NOT.USE..Crown.of.Thorns.Sea.Star, DO.NOT.USE..Soft.Coral.Other,	
                             DO.NOT.USE..Poritidae..Porites.spp..Encrusting, DO.NOT.USE..Poritidae..Porites.spp..Encrusting.Bleached,	
                             DO.NOT.USE..Plate.Foliose.with.Ridges.Bleached,	DO.NOT.USE..Turbinaria.spp...algae.,	DO.NOT.USE..Unknown, 
                             DO.NOT.USE..Acroporidae..Isopora.spp..Branching.Encrusting.Bleached, DO.NOT.USE..Poritidae..Porites.spp..Branching.Bleached,
                             Transect.gear, Shadow, Fish, unique_id, survey_title, image_name, site_longitude, 
                             site_latitude, depth_m, transect, total, project, survey_id, site_id, site_reef_name, site_country_region, site_local_region))

# Create a line where we Remove photos that have less than 80 points after cleaning columns 
df$row_total <- rowSums(df[, -(1:3)])

df <- subset(df, row_total > 80)

df <- df %>% select(-row_total)

# Combining labels
# List of columns to combine for Algae and Cyanobacteria
algae_columns <- c("Macroalgae.Other",	"Benthic.Microalgae.on.Sand", "Crustose.Coralline.Algae",
                   "Caulerpa.spp.",	"Chlorodesmis.spp.",	"Dictyota.spp.", "Halimeda.spp.",	"Padina.spp.",	"Cyanobacteria")

# Create new 'Algae' column by summing the specified columns
df$Algae_Cyanobacteria <- rowSums(df[, algae_columns])

# To remove the original columns
df <- df[, !(names(df) %in% algae_columns)]


Dead_hard_coral_columns <- c("Epithelial.Algal.Matrix.on.DHC", "Epithelial.Algal.Matrix.on.Rubble")
df$EAM_Reef_Matrix <- rowSums(df[, Dead_hard_coral_columns])
df <- df[, !(names(df) %in% Dead_hard_coral_columns)]

# List of columns to combine for Invertebrates
invertebrate_columns <- c("Mobile.Invertebrate", "Sessile.Invertebrate.Other")
df$Other_Invertebrates <- rowSums(df[, invertebrate_columns])
df <- df[, !(names(df) %in% invertebrate_columns)]


# List of columns to combine for Soft Corals
soft_corals_columns <- c("Soft.Coral.Other", "Gorgonian")
df$Soft_Corals <- rowSums(df[, soft_corals_columns])
df <- df[, !(names(df) %in% soft_corals_columns)]

# Combining living corals
# List of columns to combine for Branching Corals
Branching_corals_columns <- c("Acroporidae.Arborescent", "Acroporidae..Acropora.spp..Hispidose", 
                              "Branching.Other", "Poritidae..Porites.spp..Branching", "Acroporidae..Isopora.spp..Branching.Encrusting", 
                              "Pocilloporidae")
df$Branching_corals <- rowSums(df[, Branching_corals_columns])
df <- df[, !(names(df) %in% Branching_corals_columns)]

# List of columns to combine for Massive Encrusting corals
Massive_Encrusting_corals_columns <- c("Ceroid.Massive.Encrusting..shared.walls.", "Meandroid.Massive.Encrusting..meandering.walls.", 
                                       "Plocoid.Massive.Encrusting..separate.walls.", "Poritidae.Massive.Encrusting")
df$Massive_Encrusting_corals <- rowSums(df[, Massive_Encrusting_corals_columns])
df <- df[, !(names(df) %in% Massive_Encrusting_corals_columns)]

# List of columns to combine for Plate Foliose corals - might including encrusting in the future 
Plate_Foliose_corals_columns <- c("Plate.Foliose.with.Ridges", "Plate.Foliose.with.Round.Corallites", "Small.Corallite.Plate.Encrusting")
df$Plate_Foliose_corals <- rowSums(df[, Plate_Foliose_corals_columns])
df <- df[, !(names(df) %in% Plate_Foliose_corals_columns)]

# List of columns to rename
names(df)[names(df) == "Acroporidae..Acropora.spp..Corymbose"] <- "Corymbose_corals"

names(df)[names(df) == "Acroporidae..Acropora.spp..Digitate"] <- "Digitate_corals"

names(df)[names(df) == "Acroporidae..Acropora.spp..Tabulate"] <- "Tabulate_corals"

names(df)[names(df) == "Free.Living.Coral"] <- "Free_Living_corals"

# Combining bleached corals
# List of columns to combine for Branching Bleached Corals
Branching_Bleached_corals_columns <- c("Acroporidae.Arborescent.Bleached", "Acroporidae..Acropora.spp..Hispidose.Bleached", 
                                       "Branching.Other.Bleached", "Pocilloporidae.Bleached")
df$Branching_Bleached_corals <- rowSums(df[, Branching_Bleached_corals_columns])
df <- df[, !(names(df) %in% Branching_Bleached_corals_columns)]


# List of columns to combine for Massive Encrusting Bleached corals
Massive_Encrusting_Bleached_corals_columns <- c("Ceroid.Massive.Encrusting.Bleached..shared.walls.", "Meandroid.Massive.Encrusting.Bleached..meandering.walls.", 
                                                "Plocoid.Massive.Encrusting.Bleached..separate.walls.", "Small.Corallite.Massive.Encrusting.Bleached")
df$Massive_Encrusting_Bleached_corals <- rowSums(df[, Massive_Encrusting_Bleached_corals_columns])
df <- df[, !(names(df) %in% Massive_Encrusting_Bleached_corals_columns)]


# List of columns to rename
names(df)[names(df) == "Plate.Foliose.Encrusting.Bleached"] <- "Plate_Foliose_Bleached_corals"

names(df)[names(df) == "Acroporidae..Acropora.spp..Corymbose.Bleached"] <- "Corymbose_Bleached_corals"

names(df)[names(df) == "Acroporidae..Acropora.spp..Digitate.Bleached"] <- "Digitate_Bleached_corals"

names(df)[names(df) == "Acroporidae..Acropora.spp..Tabulate.Bleached"] <- "Tabulate_Bleached_corals"

names(df)[names(df) == "Free.Living.Bleached"] <- "Free_Living_Bleached_corals"


# Combining Hard Corals for Stacked barplot
Live_hard_coral_columns <- c("Corymbose_corals", "Tabulate_corals", "Massive_Encrusting_corals", "Digitate_corals", "Free_Living_corals", "Branching_corals", "Plate_Foliose_corals")
df$Live_Hard_coral <- rowSums(df[, Live_hard_coral_columns])
df <- df[, !(names(df) %in% Live_hard_coral_columns)]

Bleached_Hard_coral_columns <- c("Corymbose_Bleached_corals", "Tabulate_Bleached_corals", "Branching_Bleached_corals", "Plate_Foliose_Bleached_corals", "Digitate_Bleached_corals", 
                                 "Free.Living.Coral.Bleached", "Massive_Encrusting_Bleached_corals")
df$Bleached_Hard_coral <- rowSums(df[, Bleached_Hard_coral_columns])
df <- df[, !(names(df) %in% Bleached_Hard_coral_columns)]

Other_Substrate_columns <- c("Soft_Corals", "Other_Invertebrates", "Algae_Cyanobacteria")
df$Other_Substrate <- rowSums(df[, Other_Substrate_columns])
df <- df[, !(names(df) %in% Other_Substrate_columns)]

# Checks
colnames(df)

unique_sites <- unique(df$site)
print(unique_sites)

# Convert date column to date format
df$date <- as.Date(df$date)
df$year <- year(df$date)

df <- df %>%
  mutate(month = substr(date, 6, 7))

# Now create the month_category column
df <- df %>%
  mutate(month_category = case_when(
    month == "01" ~ "January",
    month == "02" ~ "February", 
    month == "03" ~ "March",
    month == "04" ~ "April",
    month == "05" ~ "May",
    month == "06" ~ "June",
    month == "07" ~ "July",
    month == "08" ~ "August",
    month == "09" ~ "September",
    month == "10" ~ "October",
    month == "11" ~ "November",
    month == "12" ~ "December"
  ))

colnames(df)

species_columns <- c("Live_Hard_coral", "Bleached_Hard_coral", "Other_Substrate", "EAM_Reef_Matrix", "Sand")
species_colors <- rainbow(length(species_columns))
species_shapes <- 1:length(species_columns)

df <- df %>%
  mutate(month_year = paste(year, month, sep = "-"))

coverage_by_year_geomorphic_zone_2019 <- df %>%
  filter(year >= 2019 & year <= 2019) %>%
  mutate(geomorphic_zones = case_when(
    grepl("HR_Junction|HR_PlateLedge|HR_CoralGrotto|HR_Cascades|HR_NWTR7|HR_NWTR6|HR_NETR5|HR_NETR4|HR_BluePools|HR_LibbysLair|HR_Tenements|HR_GorgonianHole", site) ~ "Leeward Reef Slope North",
    grepl("HR_SETR3|HR_SETR2|HR_SETR1|HR_SETR0", site) ~ "Windward Reef Slope East",
    grepl("HR_HarrysBommie|HR_CoralCanyons|HR_Halfway|HR_CoralGardens|HR_HeronBommie|HR_PamsPoint", site) ~ "Windward Reef Slope West",
    grepl("HR_RFSETR1|HR_RFHalfway|HR_RFSharkBay|HR_RFPamsPoint|HR_RFWest|HR_RFPlateLedge|HR_RFResearchBeach|HR_RFBluePools|HR_RFSETR0", site) ~ "Reef Flat",
    !grepl("HR_", site) ~ "Unknown",
    TRUE ~ "Other"
  ))%>%
  group_by(geomorphic_zones) %>%
  summarise(across(all_of(species_columns), 
                   list(mean = ~mean(., na.rm = TRUE), 
                        se = ~sd(., na.rm = TRUE) / sqrt(n()))),
            .groups = 'drop') %>%
  pivot_longer(
    cols = -c(geomorphic_zones),
    names_to = c("Species", ".value"),
    names_pattern = "(.+)_(mean|se)"
  )

coverage_by_year_geomorphic_zone_2024 <- df %>%
  filter(year >= 2024 & month == "03") %>%
  mutate(geomorphic_zones = case_when(
    grepl("HR_Junction|HR_PlateLedge|HR_CoralGrotto|HR_Cascades|HR_NWTR7|HR_NWTR6|HR_NETR5|HR_NETR4|HR_BluePools|HR_LibbysLair|HR_Tenements|20221109_HR_BlueHole|HR_GorgonianHole", site) ~ "Leeward Reef Slope North",
    grepl("HR_SETR3|HR_SETR2|HR_SETR1|HR_SETR0", site) ~ "Windward Reef Slope East",
    grepl("HR_HarrysBommie|HR_CoralCanyons|HR_Halfway|HR_CoralGardens|HR_HeronBommie|HR_PamsPoint", site) ~ "Windward Reef Slope West",
    grepl("HR_RFSETR1|HR_RFHalfway|HR_RFSharkBay|HR_RFPamsPoint|HR_RFWest|HR_RFPlateLedge|HR_RFResearchBeach|HR_RFBluePools|HR_RFSETR0", site) ~ "Reef Flat",
    !grepl("HR_", site) ~ "Unknown",
    TRUE ~ "Other"
  ))%>%
  group_by(geomorphic_zones) %>%
  summarise(across(all_of(species_columns), 
                   list(mean = ~mean(., na.rm = TRUE), 
                        se = ~sd(., na.rm = TRUE) / sqrt(n()))),
            .groups = 'drop') %>%
  pivot_longer(
    cols = -c(geomorphic_zones),
    names_to = c("Species", ".value"),
    names_pattern = "(.+)_(mean|se)"
  )

coverage_by_year_geomorphic_zone_2025 <- df %>%
  filter(year >= 2025 & month == "10") %>%
  mutate(geomorphic_zones = case_when(
    grepl("HR_Junction|HR_PlateLedge|HR_CoralGrotto|HR_Cascades|HR_NWTR7|HR_NWTR6|HR_NETR5|HR_NETR4|HR_BluePools|HR_LibbysLair|HR_Tenements|20221109_HR_BlueHole|HR_GorgonianHole", site) ~ "Leeward Reef Slope North",
    grepl("HR_SETR3|HR_SETR2|HR_SETR1|HR_SETR0", site) ~ "Windward Reef Slope East",
    grepl("HR_HarrysBommie|HR_CoralCanyons|HR_Halfway|HR_CoralGardens|HR_HeronBommie|HR_PamsPoint", site) ~ "Windward Reef Slope West",
    grepl("HR_RFSETR1|HR_RFHalfway|HR_RFSharkBay|HR_RFPamsPoint|HR_RFWest|HR_RFPlateLedge|HR_RFResearchBeach|HR_RFBluePools|HR_RFSETR0", site) ~ "Reef Flat",
    !grepl("HR_", site) ~ "Unknown",
    TRUE ~ "Other"
  ))%>%
  group_by(geomorphic_zones) %>%
  summarise(across(all_of(species_columns), 
                   list(mean = ~mean(., na.rm = TRUE), 
                        se = ~sd(., na.rm = TRUE) / sqrt(n()))),
            .groups = 'drop') %>%
  pivot_longer(
    cols = -c(geomorphic_zones),
    names_to = c("Species", ".value"),
    names_pattern = "(.+)_(mean|se)"
  )

geo_titles <- c("Leeward Reef Slope North", "Windward Reef Slope East", "Windward Reef Slope West", "Reef Flat")

coverage_by_year_geomorphic_zone_2019 <- coverage_by_year_geomorphic_zone_2019 %>%
  mutate(Species = recode(Species,
                          "Live_Hard_coral" = "Live Hard Coral",
                          "Bleached_Hard_coral" = "Bleached Hard Coral",
                          "Other_Substrate" = "Other Substrate",
                          "EAM_Reef_Matrix" = "Dead Hard Coral", 
                          "Sand" = "Sand"))

species_colors <- c('Live Hard Coral' = '#FF7F50', 'Dead Hard Coral' = '#009E73', 'Other Substrate' = '#56B4E9', 'Sand' = '#F0E442', 'Bleached Hard Coral' = '#f2b5a7')


geomorphic_zone_order <- c("Leeward Reef Slope North",
                           "Reef Flat",
                           "Windward Reef Slope East",
                           "Windward Reef Slope West")

species_colors_order <- c("Other Substrate",
                          "Sand", 
                          "Dead Hard Coral", 
                          "Bleached Hard Coral", 
                          "Live Hard Coral")


coverage_by_year_geomorphic_zone_2024 <- coverage_by_year_geomorphic_zone_2024 %>%
  mutate(Species = recode(Species,
                          "Live_Hard_coral" = "Live Hard Coral",
                          "Bleached_Hard_coral" = "Bleached Hard Coral",
                          "Other_Substrate" = "Other Substrate",
                          "EAM_Reef_Matrix" = "Dead Hard Coral", 
                          "Sand" = "Sand"))

species_colors <- c('Live Hard Coral' = '#FF7F50', 'Dead Hard Coral' = '#009E73', 'Other Substrate' = '#56B4E9', 'Sand' = '#F0E442', 'Bleached Hard Coral' = '#f2b5a7')

geomorphic_zone_order <- c("Leeward Reef Slope North",
                           "Reef Flat",
                           "Windward Reef Slope East",
                           "Windward Reef Slope West")


species_colors_order <- c("Other Substrate",
                          "Sand", 
                          "Dead Hard Coral", 
                          "Bleached Hard Coral", 
                          "Live Hard Coral")


coverage_by_year_geomorphic_zone_2025 <- coverage_by_year_geomorphic_zone_2025 %>%
  mutate(Species = recode(Species,
                          "Live_Hard_coral" = "Live Hard Coral",
                          "Bleached_Hard_coral" = "Bleached Hard Coral",
                          "Other_Substrate" = "Other Substrate",
                          "EAM_Reef_Matrix" = "Dead Hard Coral", 
                          "Sand" = "Sand"))

species_colors <- c('Live Hard Coral' = '#FF7F50', 'Dead Hard Coral' = '#009E73', 'Other Substrate' = '#56B4E9', 'Sand' = '#F0E442', 'Bleached Hard Coral' = '#f2b5a7')


geomorphic_zone_order <- c("Leeward Reef Slope North",
                           "Reef Flat",
                           "Windward Reef Slope East",
                           "Windward Reef Slope West")


species_colors_order <- c("Other Substrate",
                          "Sand", 
                          "Dead Hard Coral", 
                          "Bleached Hard Coral", 
                          "Live Hard Coral")

coverage_by_year_geomorphic_zone_2019$geomorphic_zones <- factor(coverage_by_year_geomorphic_zone_2019$geomorphic_zones, 
                                                            levels = geomorphic_zone_order)


coverage_by_year_geomorphic_zone_2024$geomorphic_zones <- factor(coverage_by_year_geomorphic_zone_2024$geomorphic_zones, 
                                                                 levels = geomorphic_zone_order)


coverage_by_year_geomorphic_zone_2025$geomorphic_zones <- factor(coverage_by_year_geomorphic_zone_2025$geomorphic_zones, 
                                                                 levels = geomorphic_zone_order)


coverage_by_year_geomorphic_zone_2019$Species <- factor(coverage_by_year_geomorphic_zone_2019$Species, 
                                                                 levels = species_colors_order)

coverage_by_year_geomorphic_zone_2024$Species <- factor(coverage_by_year_geomorphic_zone_2024$Species, 
                                                        levels = species_colors_order)

coverage_by_year_geomorphic_zone_2025$Species <- factor(coverage_by_year_geomorphic_zone_2025$Species, 
                                                        levels = species_colors_order)

# Combine the dataframes
all_coverage_data <- rbind(coverage_by_year_geomorphic_zone_2019,
                           coverage_by_year_geomorphic_zone_2024,
                           coverage_by_year_geomorphic_zone_2025)

# Add a 'year' column to identify the time period
all_coverage_data$year <- c(rep("2019-11", nrow(coverage_by_year_geomorphic_zone_2019)),
                            rep("2024-03", nrow(coverage_by_year_geomorphic_zone_2024)),
                            rep("2025-10", nrow(coverage_by_year_geomorphic_zone_2025)))

all_coverage_data <- all_coverage_data %>%
  group_by(year, geomorphic_zones) %>%
  mutate(across(starts_with("mean"), ~ . / sum(.) * 100))

write.csv(all_coverage_data, "all_coverage_data.csv", row.names = FALSE)

ggplot(all_coverage_data, aes(x = geomorphic_zones, y = mean, fill = Species)) +
  geom_bar(stat = "identity", position = "stack", color = "black", width = 0.8) +  
  scale_fill_manual(values = species_colors) +
  labs(
    title = "",
    x = "",
    y = "Percent Cover",
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  ) +
  facet_wrap(~ year, ncol = 3)

ggsave("Stacked bar plots from 2019 to 2025 average geomorphic composition.pdf", width = 12, height = 8)









coverage_by_year_geomorphic_zone_2019 <- coverage_by_year_geomorphic_zone_2019 %>%
  group_by(geomorphic_zones) %>%
  mutate(across(starts_with("mean"), ~ . / sum(.) * 100))

# Create the stacked barplot
ggplot(coverage_by_year_geomorphic_zone_2019, aes(x = geomorphic_zones, y = mean, fill = Species)) +
  geom_bar(stat = "identity", position = "stack") +  
  scale_fill_manual(values = species_colors) +
  labs(
    title = "",
    x = "",
    y = "Percent Cover",
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 360, hjust = 0.5),
    legend.position = "bottom",
   panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
   panel.border = element_rect(color = "black", fill = NA, size = 1)
  ) 

ggsave("Stacked bar plot 2019 average geomorphic compositions.pdf", width = 10, height = 8)






