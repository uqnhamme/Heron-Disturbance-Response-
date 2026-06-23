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
library(vegan)
library(cowplot)
library(ggrepel)

# Load the data
df <- read.csv('reefcloud-point-summary-MEML_HeronReef_Coral-2026-01-14-filtered.csv')

# Delete sites we do not need
unique_sites <- unique(df$site)
print(unique_sites)

df <- subset(df, !grepl("LEI", site))

df <- subset(df, !grepl("Wistari", site))

df <- subset(df, !grepl("Tiny's island", site))

df <- subset(df, !grepl("Stanford", site))

# Columns to delete
# Change names as per new dataframe - all will say Do Not Use - 
colnames(df)

df <- subset(df, select = -c(DO.NOT.USE..Soft.Coral.Other.Bleached,	
                             DO.NOT.USE..Crustose.Coralline.Algae.on.Rubble,	DO.NOT.USE..Gorgonian.Bleached,
                             DO.NOT.USE..Lobophora.spp.,	DO.NOT.USE..Sargassum.spp.,	
                             DO.NOT.USE..Massive.Encrusting.Other,	DO.NOT.USE..Massive.Encrusting.Other.Bleached,	
                             DO.NOT.USE..Crown.of.Thorns.Sea.Star, DO.NOT.USE..Soft.Coral.Other,	
                             DO.NOT.USE..Poritidae..Porites.spp..Encrusting, DO.NOT.USE..Poritidae..Porites.spp..Encrusting.Bleached,	
                             DO.NOT.USE..Plate.Foliose.with.Ridges.Bleached,	DO.NOT.USE..Turbinaria.spp...algae.,	DO.NOT.USE..Unknown, 
                             DO.NOT.USE..Acroporidae..Isopora.spp..Branching.Encrusting.Bleached, DO.NOT.USE..Poritidae..Porites.spp..Branching.Bleached,
                             Transect.gear, Shadow, Fish, unique_id, survey_title, image_name, depth_m, transect, total, project, survey_id, site_id, site_reef_name, site_country_region, site_local_region))

# Create a line where we Remove photos that have less than 80 points after cleaning columns 
# Add row totals excluding the first three columns (year and site)
df$row_total <- rowSums(df[, -(1:3)])

df <- subset(df, row_total > 80)

df <- df %>% select(-row_total)

# Create a line where we remove transects which have less than 100 photos
site_year_totals <- df %>%
  group_by(site, year) %>%
  summarise(total_rows = n())

df <- df[df$site %in% site_year_totals$site[site_year_totals$total_rows > 100], ]

# Combining Other Categories
# List of columns to combine for Algae and Cyanobacteria
algae_columns <- c("Macroalgae.Other",	"Benthic.Microalgae.on.Sand",
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

# Create new 'Invertebrates' column by summing the specified columns
df$Other_Invertebrates <- rowSums(df[, invertebrate_columns])

# Remove the original columns
df <- df[, !(names(df) %in% invertebrate_columns)]


# List of columns to combine for Soft Corals
soft_corals_columns <- c("Soft.Coral.Other", "Gorgonian")

# Create new 'Soft Corals' column by summing the specified columns
df$Soft_Corals <- rowSums(df[, soft_corals_columns])

# Remove the original columns
df <- df[, !(names(df) %in% soft_corals_columns)]


# Combining living corals
# List of columns to combine for Branching Corals
Branching_corals_columns <- c("Acroporidae.Arborescent", "Acroporidae..Acropora.spp..Hispidose", 
                              "Branching.Other", "Poritidae..Porites.spp..Branching", "Acroporidae..Isopora.spp..Branching.Encrusting", 
                              "Pocilloporidae")

# Create new 'Branching_corals' column by summing the specified columns
df$Branching_corals <- rowSums(df[, Branching_corals_columns])

# Remove the original columns
df <- df[, !(names(df) %in% Branching_corals_columns)]


# List of columns to combine for Massive Encrusting corals
Massive_Encrusting_corals_columns <- c("Ceroid.Massive.Encrusting..shared.walls.", "Meandroid.Massive.Encrusting..meandering.walls.", 
                                       "Plocoid.Massive.Encrusting..separate.walls.", "Poritidae.Massive.Encrusting")

# Create new 'Massive_Encrusting_corals' column by summing the specified columns
df$Massive_Encrusting_corals <- rowSums(df[, Massive_Encrusting_corals_columns])

# Remove the original columns
df <- df[, !(names(df) %in% Massive_Encrusting_corals_columns)]


# List of columns to combine for Plate Foliose corals - might including encrusting in the future 
Plate_Foliose_corals_columns <- c("Plate.Foliose.with.Ridges", "Plate.Foliose.with.Round.Corallites", "Small.Corallite.Plate.Encrusting")

# Create new 'Plate_Foliose_corals' column by summing the specified columns
df$Plate_Foliose_corals <- rowSums(df[, Plate_Foliose_corals_columns])

# Remove the original columns
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

# Create new 'Branching_corals' column by summing the specified columns
df$Branching_Bleached_corals <- rowSums(df[, Branching_Bleached_corals_columns])

# Remove the original columns
df <- df[, !(names(df) %in% Branching_Bleached_corals_columns)]


# List of columns to combine for Massive Encrusting Bleached corals
Massive_Encrusting_Bleached_corals_columns <- c("Ceroid.Massive.Encrusting.Bleached..shared.walls.", "Meandroid.Massive.Encrusting.Bleached..meandering.walls.", 
                                                "Plocoid.Massive.Encrusting.Bleached..separate.walls.", "Small.Corallite.Massive.Encrusting.Bleached")

# Create new 'Massive_Encrusting_corals' column by summing the specified columns
df$Massive_Encrusting_Bleached_corals <- rowSums(df[, Massive_Encrusting_Bleached_corals_columns])

# Remove the original columns
df <- df[, !(names(df) %in% Massive_Encrusting_Bleached_corals_columns)]


# List of columns to rename

names(df)[names(df) == "Plate.Foliose.Encrusting.Bleached"] <- "Plate_Foliose_Bleached_corals"

names(df)[names(df) == "Acroporidae..Acropora.spp..Corymbose.Bleached"] <- "Corymbose_Bleached_corals"

names(df)[names(df) == "Acroporidae..Acropora.spp..Digitate.Bleached"] <- "Digitate_Bleached_corals"

names(df)[names(df) == "Acroporidae..Acropora.spp..Tabulate.Bleached"] <- "Tabulate_Bleached_corals"

names(df)[names(df) == "Free.Living.Bleached"] <- "Free_Living_Bleached_corals"

# Checks
str(df)

summary(df)

colnames(df)

colnames(df)
summary(df)

# Checks
unique_sites <- unique(df$site)
print(unique_sites)

# Convert date column to date format
df$date <- as.Date(df$date)
df$year <- year(df$date)

# Assuming your dataframe is called 'df'
column_names <- names(df)

# Print the column names
print(column_names)

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

print(column_names)
summary(df)

### Additional grouping for NMDS
Branching_corals_columns <- c("Corymbose_corals", "Digitate_corals", "Branching_corals")
df$Branching_coral <- rowSums(df[, Branching_corals_columns])
df <- df[, !(names(df) %in% Branching_corals_columns)]

#Plate_Foliose_corals_columns <- c("Tabulate_corals", "Plate_Foliose_corals")
#df$Plate_Foliose_coral <- rowSums(df[, Plate_Foliose_corals_columns])
#df <- df[, !(names(df) %in% Plate_Foliose_corals_columns)]


# Modify this list to match your actual species column names
species_columns <- c("Massive_Encrusting_corals", "Free_Living_corals", "Branching_coral", "Plate_Foliose_corals", "Tabulate_corals")
#species_columns <- c("Live_Hard_coral", "EAM_Reef_Matrix", "Other_Benthos", "Bleached_Hard_coral", "Sand", "Algae_Cyanobacteria", "Crustose.Coralline.Algae")
#species_columns <- c("Live_Hard_coral", "EAM_Reef_Matrix", "Bleached_Hard_coral")
species_colors <- rainbow(length(species_columns))
species_shapes <- 1:length(species_columns)

# Prepare the data
coverage_by_year_geomorphic_zone <- df %>%
  filter(year >= 2019 & year <= 2025) %>%
  mutate(geomorphic_zones = case_when(
    grepl("HR_Junction|HR_PlateLedge|HR_CoralGrotto|HR_Cascades|HR_NWTR7|HR_NWTR6|HR_NETR5|HR_NETR4|HR_BluePools|HR_LibbysLair|HR_Tenements|HR_GorgonianHole", site) ~ "Leeward Reef Slope North",
    grepl("HR_SETR3|HR_SETR2|HR_SETR1|HR_SETR0", site) ~ "Windward Reef Slope East",
    grepl("HR_HarrysBommie|HR_CoralCanyons|HR_Halfway|HR_CoralGardens|HR_HeronBommie|HR_PamsPoint", site) ~ "Windward Reef Slope West",
    grepl("HR_RFChannel|HR_RFSETR1|HR_RFHalfway|HR_RFSharkBay|HR_RFPamsPoint|HR_RFWest|HR_RFPlateLedge|HR_RFResearchBeach|HR_RFBluePools|HR_RFSETR0", site) ~ "Reef Flat",
    !grepl("HR_", site) ~ "Unknown",
    TRUE ~ "Other"
  ))%>%
  group_by(year, month, geomorphic_zones, site) %>%
  summarise(across(all_of(species_columns), 
                   list(mean = ~mean(., na.rm = TRUE), 
                        se = ~sd(., na.rm = TRUE) / sqrt(n()))),
            .groups = 'drop') %>%
  pivot_longer(
    cols = -c(year, month, geomorphic_zones, site),
    names_to = c("Species", ".value"),
    names_pattern = "(.+)_(mean|se)"
  )


sites_per_year <- coverage_by_year_geomorphic_zone %>%
  group_by(year, month) %>%
  summarise(
    sites_surveyed = paste(unique(site), collapse = ", "),
    number_of_sites = n_distinct(site),
    .groups = 'drop'
  )

write.csv(sites_per_year , "sites_per_year .csv", row.names = FALSE)


#pivot to wider table
species_data <- coverage_by_year_geomorphic_zone %>% 
  pivot_wider(id_cols = c(year, site, month, geomorphic_zones), names_from = Species, values_from = mean)


# Relativize the data between 0 and 1
species_data_relativized <- species_data

# Identify the columns to relativize (excluding the first 7)
cols_to_relativize <- 5:ncol(species_data)

compData = as.matrix(species_data_relativized[,cols_to_relativize])
compDataRel = prop.table(compData, margin=1)
species_data_relativized[,cols_to_relativize] = as.data.frame(compDataRel)


species_data_relativized$month[(species_data_relativized$month %in% c(10,11)) &
                (species_data_relativized$year %in% c(2019,2020,2023))] = 10

species_data_relativized$month <- as.numeric(as.character(species_data_relativized$month))

species_data_relativized <- species_data_relativized[!((species_data_relativized$year == 2024 & species_data_relativized$month == 05) |
                       (species_data_relativized$year == 2024 & species_data_relativized$month == 08) |
                       (species_data_relativized$year == 2025 & species_data_relativized$month == 03)),]


# Save the results
write.csv(species_data_relativized, "species_data_relativized.csv", row.names = FALSE)


# Calculate Bray-Curtis dissimilarity matrix
bc_dist <- vegdist(sqrt(species_data_relativized[, -c(1,2,3,4)]), method = "bray")


# NMDS ordination
nmds <- metaMDS(bc_dist, k = 2, trymax = 100)

# Create a data frame with the NMDS coordinates and grouping variables
nmds_data <- data.frame(NMDS1 = nmds$points[, 1], NMDS2 = nmds$points[, 2], geomorphic_zones = species_data_relativized$geomorphic_zones, year = species_data_relativized$year, month = species_data_relativized$month, site = species_data_relativized$site)
#nmds_data <- data.frame(NMDS1 = nmds$points[, 1], NMDS2 = nmds$points[, 2], geomorphic_zones = as.factor(species_data_relativized$geomorphic_zones), year = as.factor(species_data_relativized$year), month = as.factor(species_data_relativized$month), site = as.factor(species_data_relativized$site))
#nmds_data <- data.frame(NMDS1 = nmds$points[, 1], NMDS2 = nmds$points[, 2], geomorphic_zones = species_data_relativized$geomorphic_zones, year = species_data_relativized$year, month=species_data_relativized$month)

# Extract the species scores
species_fit <- envfit(nmds, species_data_relativized[, -c(1,2,3,4)], permutations = 999)
species_scores <- as.data.frame(species_fit$vectors$arrows)

rownames(species_scores)

rownames(species_scores) <- c("Massive / Encrusting", "Free Living", "Branching", "Plate / Foliose", "Tabulate")

# Remove the row for "sd"
#species_scores <- species_scores[rownames(species_scores) != "sd", ]

# Create unique year-month combinations
nmds_data$year_month <- paste(nmds_data$year, nmds_data$month, sep="-")

# Get unique year-month combinations
unique_year_months <- unique(nmds_data$year_month)

stress_value <- nmds$stress


# Create the 4-panel figure

# legend <- get_legend(
#   ggplot(nmds_data, aes(x = NMDS1, y = NMDS2, color = year_month)) +
#     scale_color_manual(values = year_month_colors, name = "Year-Month") +
#     guides(color = guide_legend(title.position = "top", title.hjust = 0.5))
# )


hull_data <- function(nmds_data) {
  nmds_data %>% 
    group_by(year_group) %>% 
    slice(chull(NMDS1, NMDS2))
}



nmds_data <- nmds_data %>%
  mutate(
    year = as.numeric(as.character(year)),
    month = as.numeric(as.character(month)),
    year_group = case_when(
      (year == 2019) ~ "2019",
      (year == 2020) ~ "2020",
      (year == 2021) ~ "2021",
      (year == 2022) ~ "2022",
      (year == 2023) ~ "2023",
      (year == 2024 & month == 05) ~ "2024",
      (year == 2024 & month == 11) ~ "2024",
      (year == 2025 & month == 06) ~ "2025",
      (year == 2025 & month == 10) ~ "2025", 
      TRUE ~ as.character(year)
    )
  )


species_colors <- c('2019' = '#b35806', '2020' = '#FF7F50', '2021' = '#fee0b6', '2022' = '#d8daeb', '2023' = '#009E73', '2024' = '#56B4E9', '2025' = '#542788')
species_shapes <- c('2019' = 57, '2020' = 48, '2021' = 49, '2022' = 50, '2023' = 51, '2024' = 52, '2025' = 53)

yearMeans <- aggregate(cbind(NMDS1, NMDS2) ~ year_group,
                       data = nmds_data[nmds_data$geomorphic_zones == "Leeward Reef Slope North", ],
                       FUN = mean)

p1 <- ggplot(nmds_data[nmds_data$geomorphic_zones == "Leeward Reef Slope North", ], 
             aes(x = NMDS1, y = NMDS2, color = year_group)) +
  geom_point(aes(shape = year_group), size = 2, alpha = 0.5) + 
  labs(x = "", y = "nMDS2", title = "Leeward Reef Slope North",
       subtitle = paste("Stress:", round(stress_value, 3))) +
  scale_color_manual(values = species_colors) +
  scale_shape_manual(values = species_shapes) +
  guides(shape = guide_legend(override.aes = list(shape = species_shapes))) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1.0),
        axis.ticks.length = unit(1, "mm"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 10), 
        axis.title = element_text(size = 10), 
        axis.text = element_text(size = 10)) +
  coord_fixed(ratio = 1.0, xlim = c(-0.5, 0.5), ylim = c(-0.5, 0.5)) +
  geom_segment(data = species_scores, 
               aes(x = 0, y = 0, xend = NMDS1 * 0.4, yend = NMDS2 * 0.4), 
               arrow = arrow(length = unit(0.3, "cm")), 
               color = "black", linewidth = 0.1, inherit.aes = FALSE) +  
  geom_text_repel(data = species_scores, 
                  aes(x = NMDS1 * 0.4, y = NMDS2 * 0.4, 
                      label = paste0("'",rownames(species_scores), "'")), 
                  size = 3.5, color = "black", min.segment.length = 0.5, 
                  max.overlaps = 20, parse = TRUE, inherit.aes = FALSE) +  
  geom_path(data = yearMeans, aes(x = NMDS1, y = NMDS2), 
            color = "black", linewidth = 0.3, inherit.aes = FALSE) +
  geom_point(data = yearMeans, aes(x=NMDS1, y=NMDS2, color = year_group), 
           pch=16, size = 3)

yearMeans <- aggregate(cbind(NMDS1, NMDS2) ~ year_group,
                       data = nmds_data[nmds_data$geomorphic_zones == "Reef Flat", ],
                       FUN = mean)

p2 <- ggplot(nmds_data[nmds_data$geomorphic_zones == "Reef Flat", ], 
             aes(x = NMDS1, y = NMDS2, color = year_group)) +  
  geom_point(aes(shape = year_group), size = 2, alpha = 0.5) + 
  labs(x = "", y = "", title = "Reef Flat") +
  scale_color_manual(values = species_colors) +  
  scale_shape_manual(values = species_shapes) +
  guides(shape = guide_legend(override.aes = list(shape = species_shapes))) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1.0),
        axis.ticks.length = unit(1, "mm"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 10),  
        axis.text = element_text(size = 10)) + 
  coord_fixed(ratio = 1.0, xlim = c(-0.5, 0.5), ylim = c(-0.5, 0.5)) +
  geom_segment(data = species_scores, aes(x = 0, y = 0, xend = NMDS1 * 0.4, yend = NMDS2 * 0.4), 
               arrow = arrow(length = unit(0.3, "cm")), color = "black", linewidth = 0.1,
               inherit.aes = FALSE) +  
  geom_text_repel(data = species_scores, 
                  aes(x = NMDS1 * 0.4, y = NMDS2 * 0.4, 
                      label = paste0("'",rownames(species_scores), "'")), 
                  size = 3.5, color = "black", min.segment.length = 0.5, 
                  max.overlaps = 20, parse = TRUE, inherit.aes = FALSE) + 
  geom_path(data = yearMeans, aes(x = NMDS1, y = NMDS2), 
            color = "black", linewidth = 0.3, inherit.aes = FALSE) +
  geom_point(data = yearMeans, aes(x=NMDS1, y=NMDS2, color = year_group, shape = year_group), 
             size=3, pch= 16)  


yearMeans <- aggregate(cbind(NMDS1, NMDS2) ~ year_group,
                       data = nmds_data[nmds_data$geomorphic_zones == "Windward Reef Slope East", ],
                       FUN = mean)

p3 <- ggplot(nmds_data[nmds_data$geomorphic_zones == "Windward Reef Slope East", ], 
             aes(x = NMDS1, y = NMDS2, color = year_group)) +  
  geom_point(aes(shape = year_group), size = 2, alpha = 0.5) + 
  labs(x = "nMDS1", y = "nMDS2", title = "Windward Reef Slope East") +
  scale_color_manual(values = species_colors) + 
  scale_shape_manual(values = species_shapes) +
  guides(shape = guide_legend(override.aes = list(shape = species_shapes))) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1.0),
        axis.ticks.length = unit(1, "mm"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 10),  
        axis.text = element_text(size = 10)) + 
  coord_fixed(ratio = 1.0, xlim = c(-0.5, 0.5), ylim = c(-0.5, 0.5)) +
  geom_segment(data = species_scores, 
               aes(x = 0, y = 0, xend = NMDS1 * 0.4, yend = NMDS2 * 0.4), 
               arrow = arrow(length = unit(0.3, "cm")), 
               color = "black", linewidth = 0.1, inherit.aes = FALSE) +   
  geom_text_repel(data = species_scores, 
                  aes(x = NMDS1 * 0.4, y = NMDS2 * 0.4, 
                      label = paste0("'",rownames(species_scores), "'")), 
                  size = 3.5, color = "black", min.segment.length = 0.5, 
                  max.overlaps = 20, parse = TRUE, inherit.aes = FALSE) + 
  geom_path(data = yearMeans, aes(x = NMDS1, y = NMDS2), 
            color = "black", linewidth = 0.3, inherit.aes = FALSE) +
  geom_point(data = yearMeans, aes(x=NMDS1, y=NMDS2, color = year_group, shape = year_group), 
             size=3,  shape = 16)


yearMeans <- aggregate(cbind(NMDS1, NMDS2) ~ year_group,
                       data = nmds_data[nmds_data$geomorphic_zones == "Windward Reef Slope West", ],
                       FUN = mean)


p4 <- ggplot(nmds_data[nmds_data$geomorphic_zones == "Windward Reef Slope West", ], 
             aes(x = NMDS1, y = NMDS2, color = year_group)) +  
  geom_point(aes(shape = year_group), size = 2, alpha = 0.5) + 
  labs(x = "nMDS1", y = "", title = "Windward Reef Slope West") +
  scale_color_manual(values = species_colors) +  
  scale_shape_manual(values = species_shapes) +
  guides(shape = guide_legend(override.aes = list(shape = species_shapes))) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1.0),
        axis.ticks.length = unit(1, "mm"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 10),  
        axis.text = element_text(size = 10)) +  
  coord_fixed(ratio = 1.0, xlim = c(-0.5, 0.5), ylim = c(-0.5, 0.5)) +
  geom_segment(data = species_scores, aes(x = 0, y = 0, xend = NMDS1 * 0.4, yend = NMDS2 * 0.4), 
               arrow = arrow(length = unit(0.3, "cm")), color = "black", linewidth = 0.1,
               inherit.aes = FALSE) +  
  geom_text_repel(data = species_scores, 
                  aes(x = NMDS1 * 0.4, y = NMDS2 * 0.4, 
                      label = paste0("'",rownames(species_scores), "'")), 
                  size = 3.5, color = "black", min.segment.length = 0.5, 
                  max.overlaps = 20, parse = TRUE, inherit.aes = FALSE) +  
  geom_path(data = yearMeans, aes(x = NMDS1, y = NMDS2), 
            color = "black", linewidth = 0.3, inherit.aes = FALSE) +
  geom_point(data = yearMeans, aes(x=NMDS1, y=NMDS2, color = year_group, shape = year_group), 
             size=3,  shape = 16)  


library(patchwork)

(p1 | p2) / (p3 | p4) + plot_layout(guides = 'collect') & theme(legend.position='right')

# Save the plot
ggsave("Heron NMDS Figure 5.pdf", width = 10, height = 8, bg = "white")















### extra non-neccessary steps

# Perform betadisper analysis
bd <- betadisper(bc_dist, group = nmds_data$year)
# Perform permutation test for homogeneity of multivariate dispersions
perm_test <- permutest(bd, pairwise = TRUE, permutations = 999)
# Print the results
print(perm_test)

bd_int <- betadisper(bc_dist, group = interaction(nmds_data$geomorphic_zones, nmds_data$year))
perm_test_int <- permutest(bd_int, pairwise = TRUE, permutations = 999)
print(perm_test_int)


# Perform PERMANOVA and pairwise PERMANOVA
species_data$year <- as.factor(species_data$year)
species_data$month <- as.factor(species_data$month)

permanova_result <- adonis2(bc_dist ~  geomorphic_zones * year, data = species_data, permutations = 999, by = "terms")
print(permanova_result)

write.csv(permanova_result, "permanova_result.csv", row.names = FALSE)

source("https://raw.githubusercontent.com/pmartinezarbizu/pairwiseAdonis/master/pairwiseAdonis/R/pairwise.adonis.R")

geo_groups <- species_data$geomorphic_zones
geo_pairwise <- pairwise.adonis(bc_dist, geo_groups, p.adjust.m = "bonferroni")
print(geo_pairwise)
geo_p_values <- geo_pairwise$p.adjusted
print(geo_p_values)
write.csv(geo_pairwise, "geo_pairwise.csv", row.names = FALSE)

year_groups <- species_data$year
year_pairwise <- pairwise.adonis(bc_dist, year_groups, p.adjust.m = "bonferroni")
print(year_pairwise)
year_p_values <- year_pairwise$p.adjusted
write.csv(year_pairwise, "year_pairwise.csv", row.names = FALSE)

geo_year_groups <- interaction(species_data$geomorphic_zones, species_data$year)
view(geo_year_groups)
geo_year_pairwise <- pairwise.adonis(bc_dist, geo_year_groups, p.adjust.m = "bonferroni")
print(geo_year_pairwise)
geo_year_p_values <- geo_year_pairwise$p.adjusted
write.csv(geo_year_pairwise, "geo_year_pairwise.csv", row.names = FALSE)

#pairwise_p_values <- sapply(pairwise_permanova$p.adjusted, function(x) x)


# Filter species_data to only include the "reef flat" geomorphic zone
reef_flat_data <- subset(species_data, geomorphic_zones == "Reef Flat")
# Create the geo_year_groups variable for the reef flat data
geo_year_groups <- interaction(reef_flat_data$geomorphic_zones, reef_flat_data$year)
view(geo_year_groups)
geo_year_pairwise <- pairwise.adonis(bc_dist, geo_year_groups, p.adjust.m = "bonferroni")


# For geo p-values
# Assuming you have the geo_p_values object
geo_p_matrix <- matrix(geo_p_values, nrow = length(levels(geo_groups)), 
                        ncol = length(levels(geo_groups)))
# Set the row and column names
colnames(geo_p_matrix) <- rownames(geo_p_matrix) <- levels(geo_groups)

# Create the ggplot
ggplot(data = reshape2::melt(geo_p_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "red", high = "white") +
  theme_minimal() +
  labs(title = "Pairwise PERMANOVA p-values for Bank", x = "Geo", y = "Geo")

# For year p-values
year_p_matrix <- matrix(year_p_values, nrow = length(levels(year_groups)), 
                        ncol = length(levels(year_groups)))
colnames(year_p_matrix) <- rownames(year_p_matrix) <- levels(year_groups)

ggplot(data = reshape2::melt(year_p_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "red", high = "white") +
  theme_minimal() +
  labs(title = "Pairwise PERMANOVA p-values for Year", x = "Year", y = "Year")

# For geo-year interaction p-values
geo_year_p_matrix <- matrix(geo_year_p_values, nrow = length(levels(geo_year_groups)), 
                             ncol = length(levels(geo_year_groups)))
colnames(geo_year_p_matrix) <- rownames(geo_year_p_matrix) <- levels(geo_year_groups)

ggplot(data = reshape2::melt(geo_year_p_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "red", high = "white") +
  theme_minimal() +
  labs(title = "Pairwise PERMANOVA p-values for Bank and Year Interaction", x = "Geo:Year", y = "Geo:Year")