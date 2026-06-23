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
# Add row totals excluding the first three columns (year and site)
df$row_total <- rowSums(df[, -(1:3)])

df <- subset(df, row_total > 80)

df <- df %>% select(-row_total)

# Create a line where we remove transects which have less than 100 photos
site_year_totals <- df %>%
  group_by(site, year) %>%
  summarise(total_rows = n())

df <- df[df$site %in% site_year_totals$site[site_year_totals$total_rows > 100], ]

summary(df)

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

colnames(df)

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

# Assuming your data frame has a 'date' column in the format 'YYYY-MM-DD'
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
summary(df$Free_Living_corals)
str(df)


Branching_corals_columns <- c("Corymbose_corals", "Digitate_corals", "Branching_corals")
df$Branching_coral <- rowSums(df[, Branching_corals_columns])
df <- df[, !(names(df) %in% Branching_corals_columns)]


Branching_corals_bleached_columns <- c("Corymbose_Bleached_corals", "Digitate_Bleached_corals", "Branching_Bleached_corals")
df$Branching_Bleached_coral <- rowSums(df[, Branching_corals_bleached_columns])
df <- df[, !(names(df) %in% Branching_corals_bleached_columns)]

colnames(df)


# Specify species for plotting SPECIFIC FOR MARCH 2024
species_columns <- c("Massive_Encrusting_corals", "Free.Living.Coral.Bleached", "Tabulate_corals", "Tabulate_Bleached_corals","Free_Living_corals", "Branching_Bleached_coral", "Plate_Foliose_Bleached_corals", 
                    "Branching_coral", "Plate_Foliose_corals", "Massive_Encrusting_Bleached_corals")

#species_colors <- rainbow(length(species_columns))
species_colors <- c("grey", "brown", "grey","brown", "grey", "brown", "grey", "brown", "grey", "brown")
species_shapes <- 1:length(species_columns)


# # Filter data for March 2024
coverage_2024 <- df %>%
  filter(year == 2024 & month == "03") %>%
  mutate(geomorphic_zones = case_when(
    grepl("HR_Junction|HR_PlateLedge|HR_CoralGrotto|HR_Cascades|HR_NWTR7|HR_NWTR6|HR_NETR5|HR_NETR4|HR_BluePools|HR_LibbysLair|HR_Tenements|20221109_HR_BlueHole|HR_GorgonianHole", site) ~ "Leeward Reef Slope North",
    grepl("HR_SETR3|HR_SETR2|HR_SETR1|HR_SETR0", site) ~ "Windward Reef Slope East",
    grepl("HR_HarrysBommie|HR_CoralCanyons|HR_Halfway|HR_CoralGardens|HR_HeronBommie|HR_PamsPoint", site) ~ "Windward Reef Slope West",
    grepl("HR_RFChannel|HR_RFSETR1|HR_RFHalfway|HR_RFSharkBay|HR_RFPamsPoint|HR_RFWest|HR_RFPlateLedge|HR_RFResearchBeach|HR_RFBluePools|HR_RFSETR0", site) ~ "Reef Flat",
    !grepl("HR_", site) ~ "Unknown",
    TRUE ~ "Other" 
  )) %>%
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

# Filter data for March 2024
march_2024_sites <- df %>%
  filter(year == 2024 & month == "03") %>%
  select(site) %>%
  distinct()

# Print the sites
print(march_2024_sites)

geo_titles <- c("Leeward Reef Slope North", "Windward Reef Slope East", "Windward Reef Slope West", "Reef Flat")

bleached_coverage_2024 <- coverage_2024 %>%
  filter(grepl("Bleached", Species)) %>%
  group_by(geomorphic_zones) %>%
  summarise(
    Total_Bleached = sum(mean),
    Total_Bleached_SE = sum(se)
  )

# Create the individual plots
p1 <- ggplot(filter(coverage_2024, geomorphic_zones == "Leeward Reef Slope North"), aes(x = Species, y = mean, fill = Species)) +
 geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  scale_fill_manual(values = species_colors) +
  labs(title = "Leeward Reef Slope North", x = "", y = "Percent Cover (SE)") +
  scale_y_continuous(limits = c(0, 20), oob = scales::squish) +
  theme_minimal() + 
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1)
  )  

p2 <- ggplot(filter(coverage_2024, geomorphic_zones == "Windward Reef Slope East"), aes(x = Species, y = mean, fill = Species)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  scale_fill_manual(values = species_colors) +
  labs(title = "Windward Reef Slope East", x = "", y = "") +
  scale_y_continuous(limits = c(0, 20), oob = scales::squish) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1)
  ) 

p3 <- ggplot(filter(coverage_2024, geomorphic_zones == "Windward Reef Slope West"), aes(x = Species, y = mean, fill = Species)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  scale_fill_manual(values = species_colors) +
  labs(title = "Windward Reef Slope West", x = "Coral Functional Group", y = "Percent Cover (SE)") +
  scale_y_continuous(limits = c(0, 20), oob = scales::squish) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1)
  ) 

p4 <- ggplot(filter(coverage_2024, geomorphic_zones == "Reef Flat"), aes(x = Species, y = mean, fill = Species)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  scale_fill_manual(values = species_colors) +
  labs(title = "Reef Flat", x = "Coral Functional Group", y = "") +
  scale_y_continuous(limits = c(0, 20), oob = scales::squish) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1)
  ) 

# Arrange the individual plots into a 2x2 grid
plot_grid <- plot_grid(p1, p2, p3, p4, ncol = 2, align = "hv")

library(cowplot)
# Display the final plot
print(plot_grid)

# Save the plot
ggsave("Bleached coral cover composition March 2024.pdf", width = 20, height = 12, bg = "white")



##### Mean Relative Percentage
coverage_2024_mean_rel <- df %>%
  filter(year == 2024 & month == "03") %>%
  mutate(geomorphic_zones = case_when(
    grepl("HR_Junction|HR_PlateLedge|HR_CoralGrotto|HR_Cascades|HR_NWTR7|HR_NWTR6|HR_NETR5|HR_NETR4|HR_BluePools|HR_LibbysLair|HR_Tenements|20221109_HR_BlueHole|HR_GorgonianHole", site) ~ "Leeward Reef Slope North",
    grepl("HR_SETR3|HR_SETR2|HR_SETR1|HR_SETR0", site) ~ "Windward Reef Slope East",
    grepl("HR_HarrysBommie|HR_CoralCanyons|HR_Halfway|HR_CoralGardens|HR_HeronBommie|HR_PamsPoint", site) ~ "Windward Reef Slope West",
    grepl("HR_RFChannel|HR_RFSETR1|HR_RFHalfway|HR_RFSharkBay|HR_RFPamsPoint|HR_RFWest|HR_RFPlateLedge|HR_RFResearchBeach|HR_RFBluePools|HR_RFSETR0", site) ~ "Reef Flat",
    !grepl("HR_", site) ~ "Unknown",
    TRUE ~ "Other" 
  )) %>%
  group_by(geomorphic_zones, site) %>%
  mutate(total = rowSums(across(all_of(species_columns)), na.rm = TRUE)) %>%
  mutate(across(all_of(species_columns), ~ . / total * 100)) %>%
  select(-total) %>%
  ungroup() %>%
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


#import coverage_2024_mean_rel_manual

###perform square root transformation
coverage_2024_mean_rel_sqrt <- coverage_2024_mean_rel_manual %>%
  mutate(
    healthy = sqrt(healthy),
    bleached = sqrt(bleached), 
    healthy_se = sqrt(healthy_se),
    bleached_se = sqrt(bleached_se)
  )

geo_titles <- c("Leeward Reef Slope North", "Windward Reef Slope East", "Windward Reef Slope West", "Reef Flat")



geomorphic_zone_order <- c("Leeward Reef Slope North",
                           "Reef Flat",
                           "Windward Reef Slope East",
                           "Windward Reef Slope West")


species_colors <- c('Branching_coral' = '#FF7F50',  'Massive_Encrusting_corals' = '#009E73', 'Plate_Foliose_corals' = '#56B4E9', 'Tabulate_corals' = '#F0E442', 'Free_Living_corals' = '#f2b5a7')


df <- coverage_2024_mean_rel_sqrt %>%
  filter(geomorphic_zones == "Leeward Reef Slope North")

p1 <- ggplot(df, aes(x = healthy, y = bleached, color = Species)) +
  geom_point(size = 10, pch = 1, stroke = 1.5) +
  geom_errorbarh(aes(xmin = pmax(0, healthy - healthy_se), xmax = healthy + healthy_se),
                 height = 0.2, alpha = 0.6) +
  geom_errorbar(aes(ymin = pmax(0, bleached - bleached_se), ymax = bleached + bleached_se),
                width = 0.2, alpha = 0.6) +
  scale_color_manual(values = species_colors) +
  labs(title = "Leeward Reef Slope North", x = "", y = "Bleached Relative Abundance") +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1)) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 0.5)


df <- coverage_2024_mean_rel_sqrt %>%
  filter(geomorphic_zones == "Reef Flat")

p2 <- ggplot(df, aes(x = healthy, y = bleached, color = Species)) +
  geom_point(size = 10, pch = 1, stroke = 1.5) +
  geom_errorbarh(aes(xmin = pmax(0, healthy - healthy_se), xmax = healthy + healthy_se),
                 height = 0.5, alpha = 0.6) +
  geom_errorbar(aes(ymin = pmax(0, bleached - bleached_se), ymax = bleached + bleached_se),
                width = 0.5, alpha = 0.6) +
  #ggrepel::geom_text_repel(aes(label = Species), show.legend = FALSE, size = 3) +
  scale_color_manual(values = species_colors) +
  scale_shape_manual(values = species_shapes) +
  labs(title = "Reef Flat", x = "", y = "") +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  theme(legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1))  +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 0.5)


df <- coverage_2024_mean_rel_sqrt %>%
  filter(geomorphic_zones == "Windward Reef Slope East")

p3 <- ggplot(df, aes(x = healthy, y = bleached, color = Species)) +
  geom_point(size = 10, pch = 1, stroke = 1.5) +
  geom_errorbarh(aes(xmin = pmax(0, healthy - healthy_se), xmax = healthy + healthy_se),
                 height = 0.5, alpha = 0.6) +
  geom_errorbar(aes(ymin = pmax(0, bleached - bleached_se), ymax = bleached + bleached_se),
                width = 0.5, alpha = 0.6) +
  #ggrepel::geom_text_repel(aes(label = Species), show.legend = FALSE, size = 3) +
  scale_color_manual(values = species_colors) +
  scale_shape_manual(values = species_shapes) +
  labs(title = "Windward Reef Slope East", x = "Non-Bleached Relative Abundance", y = "Bleached Relative Abundance") +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1)) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 0.5)


df <- coverage_2024_mean_rel_sqrt %>%
  filter(geomorphic_zones == "Windward Reef Slope West")

p4 <- ggplot(df, aes(x = healthy, y = bleached, color = Species)) +
  geom_point(size = 10, pch = 1, stroke = 1.5) +
  geom_errorbarh(aes(xmin = pmax(0, healthy - healthy_se), xmax = healthy + healthy_se),
                 height = 0.5, alpha = 0.6) +
  geom_errorbar(aes(ymin = pmax(0, bleached - bleached_se), ymax = bleached + bleached_se),
                width = 0.5, alpha = 0.6) +
  #ggrepel::geom_text_repel(aes(label = Species), show.legend = FALSE, size = 3) +
  scale_color_manual(values = species_colors) +
  scale_shape_manual(values = species_shapes) +
  labs(title = "Windward Reef Slope West", x = "Non-Bleached Relative Abundance", y = "") +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1)) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 0.5)


library(patchwork)


plot.title = element_text(size = 10, face = "bold") +

(p1 | p2) / (p3 | p4) + plot_layout(guides = 'collect') & theme(legend.position='right')


# Save the plot
ggsave("Bleached coral cover composition March 2024 Mean Rel scatter11.pdf", width = 10, height = 8, bg = "white")
