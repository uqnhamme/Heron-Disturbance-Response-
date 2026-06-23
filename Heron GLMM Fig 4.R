# Set the working directory
setwd("/Users/nick/Desktop/Heron_project/")
rm(list=ls())

# sources
source("modelDiagTests.R")

# Load necessary libraries
library(gamm4)
library(devtools)
library(tidyverse)
library(lme4)
library(mgcv)
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)
library(glmmTMB)

# Load the data
df <- read.csv('reefcloud-point-summary-MEML_HeronReef_Coral-2026-01-14-filtered.csv')

# look for non-integer counts
dfNonInt <- rowSums(df[,18:ncol(df)] %% 1 > 0)

# remove rows with non-integer counts
df <- df[dfNonInt == 0, ]

# Delete sites we do not need - double check with new output - sites are re-organized -
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
                             Transect.gear, Shadow, Fish, unique_id, survey_title, image_name, depth_m, transect, total, project, survey_id, site_id, site_reef_name, site_country_region, site_local_region))


# Remove photos that have less than 80 points after cleaning columns 
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
algae_columns <- c("Macroalgae.Other", "Benthic.Microalgae.on.Sand",
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

colnames(df)

# Checks
str(df)

summary(df)

colnames(df)
summary(df)


# Combining Hard Corals for GAMS solely
Live_hard_coral_columns <- c("Corymbose_corals", "Tabulate_corals", "Massive_Encrusting_corals", "Digitate_corals", "Free_Living_corals", "Branching_corals", "Plate_Foliose_corals")
df$Live_Hard_coral <- rowSums(df[, Live_hard_coral_columns])
df <- df[, !(names(df) %in% Live_hard_coral_columns)]

Bleached_Hard_coral_columns <- c("Corymbose_Bleached_corals", "Tabulate_Bleached_corals", "Branching_Bleached_corals", "Plate_Foliose_Bleached_corals", "Digitate_Bleached_corals", 
                                 "Free.Living.Coral.Bleached", "Massive_Encrusting_Bleached_corals")
df$Bleached_Hard_coral <- rowSums(df[, Bleached_Hard_coral_columns])
df <- df[, !(names(df) %in% Bleached_Hard_coral_columns)]

Other_Hard_Substrate_columns <- c("Soft_Corals", "Other_Invertebrates", "Algae_Cyanobacteria", "Crustose.Coralline.Algae")
df$Other_Hard_Substrate <- rowSums(df[, Other_Hard_Substrate_columns])
df <- df[, !(names(df) %in% Other_Hard_Substrate_columns)]

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

colnames(df)

#print photo count check
site_summary <- df %>%
  group_by(site, year, month) %>%
  summarise(
    photo_count = n(),
    .groups = 'drop'
  ) %>%
  arrange(site, year, month)

# View the summary
print(site_summary)

species_columns <- c("Live_Hard_coral", "EAM_Reef_Matrix", "Bleached_Hard_coral")
species_colors <- rainbow(length(species_columns))
species_shapes <- 1:length(species_columns)

# for the bionomial
coverage_by_year_geomorphic_zone <- df %>%
  filter(year >= 2019 & year <= 2025) %>%
  mutate(geomorphic_zones = case_when(
    grepl("HR_Junction|HR_PlateLedge|HR_CoralGrotto|HR_Cascades|HR_NWTR7|HR_NWTR6|HR_NETR5|HR_NETR4|HR_BluePools|HR_LibbysLair|HR_Tenements|20221109_HR_BlueHole|HR_GorgonianHole", site) ~ "Leeward Reef Slope North",
    grepl("HR_SETR3|HR_SETR2|HR_SETR1|HR_SETR0", site) ~ "Windward Reef Slope East",
    grepl("HR_HarrysBommie|HR_CoralCanyons|HR_Halfway|HR_CoralGardens|HR_HeronBommie|HR_PamsPoint", site) ~ "Windward Reef Slope West",
    grepl("HR_RFChannel|HR_RFSETR1|HR_RFHalfway|HR_RFSharkBay|HR_RFPamsPoint|HR_RFWest|HR_RFPlateLedge|HR_RFResearchBeach|HR_RFBluePools|HR_RFSETR0", site) ~ "Reef Flat",
    !grepl("HR_", site) ~ "Unknown",
    TRUE ~ "Other"
  ))%>%
  group_by(year, geomorphic_zones, site, site_latitude, site_longitude, month) %>%
  summarise(across(all_of(species_columns), 
                   list(sum = ~sum(., na.rm = TRUE), 
                        se = ~sd(., na.rm = TRUE) / sqrt(n()))),
            .groups = 'drop') %>%
  pivot_longer(
    cols = -c(year, geomorphic_zones, site, site_latitude, site_longitude, month),
    names_to = c("Species", ".value"),
    names_pattern = "(.+)_(sum|se)"
  )

df <- df |> mutate(geomorphic_zones = case_when(
  grepl("HR_Junction|HR_PlateLedge|HR_CoralGrotto|HR_Cascades|HR_NWTR7|HR_NWTR6|HR_NETR5|HR_NETR4|HR_BluePools|HR_LibbysLair|HR_Tenements|20221109_HR_BlueHole|HR_GorgonianHole", site) ~ "Leeward Reef Slope North",
  grepl("HR_SETR3|HR_SETR2|HR_SETR1|HR_SETR0", site) ~ "Windward Reef Slope East",
  grepl("HR_HarrysBommie|HR_CoralCanyons|HR_Halfway|HR_CoralGardens|HR_HeronBommie|HR_PamsPoint", site) ~ "Windward Reef Slope West",
  grepl("HR_RFChannel|HR_RFSETR1|HR_RFHalfway|HR_RFSharkBay|HR_RFPamsPoint|HR_RFWest|HR_RFPlateLedge|HR_RFResearchBeach|HR_RFBluePools|HR_RFSETR0", site) ~ "Reef Flat",
  !grepl("HR_", site) ~ "Unknown",
  TRUE ~ "Other"))

#pivot to wider table _ note you changed "mean to sum"
species_data <- coverage_by_year_geomorphic_zone %>% 
  pivot_wider(id_cols = c(year, site, month, geomorphic_zones), names_from = Species, values_from = sum)

species_data$month <- as.numeric(species_data$month)

species_data <- species_data %>%
  mutate(idyear = as.numeric((year - 2018) + month / 12)) %>%
  select(idyear, everything())

write.csv(species_data , "species_data.csv", row.names = FALSE)

modelDF = as.data.frame(species_data)
modelDF$geomorphic_zones <- as.factor(modelDF$geomorphic_zones)
modelDF$site <- as.factor(modelDF$site)

modelDF$month[(modelDF$month %in% c(10,11)) &
                (modelDF$year %in% c(2019,2020,2023))] = 10

modelDF <- modelDF %>%
  mutate(idyear = as.numeric((year - 2018) + month / 12)) %>%
  select(idyear, everything())

modelDF <- modelDF[!((modelDF$year == 2024 & modelDF$month == 05) |
                       (modelDF$year == 2024 & modelDF$month == 08) |
                       (modelDF$year == 2025 & modelDF$month == 03)),]


modelDF$timeZone <- as.factor(paste0(modelDF$idyear, ":", modelDF$geomorphic_zones))

levels(modelDF$timeZone)

gam_models_binomial <- lapply(species_columns, function(species){
  
  modelDF$success = modelDF[,species]
  modelDF$failure = rowSums(modelDF[,species_columns[species_columns != species]])
  
  table(modelDF$year, modelDF$idyear)
  modelDF$yearint <- as.factor(floor(modelDF$idyear)-1)

  
  tempModel <- glmer(cbind(success, failure) ~ timeZone + (1|site), #+ (1|idyear),
                     data = modelDF,
                     family = binomial(link = "logit"))
  
  return(tempModel)
}) 

names(gam_models_binomial) <- species_columns

# Create the new_data data frame
new_data <- modelDF[!duplicated(modelDF$timeZone), c("timeZone", "idyear", "geomorphic_zones")]
new_data <- new_data[order(new_data$idyear, new_data$geomorphic_zones),]

# Generate predictions for each species and assign them to the data frame
result_data <- do.call("rbind", lapply(species_columns, function(species){
  print(species)
  
  #preds <- as.data.frame(predict(gam_models_binomial[[species]]$gam, 
  preds <- as.data.frame(predict(gam_models_binomial[[species]], 
                                 newdata = new_data,
                                 #exclude = "site", se.fit=TRUE))
                                 re.form = NA, se.fit=TRUE))
  cbind(new_data, preds, species = species)
}))

result_data$lci <- plogis(result_data$fit - 1.96 * result_data$se.fit)
result_data$uci <- plogis(result_data$fit + 1.96 * result_data$se.fit)
result_data$prob <- plogis(result_data$fit)

result_data$species<- factor(result_data$species, 
                             levels = c("Live_Hard_coral", "Bleached_Hard_coral", 
                                        "EAM_Reef_Matrix"))

# Function to convert idyear to date, starting from October 2019
idyear_to_date <- function(idyear) {
  origin_date <- as.Date("2019-10-01")  # October 1, 2019 as the starting point
  yearComp = (idyear %/% 1)
  dayComp = (idyear %% 1)
  
  year(origin_date) = 2018 + yearComp
  yday(origin_date) = round(dayComp * ifelse(year(origin_date) %% 4, 366, 365))
  return(origin_date)
}

# Apply the function to create a new date column
result_data$new_date <- idyear_to_date(result_data$idyear) + 
  seq(-15,15, len=4)[c(1,4,2,3)][result_data$geomorphic_zones]

# Check the results
head(result_data[c("idyear", "new_date")])

# Check the range of dates
range(result_data$new_date)

# Define the field season dates
field_season_dates <- as.Date(c("2019-10-01", "2020-10-01", "2021-11-01", "2022-11-01", "2023-10-01", "2024-03-01", "2024-11-01", "2025-06-01", "2025-10-01"))

species_colors <- c('Leeward Reef Slope North' = '#FF7F50', 'Windward Reef Slope East' = '#009E73', 'Windward Reef Slope West' = '#56B4E9', 'Reef Flat' = '#F0E442')

geomorphic_zone_order <- c("Leeward Reef Slope North",
                           "Reef Flat",
                           "Windward Reef Slope East",
                           "Windward Reef Slope West")

result_data$geomorphic_zones <- factor(result_data$geomorphic_zones, levels = geomorphic_zone_order)

#for glm
ggplot(result_data, aes(x = new_date, y = prob, color = geomorphic_zones)) +
  geom_path() +
  geom_point() +
  geom_vline(xintercept = as.Date("2025-03-15"), linetype = "dashed", color = "black", size = 0.5) +
  geom_segment(aes(x = new_date, y = uci, yend= lci)) +
  facet_wrap(~ species, ncol=1) +
  annotate("rect", xmin = as.Date("2024-01-01"), xmax = as.Date("2024-05-31"), 
           ymin = -Inf, ymax = Inf, fill = "gray50", alpha = 0.4) +
  annotate("rect", xmin = as.Date("2020-02-15"), xmax = as.Date("2020-05-31"), 
           ymin = -Inf, ymax = Inf, fill = "gray50", alpha = 0.4) +
  scale_color_manual(values = species_colors) +   # For the lines
  labs(title = "",
       x = "",
       y = "Predicted Proportion", color = "Geomorphic Zone") + 
  scale_x_date(date_labels = "%Y-%m", breaks = field_season_dates) +
  theme(legend.position = "right",
        axis.ticks.length = unit(1, "mm"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())

ggsave("Heron GLMM Figure 4.pdf", width = 10, height = 8, bg = "white")

# output model summary tables
mapply(x=gam_models_binomial,n=1:3, function(x,n){
  coefs <- summary(x)$coefficients
  factSplit <- do.call("rbind", strsplit(rownames(coefs), ":"))
  factSplit <- gsub("timeZone", "", factSplit)
  
  time <- paste0(modelDF$month, "/", modelDF$year)[match(factSplit[,1], modelDF$idyear)]
  zone <- factSplit[,2]
  
  coefMat <- cbind(`Survey time` = time,
                   Zone = zone,
                   apply(coefs, 2, function(y){sprintf("%.3f", y)}))
  
  write.csv(coefMat, paste0("./modelSummaryTable",
                            c("Live", "Bleached", "EAM")[n],
                            ".csv"))
})

# random effect variance
lapply(gam_models_binomial, function(x){as.data.frame(VarCorr(x))})

# keep r2 cond - fixed and random effects together and r2 marg - just fixed effects
compare_performance(gam_models_binomial)


