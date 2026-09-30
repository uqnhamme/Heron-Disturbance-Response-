## Figure 2: overall benthic composition by geomorphic zone, 
## October 2019 (baseline), March 2024 (peak
## bleaching), October 2025 (final survey). Five mutually exclusive
## categories: Live Hard Coral, Bleached Hard Coral, Dead Hard Coral
## (EAM), Other Substrate, Sand.

library(tidyverse)
library(cowplot)

## Load and clean
df <- read.csv('reefcloud-point-summary-MEML_HeronReef_Coral-2026-01-14-filtered.csv')

df <- subset(df, !grepl("LEI", site))
df <- subset(df, !grepl("Wistari", site))
df <- subset(df, !grepl("Tiny's island", site))
df <- subset(df, !grepl("Stanford", site))

df <- subset(df, select = -c(DO.NOT.USE..Soft.Coral.Other.Bleached,
                             DO.NOT.USE..Crustose.Coralline.Algae.on.Rubble, DO.NOT.USE..Gorgonian.Bleached,
                             DO.NOT.USE..Lobophora.spp., DO.NOT.USE..Sargassum.spp.,
                             DO.NOT.USE..Massive.Encrusting.Other, DO.NOT.USE..Massive.Encrusting.Other.Bleached,
                             DO.NOT.USE..Crown.of.Thorns.Sea.Star, DO.NOT.USE..Soft.Coral.Other,
                             DO.NOT.USE..Poritidae..Porites.spp..Encrusting, DO.NOT.USE..Poritidae..Porites.spp..Encrusting.Bleached,
                             DO.NOT.USE..Plate.Foliose.with.Ridges.Bleached, DO.NOT.USE..Turbinaria.spp...algae., DO.NOT.USE..Unknown,
                             DO.NOT.USE..Acroporidae..Isopora.spp..Branching.Encrusting.Bleached, DO.NOT.USE..Poritidae..Porites.spp..Branching.Bleached,
                             Transect.gear, Shadow, Fish, unique_id, survey_title, image_name, site_longitude,
                             site_latitude, depth_m, transect, total, project, survey_id, site_id, site_reef_name, site_country_region, site_local_region))

numeric_cols <- names(df)[sapply(df, is.numeric)]
non_numeric_cols <- names(df)[!sapply(df, is.numeric)]
cat("Columns excluded from row_total (non-numeric):", paste(non_numeric_cols, collapse = ", "), "\n")
df$row_total <- rowSums(df[, numeric_cols])
df <- subset(df, row_total > 80)
df <- df %>% select(-row_total)

## Combine labels
algae_columns <- c("Macroalgae.Other", "Benthic.Microalgae.on.Sand", "Crustose.Coralline.Algae",
                   "Caulerpa.spp.", "Chlorodesmis.spp.", "Dictyota.spp.", "Halimeda.spp.", "Padina.spp.", "Cyanobacteria")
df$Algae_Cyanobacteria <- rowSums(df[, algae_columns])
df <- df[, !(names(df) %in% algae_columns)]

Dead_hard_coral_columns <- c("Epithelial.Algal.Matrix.on.DHC", "Epithelial.Algal.Matrix.on.Rubble")
df$EAM_Reef_Matrix <- rowSums(df[, Dead_hard_coral_columns])
df <- df[, !(names(df) %in% Dead_hard_coral_columns)]

invertebrate_columns <- c("Mobile.Invertebrate", "Sessile.Invertebrate.Other")
df$Other_Invertebrates <- rowSums(df[, invertebrate_columns])
df <- df[, !(names(df) %in% invertebrate_columns)]

soft_corals_columns <- c("Soft.Coral.Other", "Gorgonian")
df$Soft_Corals <- rowSums(df[, soft_corals_columns])
df <- df[, !(names(df) %in% soft_corals_columns)]

Branching_corals_columns <- c("Acroporidae.Arborescent", "Acroporidae..Acropora.spp..Hispidose",
                              "Branching.Other", "Poritidae..Porites.spp..Branching", "Acroporidae..Isopora.spp..Branching.Encrusting",
                              "Pocilloporidae")
df$Branching_corals <- rowSums(df[, Branching_corals_columns])
df <- df[, !(names(df) %in% Branching_corals_columns)]

Massive_Encrusting_corals_columns <- c("Ceroid.Massive.Encrusting..shared.walls.", "Meandroid.Massive.Encrusting..meandering.walls.",
                                       "Plocoid.Massive.Encrusting..separate.walls.", "Poritidae.Massive.Encrusting")
df$Massive_Encrusting_corals <- rowSums(df[, Massive_Encrusting_corals_columns])
df <- df[, !(names(df) %in% Massive_Encrusting_corals_columns)]

Plate_Foliose_corals_columns <- c("Plate.Foliose.with.Ridges", "Plate.Foliose.with.Round.Corallites", "Small.Corallite.Plate.Encrusting")
df$Plate_Foliose_corals <- rowSums(df[, Plate_Foliose_corals_columns])
df <- df[, !(names(df) %in% Plate_Foliose_corals_columns)]

names(df)[names(df) == "Acroporidae..Acropora.spp..Corymbose"] <- "Corymbose_corals"
names(df)[names(df) == "Acroporidae..Acropora.spp..Digitate"] <- "Digitate_corals"
names(df)[names(df) == "Acroporidae..Acropora.spp..Tabulate"] <- "Tabulate_corals"
names(df)[names(df) == "Free.Living.Coral"] <- "Free_Living_corals"

Branching_Bleached_corals_columns <- c("Acroporidae.Arborescent.Bleached", "Acroporidae..Acropora.spp..Hispidose.Bleached",
                                       "Branching.Other.Bleached", "Pocilloporidae.Bleached")
df$Branching_Bleached_corals <- rowSums(df[, Branching_Bleached_corals_columns])
df <- df[, !(names(df) %in% Branching_Bleached_corals_columns)]

Massive_Encrusting_Bleached_corals_columns <- c("Ceroid.Massive.Encrusting.Bleached..shared.walls.", "Meandroid.Massive.Encrusting.Bleached..meandering.walls.",
                                                "Plocoid.Massive.Encrusting.Bleached..separate.walls.", "Small.Corallite.Massive.Encrusting.Bleached")
df$Massive_Encrusting_Bleached_corals <- rowSums(df[, Massive_Encrusting_Bleached_corals_columns])
df <- df[, !(names(df) %in% Massive_Encrusting_Bleached_corals_columns)]

names(df)[names(df) == "Plate.Foliose.Encrusting.Bleached"] <- "Plate_Foliose_Bleached_corals"
names(df)[names(df) == "Acroporidae..Acropora.spp..Corymbose.Bleached"] <- "Corymbose_Bleached_corals"
names(df)[names(df) == "Acroporidae..Acropora.spp..Digitate.Bleached"] <- "Digitate_Bleached_corals"
names(df)[names(df) == "Acroporidae..Acropora.spp..Tabulate.Bleached"] <- "Tabulate_Bleached_corals"
names(df)[names(df) == "Free.Living.Bleached"] <- "Free_Living_Bleached_corals"

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

## Dates
df$date <- as.Date(df$date)
df$year <- year(df$date)
df$month <- substr(df$date, 6, 7)

species_columns <- c("Live_Hard_coral", "Bleached_Hard_coral", "Other_Substrate", "EAM_Reef_Matrix", "Sand")

assign_zones <- function(data) {
  data %>% mutate(geomorphic_zones = case_when(
    grepl("HR_Junction|HR_PlateLedge|HR_CoralGrotto|HR_Cascades|HR_NWTR7|HR_NWTR6|HR_NETR5|HR_NETR4|HR_BluePools|HR_LibbysLair|HR_Tenements|20221109_HR_BlueHole|HR_GorgonianHole", site) ~ "Leeward Reef Slope North",
    grepl("HR_SETR3|HR_SETR2|HR_SETR1|HR_SETR0", site) ~ "Windward Reef Slope East",
    grepl("HR_HarrysBommie|HR_CoralCanyons|HR_Halfway|HR_CoralGardens|HR_HeronBommie|HR_PamsPoint", site) ~ "Windward Reef Slope West",
    grepl("HR_RFChannel|HR_RFSETR1|HR_RFHalfway|HR_RFSharkBay|HR_RFPamsPoint|HR_RFWest|HR_RFPlateLedge|HR_RFResearchBeach|HR_RFBluePools|HR_RFSETR0", site) ~ "Reef Flat",
    !grepl("HR_", site) ~ "Unknown",
    TRUE ~ "Other"
  ))
}

summarise_by_transect_then_zone <- function(data, value_cols, group_cols) {
  transect_level <- data %>%
    group_by(across(all_of(c("site", group_cols)))) %>%
    summarise(across(all_of(value_cols), ~mean(., na.rm = TRUE)), .groups = "drop")
  
  transect_level %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(across(all_of(value_cols),
                     list(mean = ~mean(., na.rm = TRUE),
                          se   = ~sd(., na.rm = TRUE) / sqrt(n()))),
              n_transects = n(),
              .groups = "drop")
}

build_snapshot <- function(data_filtered, label) {
  data_filtered <- assign_zones(data_filtered)
  summary <- summarise_by_transect_then_zone(data_filtered, species_columns, "geomorphic_zones")
  summary %>%
    select(geomorphic_zones, n_transects, ends_with("_mean"), ends_with("_se")) %>%
    pivot_longer(cols = c(ends_with("_mean"), ends_with("_se")),
                 names_to = c("Species", ".value"),
                 names_pattern = "(.+)_(mean|se)") %>%
    mutate(snapshot = label)
}

coverage_2019 <- build_snapshot(df %>% filter(year == 2019), "A) October 2019 (baseline)")
coverage_2024 <- build_snapshot(df %>% filter(year == 2024 & month == "03"), "B) March 2024 (peak bleaching)")
coverage_2025 <- build_snapshot(df %>% filter(year == 2025 & month == "10"), "C) October 2025 (final survey)")

## Combine, relabel, order
all_coverage_data <- bind_rows(coverage_2019, coverage_2024, coverage_2025)

all_coverage_data <- all_coverage_data %>%
  mutate(Species = recode(Species,
                          "Live_Hard_coral"    = "Non-Bleached Hard Coral",
                          "Bleached_Hard_coral" = "Bleached Hard Coral",
                          "Other_Substrate"     = "Other Substrate",
                          "EAM_Reef_Matrix"     = "Turf on Dead Hard Coral",
                          "Sand"                = "Sand"))

geomorphic_zone_order <- c("Leeward Reef Slope North", "Reef Flat", "Windward Reef Slope East", "Windward Reef Slope West")
species_colors_order  <- c("Other Substrate", "Sand", "Turf on Dead Hard Coral", "Bleached Hard Coral", "Non-Bleached Hard Coral")
species_colors <- c('Non-Bleached Hard Coral' = '#FF7F50', 'Turf on Dead Hard Coral' = '#009E73', 'Other Substrate' = '#56B4E9',
                    'Sand' = '#F0E442', 'Bleached Hard Coral' = '#f2b5a7')
snapshot_order <- c("A) October 2019 (baseline)", "B) March 2024 (peak bleaching)", "C) October 2025 (final survey)")

all_coverage_data$geomorphic_zones <- factor(all_coverage_data$geomorphic_zones, levels = geomorphic_zone_order)
all_coverage_data$Species <- factor(all_coverage_data$Species, levels = species_colors_order)
all_coverage_data$snapshot <- factor(all_coverage_data$snapshot, levels = snapshot_order)

## Renormalize each zone x snapshot's 5 category means to sum to 100%

all_coverage_data <- all_coverage_data %>%
  group_by(snapshot, geomorphic_zones) %>%
  mutate(mean = mean / sum(mean) * 100) %>%
  ungroup()

write.csv(all_coverage_data, "Figure2_data_CORRECTED.csv", row.names = FALSE)

## Plot
ggplot(all_coverage_data, aes(x = geomorphic_zones, y = mean, fill = Species)) +
  geom_bar(stat = "identity", position = "stack", color = "black", width = 0.8) +
  scale_fill_manual(values = species_colors) +
  labs(title = "", x = "", y = "Percent Cover", fill = "Substrate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        strip.background = element_blank(),
        strip.text.x = element_text(face = "bold", hjust = 0),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(1, "mm")) +
  facet_wrap(~ snapshot, ncol = 3)

ggsave("Figure2_composition_by_zone.pdf", width = 12, height = 8, bg = "white")

## Check: transect counts per zone x snapshot
all_coverage_data %>% distinct(snapshot, geomorphic_zones, n_transects) %>% arrange(snapshot, geomorphic_zones)