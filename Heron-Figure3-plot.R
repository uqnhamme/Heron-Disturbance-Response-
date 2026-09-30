## Figure 3 Morphotype comparison for peak bleaching event March 2024

library(tidyverse)

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
                             Transect.gear, Shadow, Fish, unique_id, survey_title, image_name, depth_m, transect, total, project, survey_id, site_id, site_reef_name, site_country_region, site_local_region))

## Combine raw ReefCloud labels into per-morphotype categories
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

df$Branching_coral <- rowSums(df[, c("Corymbose_corals", "Digitate_corals", "Branching_corals")])
df <- df[, !(names(df) %in% c("Corymbose_corals", "Digitate_corals", "Branching_corals"))]

df$Branching_Bleached_coral <- rowSums(df[, c("Corymbose_Bleached_corals", "Digitate_Bleached_corals", "Branching_Bleached_corals")])
df <- df[, !(names(df) %in% c("Corymbose_Bleached_corals", "Digitate_Bleached_corals", "Branching_Bleached_corals"))]

## Dates
df$date <- as.Date(df$date)
df$year <- year(df$date)
df$month <- substr(df$date, 6, 7)

## The 10 morphotype columns used in this figure (5 morphotypes x
## healthy/bleached), and the healthy<->bleached pairing between them
species_columns <- c("Massive_Encrusting_corals", "Free.Living.Coral.Bleached", "Tabulate_corals", "Tabulate_Bleached_corals",
                     "Free_Living_corals", "Branching_Bleached_coral", "Plate_Foliose_Bleached_corals",
                     "Branching_coral", "Plate_Foliose_corals", "Massive_Encrusting_Bleached_corals")

morphotype_pairs <- tribble(
  ~Species,                ~healthy_col,                ~bleached_col,
  "Massive / Encrusting",  "Massive_Encrusting_corals",  "Massive_Encrusting_Bleached_corals",
  "Free Living",           "Free_Living_corals",         "Free.Living.Coral.Bleached",
  "Tabulate",              "Tabulate_corals",             "Tabulate_Bleached_corals",
  "Branching",             "Branching_coral",             "Branching_Bleached_coral",
  "Plate / Foliose",       "Plate_Foliose_corals",        "Plate_Foliose_Bleached_corals"
)

## STEP 1: filter to March 2024, assign zones, and compute each
## photo's RELATIVE abundance (% of total coral cover -- healthy +
## bleached, across all 5 morphotypes -- that this morphotype/status
## represents). This is per-photo, same as before.
coverage_2024_photo <- df %>%
  filter(year == 2024 & month == "03") %>%
  mutate(geomorphic_zones = case_when(
    grepl("HR_Junction|HR_PlateLedge|HR_CoralGrotto|HR_Cascades|HR_NWTR7|HR_NWTR6|HR_NETR5|HR_NETR4|HR_BluePools|HR_LibbysLair|HR_Tenements|20221109_HR_BlueHole|HR_GorgonianHole", site) ~ "Leeward Reef Slope North",
    grepl("HR_SETR3|HR_SETR2|HR_SETR1|HR_SETR0", site) ~ "Windward Reef Slope East",
    grepl("HR_HarrysBommie|HR_CoralCanyons|HR_Halfway|HR_CoralGardens|HR_HeronBommie|HR_PamsPoint", site) ~ "Windward Reef Slope West",
    grepl("HR_RFChannel|HR_RFSETR1|HR_RFHalfway|HR_RFSharkBay|HR_RFPamsPoint|HR_RFWest|HR_RFPlateLedge|HR_RFResearchBeach|HR_RFBluePools|HR_RFSETR0", site) ~ "Reef Flat",
    !grepl("HR_", site) ~ "Unknown",
    TRUE ~ "Other"
  )) %>%
  mutate(total = rowSums(across(all_of(species_columns)), na.rm = TRUE)) %>%
  mutate(across(all_of(species_columns), ~ . / total * 100)) %>%
  select(-total)

## STEP 2: collapse to one row PER TRANSECT first -- mean
## relative abundance across that transect's photos.
coverage_2024_transect <- coverage_2024_photo %>%
  group_by(geomorphic_zones, site) %>%
  summarise(across(all_of(species_columns), ~mean(., na.rm = TRUE)), .groups = "drop")

## STEP 3: zone-level mean and SE
coverage_2024_zone <- coverage_2024_transect %>%
  group_by(geomorphic_zones) %>%
  summarise(across(all_of(species_columns),
                   list(mean = ~mean(., na.rm = TRUE),
                        se   = ~sd(., na.rm = TRUE) / sqrt(n()))),
            n_transects = n(),
            .groups = "drop")

## STEP 4: reshape into the healthy/bleached wide format the plotting
## code expects (geomorphic_zones, Species, healthy, healthy_se,
## bleached, bleached_se)
coverage_2024_mean_rel <- purrr::pmap_dfr(morphotype_pairs, function(Species, healthy_col, bleached_col){
  coverage_2024_zone %>%
    transmute(
      geomorphic_zones,
      Species    = Species,
      healthy    = .data[[paste0(healthy_col, "_mean")]],
      healthy_se = .data[[paste0(healthy_col, "_se")]],
      bleached    = .data[[paste0(bleached_col, "_mean")]],
      bleached_se = .data[[paste0(bleached_col, "_se")]]
    )
})

## STEP 5: square-root transform
delta_sqrt_se <- function(mean_val, se_val) {
  ifelse(mean_val > 0, se_val / (2 * sqrt(mean_val)), NA_real_)
}

coverage_2024_mean_rel_sqrt <- coverage_2024_mean_rel %>%
  mutate(
    healthy_se  = delta_sqrt_se(healthy, healthy_se),
    bleached_se = delta_sqrt_se(bleached, bleached_se),
    healthy     = sqrt(healthy),
    bleached    = sqrt(bleached)
  )

## Check before trusting the plot: n_transects per zone
coverage_2024_zone %>% select(geomorphic_zones, n_transects)

## PLOT: Figure 3 bubble plot, bleached vs. non-bleached
## relative abundance per morphotype, one panel per zone, sqrt-scaled
## with a 1:1 reference line (points above the line = >50% bleached).

library(cowplot)

species_colors <- c('Branching' = '#FF7F50', 'Massive / Encrusting' = '#009E73',
                    'Plate / Foliose' = '#56B4E9', 'Tabulate' = '#F0E442',
                    'Free Living' = '#f2b5a7')

geomorphic_zone_order <- c("Leeward Reef Slope North", "Reef Flat",
                           "Windward Reef Slope East", "Windward Reef Slope West")

make_bubble_panel <- function(zone_name, panel_title, show_axis_labels = FALSE, show_y_label = TRUE) {
  plot_df <- coverage_2024_mean_rel_sqrt %>% filter(geomorphic_zones == zone_name)
  
  ggplot(plot_df, aes(x = healthy, y = bleached, color = Species)) +
    geom_point(size = 10, pch = 1, stroke = 1.5) +
    geom_errorbarh(aes(xmin = pmax(0, healthy - healthy_se), xmax = healthy + healthy_se),
                   height = 0.2, alpha = 0.6) +
    geom_errorbar(aes(ymin = pmax(0, bleached - bleached_se), ymax = bleached + bleached_se),
                  width = 0.2, alpha = 0.6) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 0.5) +
    scale_color_manual(values = species_colors, name = "Morphotype") +
    labs(title = panel_title,
         x = if (show_axis_labels) "Non-Bleached Relative Abundance (\u00b1 SE)" else "",
         y = if (show_y_label) "Bleached Relative Abundance (\u00b1 SE)" else "") +
    coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, size = 1),
          axis.ticks = element_line(color = "black"),
          axis.ticks.length = unit(1, "mm"),
          plot.title = element_text(face = "bold"))
}



p1 <- make_bubble_panel("Leeward Reef Slope North", "A) Leeward Reef Slope North")
p2 <- make_bubble_panel("Reef Flat", "B) Reef Flat", show_y_label = FALSE)
p3 <- make_bubble_panel("Windward Reef Slope East", "C) Windward Reef Slope East", show_axis_labels = TRUE)
p4 <- make_bubble_panel("Windward Reef Slope West", "D) Windward Reef Slope West", show_axis_labels = TRUE, show_y_label = FALSE)

# Shared legend, pulled from one panel with the legend temporarily enabled
legend_source <- p1 + theme(legend.position = "right")
shared_legend <- get_legend(legend_source)

figure3_grid <- plot_grid(
  plot_grid(p1, p2, p3, p4, ncol = 2, align = "hv"),
  shared_legend,
  rel_widths = c(1, 0.25),
  nrow = 1
)

print(figure3_grid)

ggsave("Figure3_bleaching_by_morphotype.pdf", plot = figure3_grid,
       width = 12, height = 10, bg = "white")

## Export the underlying corrected data too, for the supplement / for
## anyone checking the figure against the numbers directly
write.csv(coverage_2024_mean_rel_sqrt, "Figure3_data.csv", row.names = FALSE)
write.csv(coverage_2024_mean_rel, "Figure3_data_untransformed.csv", row.names = FALSE)