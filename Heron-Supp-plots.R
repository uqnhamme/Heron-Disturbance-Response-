## Figures S2-S5

library(tidyverse)

## Transect-level aggregation -> zone-level mean/SE.
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

## Capture the raw rubble label BEFORE it gets combined into
## EAM_Reef_Matrix below (needed for Figure S5)
df$rubble_pct_raw <- df$Epithelial.Algal.Matrix.on.Rubble

## Combine raw ReefCloud labels into the 5 morphotype categories,
## KEPT SEPARATE (needed for S3/S4), same convention as the Figure 3
## rebuild: singular "Branching_coral" from Corymbose+Digitate+Branching.
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

df$Branching_coral <- rowSums(df[, c("Corymbose_corals", "Digitate_corals", "Branching_corals")])
df <- df[, !(names(df) %in% c("Corymbose_corals", "Digitate_corals", "Branching_corals"))]

morphotype_cols <- c("Massive_Encrusting_corals", "Free_Living_corals", "Branching_coral",
                     "Plate_Foliose_corals", "Tabulate_corals")

## Live_Hard_coral (for S2) = sum of the 5 morphotypes, WITHOUT
## removing the individual morphotype columns (S3/S4 still need them)
df$Live_Hard_coral <- rowSums(df[, morphotype_cols])

## Dates, zones, survey_event
df$date <- as.Date(df$date)
df$year <- year(df$date)
df$month <- as.numeric(substr(df$date, 6, 7))

df <- df |> mutate(geomorphic_zones = case_when(
  grepl("HR_Junction|HR_PlateLedge|HR_CoralGrotto|HR_Cascades|HR_NWTR7|HR_NWTR6|HR_NETR5|HR_NETR4|HR_BluePools|HR_LibbysLair|HR_Tenements|20221109_HR_BlueHole|HR_GorgonianHole", site) ~ "Leeward Reef Slope North",
  grepl("HR_SETR3|HR_SETR2|HR_SETR1|HR_SETR0", site) ~ "Windward Reef Slope East",
  grepl("HR_HarrysBommie|HR_CoralCanyons|HR_Halfway|HR_CoralGardens|HR_HeronBommie|HR_PamsPoint", site) ~ "Windward Reef Slope West",
  grepl("HR_RFChannel|HR_RFSETR1|HR_RFHalfway|HR_RFSharkBay|HR_RFPamsPoint|HR_RFWest|HR_RFPlateLedge|HR_RFResearchBeach|HR_RFBluePools|HR_RFSETR0", site) ~ "Reef Flat",
  TRUE ~ "Other"))

df <- df %>% filter(year >= 2019 & year <= 2025)

## Exclude the same intermediate surveys the main GLMM/nMDS analyses
## exclude (May 2024, August 2024, March 2025), so S2-S5 use the same
## 9 curated survey events as the rest of the paper rather than the
## full set of raw survey dates.
df <- df[!((df$year == 2024 & df$month %in% c(5, 8)) | (df$year == 2025 & df$month == 3)), ]

## Standardize survey month labels
df$month[(df$month %in% c(10,11)) & (df$year %in% c(2019,2020,2023))] <- 10
df$idyear <- (df$year - 2018) + df$month / 12

month_abbrev <- c("01"="Jan","02"="Feb","03"="Mar","04"="Apr","05"="May","06"="Jun",
                  "07"="Jul","08"="Aug","09"="Sep","10"="Oct","11"="Nov","12"="Dec")
df$survey_event <- paste0(month_abbrev[sprintf("%02d", df$month)], " ", df$year)

survey_order <- df %>%
  distinct(survey_event, idyear) %>%
  arrange(idyear) %>%
  pull(survey_event)
df$survey_event <- factor(df$survey_event, levels = survey_order)

levels(df$survey_event)   # confirm this looks right before proceeding

## FIGURE S2: Percent coral cover by zone, over time
## ASSUMPTION: this is the RAW OBSERVED (not model-fitted) hard-coral
## percentage per photo i.e. Live_Hard_coral as a fraction of ALL
## substrate (not restricted to hard substrate only), since the axis
## says "Percent Coral Cover" generically rather than the
## hard-substrate-conditional language used for the GLMM.

df$pct_coral_cover_all_substrate <- df$Live_Hard_coral   # % of ALL points, not just hard substrate

s2_data <- summarise_by_transect_then_zone(
  df, value_cols = "pct_coral_cover_all_substrate",
  group_cols = c("geomorphic_zones", "survey_event")
)

ggplot(s2_data, aes(x = survey_event, y = pct_coral_cover_all_substrate_mean, group = 1)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = pct_coral_cover_all_substrate_mean - pct_coral_cover_all_substrate_se,
                    ymax = pct_coral_cover_all_substrate_mean + pct_coral_cover_all_substrate_se),
                width = 0.2) +
  facet_wrap(~ geomorphic_zones, scales = "free_x") +
  labs(x = "Survey Event", y = "Percent Coral Cover (mean ± SE, transect-level)") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.ticks = element_line(color = 'black'),
        axis.ticks.length = unit(1, 'mm'),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("FigureS2_percent_coral_cover.pdf", width = 10, height = 8, bg = "white")
write.csv(s2_data, "FigureS2_data.csv", row.names = FALSE)

## FIGURE S3 and S4: Relative abundance (S3) and absolute percent
## cover (S4) of the 5 morphotypes, by zone, over the full survey
## time series

## S4 first: absolute percent cover, straight from df (each
## morphotype's raw % of ALL points in that photo), transect-level SE

s4_summary <- summarise_by_transect_then_zone(
  df, value_cols = morphotype_cols,
  group_cols = c("geomorphic_zones", "survey_event")
) %>%
  select(geomorphic_zones, survey_event, n_transects, ends_with("_mean"), ends_with("_se")) %>%
  pivot_longer(cols = c(ends_with("_mean"), ends_with("_se")),
               names_to = c("morphotype", ".value"),
               names_pattern = "(.+)_(mean|se)")

## Same clean display labels as S3
## first in the script -- reused for S3 below
morphotype_display_names <- c(
  "Massive_Encrusting_corals" = "Massive / Encrusting",
  "Free_Living_corals"        = "Free Living",
  "Branching_coral"           = "Branching",
  "Plate_Foliose_corals"      = "Plate / Foliose",
  "Tabulate_corals"           = "Tabulate"
)
s4_summary$morphotype <- morphotype_display_names[s4_summary$morphotype]

ggplot(s4_summary, aes(x = survey_event, y = mean, fill = morphotype)) +
  geom_col(position = "stack") +
  facet_wrap(~ geomorphic_zones, scales = "free_x") +
  labs(x = "Survey Event", y = "Percent Cover", fill = "Morphotype") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.ticks = element_line(color = 'black'),
        axis.ticks.length = unit(1, 'mm'),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("FigureS4_absolute_percent_cover.pdf", width = 10, height = 8, bg = "white")
write.csv(s4_summary, "FigureS4_data.csv", row.names = FALSE)

## S3: relative abundance, each morphotype

df_relativized <- df
morphotype_total <- rowSums(df[, morphotype_cols])
df_relativized[, morphotype_cols] <- df[, morphotype_cols] / morphotype_total * 100

s3_summary <- summarise_by_transect_then_zone(
  df_relativized, value_cols = morphotype_cols,
  group_cols = c("geomorphic_zones", "survey_event")
) %>%
  select(geomorphic_zones, survey_event, n_transects, ends_with("_mean"), ends_with("_se")) %>%
  pivot_longer(cols = c(ends_with("_mean"), ends_with("_se")),
               names_to = c("morphotype", ".value"),
               names_pattern = "(.+)_(mean|se)")

## Reuses morphotype_display_names defined above (in the S4 section)
s3_summary$morphotype <- morphotype_display_names[s3_summary$morphotype]

ggplot(s3_summary, aes(x = survey_event, y = mean, fill = morphotype)) +
  geom_col(position = "stack") +
  facet_wrap(~ geomorphic_zones, scales = "free_x") +
  labs(x = "Survey Event", y = "Relative Abundance (%)", fill = "Morphotype") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.ticks = element_line(color = 'black'),
        axis.ticks.length = unit(1, 'mm'),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("FigureS3_relative_abundance.pdf", width = 10, height = 8, bg = "white")
write.csv(s3_summary, "FigureS3_data.csv", row.names = FALSE)

## Note: S3/S4 error bars omitted from the stacked-bar plots above
## (stacked bars with per-segment error bars get visually unreadable)

## FIGURE S5: Percent rubble cover by zone, over time
## ASSUMPTION: "rubble cover" = the "epilithic algal matrix on rubble"
## label specifically (the sub-component of turf-on-dead-coral used
## as the cyclone/structural-collapse indicator)

s5_data <- summarise_by_transect_then_zone(
  df, value_cols = "rubble_pct_raw",
  group_cols = c("geomorphic_zones", "survey_event")
)

ggplot(s5_data, aes(x = survey_event, y = rubble_pct_raw_mean, group = 1)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = rubble_pct_raw_mean - rubble_pct_raw_se,
                    ymax = rubble_pct_raw_mean + rubble_pct_raw_se),
                width = 0.2) +
  facet_wrap(~ geomorphic_zones, scales = "free_x") +
  labs(x = "Survey Event", y = "Percent Rubble Cover (mean ± SE, transect-level)") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.ticks = element_line(color = 'black'),
        axis.ticks.length = unit(1, 'mm'),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("FigureS5_percent_rubble_cover.pdf", width = 10, height = 8, bg = "white")
write.csv(s5_data, "FigureS5_data.csv", row.names = FALSE)