## Figure 5: nMDS

library(tidyverse)
library(vegan)
library(cowplot)
library(ggrepel)
library(patchwork)

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

df$date <- as.Date(df$date)
df$year <- year(df$date)
df$month <- substr(df$date, 6, 7)

## Combine columns for NMDS (5 morphotypes, non-bleached only)
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

species_columns <- c("Massive_Encrusting_corals", "Free_Living_corals", "Branching_coral", "Plate_Foliose_corals", "Tabulate_corals")

## Assign zones, aggregate to transect level (mean per site per
## survey)
coverage_by_year_geomorphic_zone <- df %>%
  filter(year >= 2019 & year <= 2025) %>%
  mutate(geomorphic_zones = case_when(
    grepl("HR_Junction|HR_PlateLedge|HR_CoralGrotto|HR_Cascades|HR_NWTR7|HR_NWTR6|HR_NETR5|HR_NETR4|HR_BluePools|HR_LibbysLair|HR_Tenements|HR_GorgonianHole", site) ~ "Leeward Reef Slope North",
    grepl("HR_SETR3|HR_SETR2|HR_SETR1|HR_SETR0", site) ~ "Windward Reef Slope East",
    grepl("HR_HarrysBommie|HR_CoralCanyons|HR_Halfway|HR_CoralGardens|HR_HeronBommie|HR_PamsPoint", site) ~ "Windward Reef Slope West",
    grepl("HR_RFChannel|HR_RFSETR1|HR_RFHalfway|HR_RFSharkBay|HR_RFPamsPoint|HR_RFWest|HR_RFPlateLedge|HR_RFResearchBeach|HR_RFBluePools|HR_RFSETR0", site) ~ "Reef Flat",
    !grepl("HR_", site) ~ "Unknown",
    TRUE ~ "Other"
  )) %>%
  group_by(year, month, geomorphic_zones, site) %>%
  summarise(across(all_of(species_columns), ~mean(., na.rm = TRUE)), .groups = 'drop')

species_data <- coverage_by_year_geomorphic_zone
species_data$geomorphic_zones <- factor(species_data$geomorphic_zones)

## Relativize (proportion of the 5 morphotypes, per transect-survey)
cols_to_relativize <- which(names(species_data) %in% species_columns)
compData <- as.matrix(species_data[, cols_to_relativize])
compDataRel <- prop.table(compData, margin = 1)
species_data_relativized <- species_data
species_data_relativized[, cols_to_relativize] <- as.data.frame(compDataRel)

## Standardize month labels the SAME way as the GLMM
## (idyear_f) rather than collapsing multiple events into one year
species_data_relativized$month <- as.numeric(species_data_relativized$month)
species_data_relativized$month[(species_data_relativized$month %in% c(10,11)) &
                                 (species_data_relativized$year %in% c(2019,2020,2023))] <- 10

species_data_relativized <- species_data_relativized[!((species_data_relativized$year == 2024 & species_data_relativized$month %in% c(05,08)) |
                                                         (species_data_relativized$year == 2025 & species_data_relativized$month == 03)), ]

species_data_relativized$idyear <- (species_data_relativized$year - 2018) + species_data_relativized$month / 12

month_abbrev <- c("01"="Jan","02"="Feb","03"="Mar","04"="Apr","05"="May","06"="Jun",
                  "07"="Jul","08"="Aug","09"="Sep","10"="Oct","11"="Nov","12"="Dec")
species_data_relativized$survey_event <- paste0(
  month_abbrev[sprintf("%02d", species_data_relativized$month)], " ", species_data_relativized$year
)

# Order survey_event chronologically (by idyear), not alphabetically
survey_order <- species_data_relativized %>%
  distinct(survey_event, idyear) %>%
  arrange(idyear) %>%
  pull(survey_event)
species_data_relativized$survey_event <- factor(species_data_relativized$survey_event, levels = survey_order)

survey_order   # print to confirm the 9 distinct events, in order

## Ordination (Bray-Curtis on sqrt-transformed
## relative abundance, k=2)
bc_dist <- vegdist(sqrt(species_data_relativized[, species_columns]), method = "bray")
nmds <- metaMDS(bc_dist, k = 2, trymax = 100)
stress_value <- nmds$stress

nmds_data <- data.frame(
  NMDS1 = nmds$points[, 1], NMDS2 = nmds$points[, 2],
  geomorphic_zones = species_data_relativized$geomorphic_zones,
  survey_event = species_data_relativized$survey_event,
  site = species_data_relativized$site
)

species_fit <- envfit(nmds, species_data_relativized[, species_columns], permutations = 999)
species_scores <- as.data.frame(species_fit$vectors$arrows)
rownames(species_scores) <- c("Massive / Encrusting", "Free Living", "Branching", "Plate / Foliose", "Tabulate")

## Colors for 9 distinct survey events (was 7 for years)
n_events <- length(survey_order)
event_colors <- setNames(colorRampPalette(c('#b35806', '#FF7F50', '#fee0b6', '#d8daeb',
                                            '#009E73', '#56B4E9', '#542788'))(n_events),
                         survey_order)

## Plot: one panel per zone
make_nmds_panel <- function(zone_name, panel_title, subtitle = NULL, xlab = "", ylab = "nMDS2") {
  zone_data <- nmds_data %>% filter(geomorphic_zones == zone_name)
  event_means <- aggregate(cbind(NMDS1, NMDS2) ~ survey_event, data = zone_data, FUN = mean)
  event_means <- event_means[order(match(event_means$survey_event, survey_order)), ]
  
  ggplot(zone_data, aes(x = NMDS1, y = NMDS2, color = survey_event)) +
    geom_point(size = 2, alpha = 0.5, shape = 16) +
    labs(x = xlab, y = ylab, title = panel_title, subtitle = subtitle) +
    scale_color_manual(values = event_colors, name = "Survey Event") +
    scale_fill_manual(values = event_colors, guide = "none") +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 1.0),
          axis.ticks = element_line(color = "black"),
          axis.ticks.length = unit(1, "mm"),
          panel.background = element_blank(),
          strip.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 10, face = "bold"),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 10)) +
    coord_fixed(ratio = 1.0, xlim = c(-0.5, 0.5), ylim = c(-0.5, 0.5)) +
    geom_segment(data = species_scores, aes(x = 0, y = 0, xend = NMDS1 * 0.4, yend = NMDS2 * 0.4),
                 arrow = arrow(length = unit(0.3, "cm")), color = "black", linewidth = 0.1, inherit.aes = FALSE) +
    geom_text_repel(data = species_scores, aes(x = NMDS1 * 0.4, y = NMDS2 * 0.4, label = rownames(species_scores)),
                    size = 3.5, color = "black", min.segment.length = 0.5, max.overlaps = 20, inherit.aes = FALSE) +
    geom_path(data = event_means, aes(x = NMDS1, y = NMDS2), color = "black", linewidth = 0.3, inherit.aes = FALSE) +
    geom_point(data = event_means, aes(x = NMDS1, y = NMDS2, fill = survey_event), shape = 21, color = "black", stroke = 0.8, size = 3, inherit.aes = FALSE)
}

## Figure 5: (A) overall ordination, all zones pooled
## into one NMDS + (B-E) separate, independently-run NMDS
## ordinations within each zone.

zone_colors <- c('Leeward Reef Slope North' = '#FF7F50', 'Windward Reef Slope East' = '#009E73',
                 'Windward Reef Slope West' = '#56B4E9', 'Reef Flat' = '#F0E442')

## Panel A: overall ordination, all zones pooled (reuses nmds,
## species_scores, stress_value already computed above)
panel_A <- ggplot(nmds_data, aes(x = NMDS1, y = NMDS2, color = geomorphic_zones)) +
  geom_point(size = 2, alpha = 0.5, shape = 16) +
  stat_ellipse(level = 0.95, linewidth = 0.5) +
  labs(x = "nMDS1", y = "nMDS2", title = "A) Overall",
       subtitle = paste("Stress:", round(stress_value, 3))) +
  scale_color_manual(values = zone_colors, name = "Geomorphic Zone") +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1.0),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(1, "mm"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10)) +
  coord_fixed(ratio = 1.0, xlim = c(-0.5, 0.5), ylim = c(-0.5, 0.5)) +
  geom_segment(data = species_scores, aes(x = 0, y = 0, xend = NMDS1 * 0.4, yend = NMDS2 * 0.4),
               arrow = arrow(length = unit(0.3, "cm")), color = "black", linewidth = 0.1, inherit.aes = FALSE) +
  geom_text_repel(data = species_scores, aes(x = NMDS1 * 0.4, y = NMDS2 * 0.4, label = rownames(species_scores)),
                  size = 3.5, color = "black", min.segment.length = 0.5, max.overlaps = 20, inherit.aes = FALSE)

## Panels B-E: independent NMDS ordination fit WITHIN each zone
zone_names_ordered <- c("Leeward Reef Slope North", "Reef Flat",
                        "Windward Reef Slope East", "Windward Reef Slope West")

run_zone_nmds <- function(zone_name) {
  idx <- which(species_data_relativized$geomorphic_zones == zone_name)
  zone_sp <- species_data_relativized[idx, ]
  zone_bc <- vegdist(sqrt(zone_sp[, species_columns]), method = "bray")
  zone_nmds <- metaMDS(zone_bc, k = 2, trymax = 100)
  list(nmds = zone_nmds, data = zone_sp, dist = zone_bc, stress = zone_nmds$stress)
}

zone_nmds_results <- lapply(zone_names_ordered, run_zone_nmds)
names(zone_nmds_results) <- zone_names_ordered


sapply(zone_nmds_results, function(x) x$stress)

## Shared axis limits across B-E, computed after all 4 zone
## ordinations are fit, before any panels are plotted
all_zone_points <- do.call(rbind, lapply(zone_nmds_results, function(res) {
  as.data.frame(res$nmds$points)
}))

axis_pad <- 0.05  # small padding so points aren't flush against the panel border
shared_xlim <- range(all_zone_points$MDS1) + c(-axis_pad, axis_pad)
shared_ylim <- range(all_zone_points$MDS2) + c(-axis_pad, axis_pad)

make_zone_panel <- function(zone_name, panel_title, xlab = "", ylab = "",
                            xlim = shared_xlim, ylim = shared_ylim) {
  res        <- zone_nmds_results[[zone_name]]
  zone_nmds  <- res$nmds
  zone_data  <- res$data
  
  zone_scores <- data.frame(
    NMDS1 = zone_nmds$points[, 1], NMDS2 = zone_nmds$points[, 2],
    survey_event = zone_data$survey_event
  )
  
  zone_species_fit <- envfit(zone_nmds, zone_data[, species_columns], permutations = 999)
  zone_species_scores <- as.data.frame(zone_species_fit$vectors$arrows)
  rownames(zone_species_scores) <- c("Massive / Encrusting", "Free Living", "Branching", "Plate / Foliose", "Tabulate")
  
  event_means <- aggregate(cbind(NMDS1, NMDS2) ~ survey_event, data = zone_scores, FUN = mean)
  event_means <- event_means[order(match(event_means$survey_event, survey_order)), ]
  
  ggplot(zone_scores, aes(x = NMDS1, y = NMDS2, color = survey_event)) +
    geom_point(size = 2, alpha = 0.5, shape = 16) +
    labs(x = xlab, y = ylab, title = panel_title,
         subtitle = paste("Stress:", round(zone_nmds$stress, 3))) +
    scale_color_manual(values = event_colors, name = "Survey Event") +
    scale_fill_manual(values = event_colors, guide = "none") +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 1.0),
          axis.ticks = element_line(color = "black"),
          axis.ticks.length = unit(1, "mm"),
          panel.background = element_blank(),
          strip.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 10, face = "bold"),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 10)) +
    coord_fixed(ratio = 1.0, xlim = xlim, ylim = ylim) +
    geom_segment(data = zone_species_scores, aes(x = 0, y = 0, xend = NMDS1 * 0.4, yend = NMDS2 * 0.4),
                 arrow = arrow(length = unit(0.3, "cm")), color = "black", linewidth = 0.1, inherit.aes = FALSE) +
    geom_text_repel(data = zone_species_scores, aes(x = NMDS1 * 0.4, y = NMDS2 * 0.4, label = rownames(zone_species_scores)),
                    size = 3.5, color = "black", min.segment.length = 0.5, max.overlaps = 20, inherit.aes = FALSE) +
    geom_path(data = event_means, aes(x = NMDS1, y = NMDS2), color = "black", linewidth = 0.3, inherit.aes = FALSE) +
    geom_point(data = event_means, aes(x = NMDS1, y = NMDS2, fill = survey_event), shape = 21, color = "black", stroke = 0.8, size = 3, inherit.aes = FALSE)
}

panel_B <- make_zone_panel("Leeward Reef Slope North", "B) Leeward Reef Slope North", xlab = "")
panel_C <- make_zone_panel("Reef Flat", "C) Reef Flat", xlab = "", ylab = "")
panel_D <- make_zone_panel("Windward Reef Slope East", "D) Windward Reef Slope East", xlab = "nMDS1")
panel_E <- make_zone_panel("Windward Reef Slope West", "E) Windward Reef Slope West", xlab = "nMDS1", ylab = "")

## Compose: A on the left, B-E in a 2x2 grid on the right
right_grid <- (panel_B | panel_C) / (panel_D | panel_E) +
  plot_layout(guides = "collect") & theme(legend.position = "right")

figure5_final <- panel_A | right_grid

ggsave("Figure5_nMDS_overall_and_byzone.pdf", plot = figure5_final, width = 16, height = 8, bg = "white")


## PERMANOVA + dispersion, re-attached to the event-level-
## Run this after Heron-revised.R (needs species_data_relativized,
## bc_dist, and survey_event already built).

library(vegan)
source("https://raw.githubusercontent.com/pmartinezarbizu/pairwiseAdonis/master/pairwiseAdonis/R/pairwise.adonis.R")
# or: devtools::install_github("pmartinezarbizu/pairwiseAdonis"); library(pairwiseAdonis)

species_data_relativized$geomorphic_zones <- factor(species_data_relativized$geomorphic_zones)
bc_mat <- as.matrix(bc_dist)
zone_names <- levels(species_data_relativized$geomorphic_zones)

## PART 1: Dispersion (homoscedasticity) checks for the whole model
bd_zone <- betadisper(bc_dist, group = species_data_relativized$geomorphic_zones)
disp_test_zone <- permutest(bd_zone, permutations = 999, pairwise = TRUE)
print(disp_test_zone)

bd_event <- betadisper(bc_dist, group = species_data_relativized$survey_event)
disp_test_event <- permutest(bd_event, permutations = 999, pairwise = TRUE)
print(disp_test_event)

bd_int <- betadisper(bc_dist, group = interaction(species_data_relativized$geomorphic_zones,
                                                  species_data_relativized$survey_event))
disp_test_int <- permutest(bd_int, permutations = 999, pairwise = TRUE)
print(disp_test_int)

write.csv(as.data.frame(disp_test_zone$tab), "dispersion_test_zone_FINAL.csv")
write.csv(as.data.frame(disp_test_event$tab), "dispersion_test_event_FINAL.csv")
write.csv(as.data.frame(disp_test_int$tab), "dispersion_test_zone_by_event_FINAL.csv")

## PART 2: Dispersion checks WITHIN each zone, across events
zone_dispersion_results <- lapply(zone_names, function(zn){
  idx <- which(species_data_relativized$geomorphic_zones == zn)
  zone_dist <- as.dist(bc_mat[idx, idx])
  zone_event <- factor(species_data_relativized$survey_event[idx])
  
  bd <- betadisper(zone_dist, group = zone_event)
  permutest(bd, permutations = 999, pairwise = TRUE)
})
names(zone_dispersion_results) <- zone_names

for (zn in zone_names) {
  cat("\n====", zn, "====\n")
  print(zone_dispersion_results[[zn]])
  write.csv(as.data.frame(zone_dispersion_results[[zn]]$tab),
            paste0("dispersion_test_within_FINAL_", gsub(" ", "_", zn), ".csv"))
}

## PART 3: Restricted pairwise PERMANOVA, the three
## disturbance-relevant transitions, defined by EXACT EVENT
## (Oct 2019 / Mar 2024 / Oct 2025)
target_events <- c("Oct 2019", "Mar 2024", "Oct 2025")

zone_time_pairwise_targeted <- lapply(zone_names, function(zn){
  idx <- which(species_data_relativized$geomorphic_zones == zn &
                 species_data_relativized$survey_event %in% target_events)
  zone_dist <- as.dist(bc_mat[idx, idx])
  zone_event <- factor(species_data_relativized$survey_event[idx], levels = target_events)
  
  pairwise <- pairwise.adonis(zone_dist, zone_event, p.adjust.m = "bonferroni")
  list(zone = zn, pairwise = pairwise)
})
names(zone_time_pairwise_targeted) <- zone_names

for (zn in zone_names) {
  cat("\n====", zn, ": targeted pairwise (Oct 2019 / Mar 2024 / Oct 2025) ====\n")
  print(zone_time_pairwise_targeted[[zn]]$pairwise)
  write.csv(zone_time_pairwise_targeted[[zn]]$pairwise,
            paste0("permanova_pairwise_targeted_FINAL_", gsub(" ", "_", zn), ".csv"),
            row.names = FALSE)
}

## PART 4: Combined summary, Bonferroni + Holm side by side
targeted_summary <- do.call(rbind, lapply(zone_names, function(zn){
  df <- zone_time_pairwise_targeted[[zn]]$pairwise
  df$zone <- zn
  df
}))
targeted_summary <- targeted_summary[, c("zone", "pairs", "F.Model", "R2", "p.value", "p.adjusted", "sig")]

targeted_summary$p.adjusted_holm <- ave(targeted_summary$p.value, targeted_summary$zone,
                                        FUN = function(p) p.adjust(p, method = "holm"))

targeted_summary
write.csv(targeted_summary, "permanova_pairwise_targeted_summary_FINAL.csv", row.names = FALSE)


## Check: confirm no zone/event combination was silently
## dropped (e.g. if a zone had zero transects at one of the three
## target events, that comparison can't be computed)
table(species_data_relativized$survey_event[species_data_relativized$survey_event %in% target_events],
      species_data_relativized$geomorphic_zones[species_data_relativized$survey_event %in% target_events])




