## Figure 4: GLMM

library(tidyverse)
library(lme4)
library(glmmTMB)
library(performance)
library(DHARMa)

## Load and clean
setwd("/Users/nick/Desktop/Heron_project/") ### set to your own working directory 
df <- read.csv('reefcloud-point-summary-MEML_HeronReef_Coral-2026-01-14-filtered.csv')

dfNonInt <- rowSums(df[,18:ncol(df)] %% 1 > 0)
df <- df[dfNonInt == 0, ]

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
                             Transect.gear, Shadow, Fish, survey_title, image_name, depth_m, transect, total, project, survey_id, site_id, site_reef_name, site_country_region, site_local_region))


id_columns <- c("unique_id", "date..UTC.", "year", "site", "site_latitude", "site_longitude")
df$row_total <- rowSums(df[, !(names(df) %in% id_columns)])
df <- subset(df, row_total > 80)

site_year_totals <- df %>% group_by(site, year) %>% summarise(total_rows = n(), .groups = "drop")
df <- df[df$site %in% site_year_totals$site[site_year_totals$total_rows > 100], ]

## Combine raw ReefCloud labels into life-status / morphotype
algae_columns <- c("Macroalgae.Other", "Benthic.Microalgae.on.Sand",
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

Other_Hard_Substrate_columns <- c("Soft_Corals", "Other_Invertebrates", "Algae_Cyanobacteria", "Crustose.Coralline.Algae")
df$Other_Hard_Substrate <- rowSums(df[, Other_Hard_Substrate_columns])
df <- df[, !(names(df) %in% Other_Hard_Substrate_columns)]

## Dates, zones, month standardization
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

df$month[(df$month %in% c(10,11)) & (df$year %in% c(2019,2020,2023))] <- 10
df <- df[!((df$year == 2024 & df$month %in% c(05,08)) | (df$year == 2025 & df$month == 03)), ]
df$idyear <- (df$year - 2018) + df$month / 12
df$idyear_f <- factor(df$idyear)
df$site <- factor(df$site)
df$geomorphic_zones <- factor(df$geomorphic_zones)
df$unique_id <- factor(df$unique_id)

df$total_points <- round(df$row_total / 2)

species_columns <- c("Live_Hard_coral", "EAM_Reef_Matrix", "Bleached_Hard_coral")

hard_substrate_pct <- rowSums(df[, species_columns])
df$hard_substrate_points <- round(hard_substrate_pct / 100 * df$total_points)
df <- df[df$hard_substrate_points > 0, ]

df$Live_Hard_coral_pts     <- round(df$Live_Hard_coral     / 100 * df$total_points)
df$EAM_Reef_Matrix_pts     <- round(df$EAM_Reef_Matrix     / 100 * df$total_points)
df$Bleached_Hard_coral_pts <- round(df$Bleached_Hard_coral / 100 * df$total_points)

modelDF <- df %>%
  group_by(site, geomorphic_zones, year, month, idyear, idyear_f) %>%
  summarise(
    Live_Hard_coral_pts     = sum(Live_Hard_coral_pts),
    EAM_Reef_Matrix_pts     = sum(EAM_Reef_Matrix_pts),
    Bleached_Hard_coral_pts = sum(Bleached_Hard_coral_pts),
    hard_substrate_points   = sum(hard_substrate_points),
    n_photos                = n(),
    .groups = "drop"
  )

species_columns_pts <- c("Live_Hard_coral_pts", "EAM_Reef_Matrix_pts", "Bleached_Hard_coral_pts")

## Fit BINOMIAL models (glmmTMB), one per life-status group. All three
## use the same random-effects structure, random slope + intercept
## for the continuous survey-time variable, (idyear | site), so the
## three models are directly comparable in specification. A formal
## overdispersion test (see below) found no evidence of overdispersion
## under this binomial specification (dispersion ratios 0.92-1.01, all
## p > 0.7), so the binomial family is retained as the final/reported
## model family throughout.
gam_models_binom <- lapply(species_columns_pts, function(species){
  
  modelDF$success <- modelDF[[species]]
  modelDF$failure <- modelDF$hard_substrate_points - modelDF$success
  modelDF$failure[modelDF$failure < 0] <- 0
  
  glmmTMB(cbind(success, failure) ~ geomorphic_zones * idyear_f + (idyear | site),
          data   = modelDF,
          family = binomial(link = "logit"))
})
names(gam_models_binom) <- species_columns_pts

## Diagnostics (DHARMa simulation-based dispersion test)
lapply(gam_models_binom, summary)
set.seed(2026)
lapply(gam_models_binom, function(m) testDispersion(simulateResiduals(m, n = 1000, plot = FALSE), plot = FALSE))
lapply(gam_models_binom, function(m) glmmTMB::sigma(m))   # dispersion parameter
lapply(gam_models_binom, performance::check_singularity)

levels(modelDF$idyear_f)

## Formal overdispersion test: fit each life-status model as a plain
## BINOMIAL GLMM first, on the SAME corrected trial counts and SAME
## random-effects structure as the final models, then test for
## overdispersion via DHARMa's simulation-based testDispersion()
## (Hartig 2024). DHARMa simulates new response data from the
## fitted model, computes scaled (quantile) residuals, and compares
## the variance of the simulated data to the observed data directly

library(lme4)
library(DHARMa)

fit_binomial_randomslope <- function(species, data) {
  data$success <- data[[species]]
  data$failure <- data$hard_substrate_points - data$success
  data$failure[data$failure < 0] <- 0
  glmer(cbind(success, failure) ~ geomorphic_zones * idyear_f + (1 + idyear | site),
        data    = data,
        family  = binomial(link = "logit"),
        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
}

gam_models_binomial_check <- list(
  Live_Hard_coral_pts     = fit_binomial_randomslope("Live_Hard_coral_pts", modelDF),
  EAM_Reef_Matrix_pts     = fit_binomial_randomslope("EAM_Reef_Matrix_pts", modelDF),
  Bleached_Hard_coral_pts = fit_binomial_randomslope("Bleached_Hard_coral_pts", modelDF)
)

## Formal overdispersion test via DHARMa::testDispersion(), applied to
## 1000 simulations per model. set.seed() makes the simulation (and
## therefore the reported ratio/p-value) reproducible run to run.
set.seed(2026)

dharma_sims <- lapply(gam_models_binomial_check, function(m) {
  simulateResiduals(fittedModel = m, n = 1000, plot = FALSE)
})

overdispersion_results <- lapply(dharma_sims, testDispersion, plot = FALSE)
overdispersion_results

## Extract a clean summary table
species_output_names <- c(
  Live_Hard_coral_pts     = "Non-Bleached Hard Coral",
  EAM_Reef_Matrix_pts     = "Turf on Dead Hard Coral",
  Bleached_Hard_coral_pts = "Bleached Hard Coral"
)

overdispersion_table <- do.call(rbind, lapply(names(overdispersion_results), function(species){
  res <- overdispersion_results[[species]]
  data.frame(
    life_status       = species_output_names[[species]],
    dispersion_ratio  = as.numeric(res$statistic),
    p_value           = as.numeric(res$p.value)
  )
}))

overdispersion_table
write.csv(overdispersion_table, "binomial_overdispersion_test_FINAL.csv", row.names = FALSE)


## emmeans contrasts, built on the corrected binomial models
## (family = binomial(), see above).

library(emmeans)

## Assemble the final model set: all three life-status models share
## the same random-slope + random-intercept structure, (idyear | site)
gam_models_final <- gam_models_binom

## Baseline level
levels(modelDF$idyear_f)
baseline_level <- levels(modelDF$idyear_f)[1]
baseline_index <- match(baseline_level, levels(modelDF$idyear_f))

idyear_lookup <- unique(modelDF[, c("idyear", "year", "month")])
idyear_lookup$survey_label <- paste0(idyear_lookup$month, "/", idyear_lookup$year)
idyear_lookup

## Each zone vs. its own Oct 2019 baseline, on the probability scale
baseline_contrasts_prob_final <- lapply(names(gam_models_final), function(species){
  mod <- gam_models_final[[species]]
  emm <- emmeans(mod, ~ idyear_f | geomorphic_zones, type = "response")
  emm_prob <- regrid(emm, transform = "response")
  contrast(emm_prob, method = "trt.vs.ctrl", ref = baseline_index, adjust = "mvt")
})
names(baseline_contrasts_prob_final) <- names(gam_models_final)

summary(baseline_contrasts_prob_final[["Live_Hard_coral_pts"]])

## Attach readable survey labels + 95% CI, export
attach_baseline_labels <- function(df_out) {
  raw <- as.character(df_out$contrast)
  sep <- if (any(grepl(" / ", raw, fixed = TRUE))) " / " else " - "
  compared_level <- vapply(strsplit(raw, sep, fixed = TRUE), `[`, character(1), 1)
  compared_level <- gsub("idyear_f", "", compared_level, fixed = TRUE)
  compared_level <- trimws(gsub("[()]", "", compared_level))
  df_out$idyear <- as.numeric(compared_level)
  merge(df_out, idyear_lookup[, c("idyear", "survey_label")], by = "idyear", all.x = TRUE)
}

species_output_names <- c(
  Live_Hard_coral_pts     = "Live_Hard_coral",
  EAM_Reef_Matrix_pts     = "EAM_Reef_Matrix",
  Bleached_Hard_coral_pts = "Bleached_Hard_coral"
)

baseline_contrasts_prob_final_df <- lapply(names(gam_models_final), function(species){
  df_out <- attach_baseline_labels(as.data.frame(baseline_contrasts_prob_final[[species]]))
  df_out$lci_95 <- df_out$estimate - 1.96 * df_out$SE
  df_out$uci_95 <- df_out$estimate + 1.96 * df_out$SE
  df_out
})
names(baseline_contrasts_prob_final_df) <- names(gam_models_final)

for (species in names(gam_models_final)) {
  out_name <- species_output_names[[species]]
  write.csv(baseline_contrasts_prob_final_df[[species]],
            paste0("time_contrasts_vs_baseline_probscale_FINAL_", out_name, ".csv"),
            row.names = FALSE)
}

baseline_contrasts_prob_final_df[["Live_Hard_coral_pts"]] %>%
  select(survey_label, geomorphic_zones, estimate, lci_95, uci_95, p.value) %>%
  arrange(geomorphic_zones, survey_label)


summary(baseline_contrasts_prob_final[["EAM_Reef_Matrix_pts"]])
summary(baseline_contrasts_prob_final[["Bleached_Hard_coral_pts"]])


## Table S8: combined baseline contrasts, all three
zone_levels  <- levels(modelDF$geomorphic_zones)
survey_order <- idyear_lookup$survey_label[order(idyear_lookup$idyear)]

table_s8_species_labels <- c(
  Live_Hard_coral_pts     = "Non-Bleached Hard Coral",
  EAM_Reef_Matrix_pts     = "Turf on Dead Hard Coral",
  Bleached_Hard_coral_pts = "Bleached Hard Coral"
)

baseline_contrasts_TableS8 <- do.call(rbind, lapply(names(baseline_contrasts_prob_final_df), function(species){
  df_out <- baseline_contrasts_prob_final_df[[species]]
  data.frame(
    life_status     = table_s8_species_labels[[species]],
    survey_time     = df_out$survey_label,
    geomorphic_zone = df_out$geomorphic_zones,
    estimate        = df_out$estimate,
    SE              = df_out$SE,
    lci_95          = df_out$lci_95,
    uci_95          = df_out$uci_95,
    p_value         = df_out$p.value
  )
})) %>%
  mutate(
    life_status = factor(life_status, levels = table_s8_species_labels[c("Live_Hard_coral_pts",
                                                                         "Bleached_Hard_coral_pts",
                                                                         "EAM_Reef_Matrix_pts")]),
    geomorphic_zone = factor(geomorphic_zone, levels = zone_levels),
    survey_time     = factor(survey_time, levels = survey_order)
  ) %>%
  arrange(life_status, geomorphic_zone, survey_time)

baseline_contrasts_TableS8
write.csv(baseline_contrasts_TableS8, "baseline_contrasts_TableS8_combined.csv", row.names = FALSE)

## Figure 4: population-level predicted trajectories

library(tidyverse)
library(glmmTMB)

## Unique zone x survey combinations actually present in the data
new_data <- unique(modelDF[, c("geomorphic_zones", "idyear_f", "idyear")])
new_data <- new_data[order(new_data$idyear, new_data$geomorphic_zones), ]

species_output_names <- c(
  Live_Hard_coral_pts     = "Live_Hard_coral",
  EAM_Reef_Matrix_pts     = "EAM_Reef_Matrix",
  Bleached_Hard_coral_pts = "Bleached_Hard_coral"
)

result_data_final <- do.call("rbind", lapply(names(gam_models_final), function(species){
  mod <- gam_models_final[[species]]
  
  preds <- predict(mod, newdata = new_data, type = "link",
                   se.fit = TRUE, re.form = NA, allow.new.levels = TRUE)
  
  cbind(new_data,
        fit    = preds$fit,
        se.fit = preds$se.fit,
        species = species_output_names[[species]])
}))

result_data_final$lci  <- plogis(result_data_final$fit - 1.96 * result_data_final$se.fit)
result_data_final$uci  <- plogis(result_data_final$fit + 1.96 * result_data_final$se.fit)
result_data_final$prob <- plogis(result_data_final$fit)

result_data_final$species <- factor(result_data_final$species,
                                    levels = c("Live_Hard_coral", "Bleached_Hard_coral", "EAM_Reef_Matrix"))

result_data_final$species_label <- factor(result_data_final$species,
                                          levels = c("Live_Hard_coral", "Bleached_Hard_coral", "EAM_Reef_Matrix"),
                                          labels = c("A) Non-Bleached Hard Coral", "B) Bleached Hard Coral", "C) Turf on Dead Hard Coral"))

## Date reconstruction
## converts idyear back to a plottable date, with a small per-zone
## horizontal offset so the four zones' points don't overlap exactly
idyear_to_date <- function(idyear) {
  origin_date <- as.Date("2019-10-01")
  yearComp <- (idyear %/% 1)
  dayComp  <- (idyear %% 1)
  year(origin_date) <- 2018 + yearComp
  yday(origin_date) <- round(dayComp * ifelse(year(origin_date) %% 4, 366, 365))
  return(origin_date)
}

geomorphic_zone_order <- c("Leeward Reef Slope North", "Reef Flat",
                           "Windward Reef Slope East", "Windward Reef Slope West")
result_data_final$geomorphic_zones <- factor(result_data_final$geomorphic_zones, levels = geomorphic_zone_order)

result_data_final$new_date <- idyear_to_date(result_data_final$idyear) +
  seq(-15, 15, len = 4)[c(1,4,2,3)][result_data_final$geomorphic_zones]

## Plot
field_season_dates <- as.Date(c("2019-10-01", "2020-10-01", "2021-11-01", "2022-11-01",
                                "2023-10-01", "2024-03-01", "2024-11-01", "2025-06-01", "2025-10-01"))

species_colors <- c('Leeward Reef Slope North' = '#FF7F50', 'Windward Reef Slope East' = '#009E73',
                    'Windward Reef Slope West' = '#56B4E9', 'Reef Flat' = '#F0E442')

ggplot(result_data_final, aes(x = new_date, y = prob, color = geomorphic_zones)) +
  geom_path() +
  geom_point() +
  geom_vline(xintercept = as.Date("2025-03-15"), linetype = "dashed", color = "black", size = 0.5) +
  geom_segment(aes(x = new_date, y = uci, yend = lci)) +
  facet_wrap(~ species_label, ncol = 1) +
  annotate("rect", xmin = as.Date("2024-01-01"), xmax = as.Date("2024-05-31"),
           ymin = -Inf, ymax = Inf, fill = "gray50", alpha = 0.4) +
  annotate("rect", xmin = as.Date("2020-02-15"), xmax = as.Date("2020-05-31"),
           ymin = -Inf, ymax = Inf, fill = "gray50", alpha = 0.4) +
  scale_color_manual(values = species_colors) +
  labs(title = "", x = "Survey Event", y = "Predicted Proportion (composition conditional on hard substrate)",
       color = "Geomorphic Zone") +
  scale_x_date(date_labels = "%Y-%m", breaks = field_season_dates) +
  theme(legend.position = "right",
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(1, "mm"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(face = "bold", hjust = 0))

ggsave("Figure4_GLMM_FINAL_binomial.pdf", width = 10, height = 8, bg = "white")

write.csv(result_data_final, "result_data_FINAL_binomial.csv", row.names = FALSE)

coef_table <- do.call(rbind, lapply(names(gam_models_binom), function(sp){
  s <- summary(gam_models_binom[[sp]])$coefficients$cond
  data.frame(life_status = species_output_names[[sp]], term = rownames(s), s, row.names = NULL)
}))
write.csv(coef_table, "fixed_effect_coefficients_TableS6.csv", row.names = FALSE)

## (A) sensitivity analysis restricted to consistently-resampled
##     transects, to check whether main conclusions are robust to
##     changes in sampling composition
## (B) images-retained-per-cell table, built from n_photos (already
##     tracked in modelDF from the trial-count correction)

library(tidyverse)
library(glmmTMB)
library(emmeans)
library(performance)

## PART A: Images-retained-per-cell table

images_retained_table <- modelDF %>%
  group_by(geomorphic_zones, idyear_f) %>%
  summarise(
    n_transects = n_distinct(site),
    n_photos    = sum(n_photos),
    .groups = "drop"
  ) %>%
  arrange(geomorphic_zones, idyear_f)

images_retained_table
write.csv(images_retained_table, "images_retained_per_cell_TableS2.csv", row.names = FALSE)

images_retained_table %>%
  group_by(geomorphic_zones) %>%
  mutate(median_transects_this_zone = median(n_transects)) %>%
  filter(n_transects < median_transects_this_zone) %>%
  ungroup() %>%
  select(geomorphic_zones, idyear_f, n_transects, median_transects_this_zone, n_photos)

## PART B: Sensitivity analysis -- consistently-resampled transects only
## Identify which transects were sampled at EVERY one of the 9 survey
## events
n_surveys_total <- n_distinct(modelDF$idyear_f)

site_survey_counts <- modelDF %>%
  group_by(site, geomorphic_zones) %>%
  summarise(n_surveys_present = n_distinct(idyear_f), .groups = "drop")

# How many transects per zone are fully balanced (present at all 9)?
site_survey_counts %>%
  group_by(geomorphic_zones) %>%
  summarise(
    n_transects_total     = n(),
    n_transects_balanced  = sum(n_surveys_present == n_surveys_total),
    .groups = "drop"
  )

balanced_sites <- site_survey_counts %>%
  filter(n_surveys_present == n_surveys_total) %>%
  pull(site)

modelDF_balanced <- modelDF %>% filter(site %in% balanced_sites)

# Confirm the balanced panel's zone composition before fitting anything
modelDF_balanced %>%
  group_by(geomorphic_zones) %>%
  summarise(n_transects = n_distinct(site), n_rows = n(), .groups = "drop")

## Refit the three final models on the balanced panel, same
## specification as the full-panel final models (random slope +
## intercept, (idyear | site), for all three)
species_columns_pts <- c("Live_Hard_coral_pts", "EAM_Reef_Matrix_pts", "Bleached_Hard_coral_pts")

fit_randomslope <- function(species, data) {
  data$success <- data[[species]]
  data$failure <- data$hard_substrate_points - data$success
  data$failure[data$failure < 0] <- 0
  glmmTMB(cbind(success, failure) ~ geomorphic_zones * idyear_f + (1 + idyear | site),
          data = data, family = binomial(link = "logit"))
}

gam_models_balanced <- list(
  Live_Hard_coral_pts     = fit_randomslope("Live_Hard_coral_pts", modelDF_balanced),
  EAM_Reef_Matrix_pts     = fit_randomslope("EAM_Reef_Matrix_pts", modelDF_balanced),
  Bleached_Hard_coral_pts = fit_randomslope("Bleached_Hard_coral_pts", modelDF_balanced)
)

# Check convergence/singularity on the balanced-panel models before
# trusting them
lapply(gam_models_balanced, performance::check_singularity)

## Same baseline-vs-Oct2019 contrasts as the full-panel analysis
baseline_level_bal <- levels(modelDF_balanced$idyear_f)[1]
baseline_index_bal <- match(baseline_level_bal, levels(modelDF_balanced$idyear_f))

set.seed(2026)
baseline_contrasts_balanced <- lapply(names(gam_models_balanced), function(species){
  mod <- gam_models_balanced[[species]]
  emm <- emmeans(mod, ~ idyear_f | geomorphic_zones, type = "response")
  emm_prob <- regrid(emm, transform = "response")
  contrast(emm_prob, method = "trt.vs.ctrl", ref = baseline_index_bal, adjust = "mvt")
})
names(baseline_contrasts_balanced) <- names(gam_models_balanced)

summary(baseline_contrasts_balanced[["Live_Hard_coral_pts"]])

## Direct comparison: full panel vs. balanced panel, for specific
## claims (windward west recovery, windward east
## persistent decline, reef flat return to baseline)
compare_sensitivity <- function(species){
  full_df <- as.data.frame(baseline_contrasts_prob_final[[species]])
  bal_df  <- as.data.frame(baseline_contrasts_balanced[[species]])
  
  full_df$panel <- "full"
  bal_df$panel  <- "balanced"
  
  merge(
    full_df[, c("contrast", "geomorphic_zones", "estimate", "SE", "p.value")],
    bal_df[,  c("contrast", "geomorphic_zones", "estimate", "SE", "p.value")],
    by = c("contrast", "geomorphic_zones"),
    suffixes = c("_full", "_balanced")
  )
}

sensitivity_comparison <- compare_sensitivity("Live_Hard_coral_pts")
sensitivity_comparison
write.csv(sensitivity_comparison, "sensitivity_analysis_comparison_LiveHardCoral_TableS9.csv", row.names = FALSE)

## Flag any comparison where the conclusion (sign + significance)
## actually flips between the full and balanced panel
sensitivity_comparison %>%
  mutate(
    sig_full     = p.value_full < 0.05,
    sig_balanced = p.value_balanced < 0.05,
    conclusion_flipped = sig_full != sig_balanced
  ) %>%
  filter(conclusion_flipped) %>%
  select(contrast, geomorphic_zones, estimate_full, p.value_full, estimate_balanced, p.value_balanced)
