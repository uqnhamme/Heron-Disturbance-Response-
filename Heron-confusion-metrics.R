## Classification metrics (precision, recall, F1, support), computed
## for BOTH label resolutions:
##   1. Group-level codes:  point_human_group_code vs point_machine_group_code
##   2. Finer classification codes: point_human_classification vs point_machine_classification
## across all three periods, combined into one tidy CSV.

library(caret)
library(dplyr)
library(tidyr)
library(purrr)

setwd("/") ##set to your own 

period_files <- c(
  "2019-2023 (baseline)"     = "reefcloud-point-classification-MEML_HeronReef_Coral-2026-04-13-filtered_2019_2023.csv",
  "March 2024 (peak bleach)" = "reefcloud-point-classification-MEML_HeronReef_Coral-2026-04-13-filtered_03_2024.csv",
  "2024-2025 (post-bleach)"  = "reefcloud-point-classification-MEML_HeronReef_Coral-2026-04-13-filtered_2024-2025.csv"
)



compute_period_metrics <- function(filepath, period_label, true_col, predicted_col) {
  
  data <- read.csv(filepath)
  
  true <- data[[true_col]]
  predicted <- data[[predicted_col]]
  
  keep <- true != "DNU" & predicted != "DNU" & true != "" & predicted != "" &
    !is.na(true) & !is.na(predicted)
  true <- true[keep]
  predicted <- predicted[keep]
  
  all_levels <- sort(unique(c(true, predicted)))
  true <- factor(true, levels = all_levels)
  predicted <- factor(predicted, levels = all_levels)
  
  cm <- caret::confusionMatrix(predicted, true)
  
  if (is.matrix(cm$byClass)) {
    byclass <- as.data.frame(cm$byClass)
    byclass$class <- gsub("^Class: ", "", rownames(byclass))
    rownames(byclass) <- NULL
  } else {
    byclass <- as.data.frame(t(cm$byClass))
    byclass$class <- levels(true)[2]   # caret's 2-class convention: second level is "positive"
  }
  
  support <- as.data.frame(table(true))
  names(support) <- c("class", "n_support")
  
  byclass %>%
    select(class, precision = `Pos Pred Value`, recall = Sensitivity, f1 = F1) %>%
    left_join(support, by = "class") %>%
    mutate(period = period_label) %>%
    select(period, class, n_support, precision, recall, f1)
}

## Run both label resolutions across all three periods
run_all_periods <- function(true_col, predicted_col, label_level) {
  purrr::map2_dfr(period_files, names(period_files), function(fp, lbl) {
    compute_period_metrics(fp, lbl, true_col, predicted_col)
  }) %>%
    mutate(label_level = label_level)
}

metrics_group <- run_all_periods("point_human_group_code", "point_machine_group_code", "Group")
metrics_fine  <- run_all_periods("point_human_classification", "point_machine_classification", "Classification")

## Combine into one tidy table, chronological period order, group
## resolution before classification resolution
all_period_metrics <- bind_rows(metrics_group, metrics_fine) %>%
  mutate(period = factor(period, levels = names(period_files)),
         label_level = factor(label_level, levels = c("Group", "Classification"))) %>%
  arrange(label_level, period, class) %>%
  select(label_level, period, class, n_support, precision, recall, f1)

all_period_metrics
write.csv(all_period_metrics, "classification_metrics_full.csv", row.names = FALSE)

## Focus view: key group-level classes
key_group_classes <- c("HC", "HCB", "A")  

all_period_metrics %>%
  filter(label_level == "Group" & class %in% key_group_classes) %>%
  arrange(class, period)

all_period_metrics %>%
  filter(label_level == "Classification") %>%
  filter(grepl("rubble|Rubble", class)) %>%
  arrange(class, period)

all_period_metrics %>% filter(label_level == "Classification" & period == "2024-2025 (post-bleach)" & class %in% c("EAM_RB", "EAM_DHC"))
