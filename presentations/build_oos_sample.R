# build oos sample for hyperparameter tuning set used in species_20240404

# libraries
library(dplyr)

# load annotations
df <- data.table::fread("G:/!ML_training_datasets/!VarifiedBoundingBoxes/varified.bboxes.for.training.20240521.csv")

# filter to training sample species
specs <- c("Common_Raccoon", "Common_Raven", "Mountain_Lion", "White-Tailed_Deer", "Wild_Pig",
           "Wild_Turkey")
hp_df <- df %>% dplyr::filter(common.name.general %in% specs)

# get view of sites
hp_df %>%
  dplyr::group_by(common.name.general) %>%
  dplyr::count(site) %>%
  tidyr::pivot_wider(., names_from = common.name.general, values_from = n)
