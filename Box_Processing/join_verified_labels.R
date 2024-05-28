### Update Labels Doc with New Images

# load libraries
library(dplyr)
library(stringr)

# define new labels to add
df <- read.csv("G:/!ML_training_datasets/!TrainingUpdatePhotos/ARS_RRSRU/ARS_RRSRU_verified_bboxes_round1.csv")
df <- df %>%
  dplyr::mutate(photo_name = stringr::str_split_i(filename, "/", -1))
df <- df %>%
  dplyr::rename(common.name.general = true_class)
df <- df %>%
  mutate(across(c(filename, common.name.general), ~ stringr::str_replace(.x, "Arizona_Black-tailed_Prairie_Dog", "Black-Tailed_Prairie_Dog"))) %>%
  mutate(across(c(filename, common.name.general), ~ stringr::str_replace(.x, "Black-tailed_Jackrabbit", "Black-Tailed_Jackrabbit")))
df <- df %>%
  dplyr::mutate(original.filename = paste0("G:/!ML_training_datasets/!VarifiedPhotos/", filename)) %>%
  dplyr::select(-c(category, conf))

# load labels
labels <- utils::read.csv("G:/!ML_training_datasets/!VarifiedBoundingBoxes/varified.bounding.boxes_for.training.final.2023-09-27.csv")

# load taxonomic dict
tax_dict <- utils::read.csv("G:/!ML_training_datasets/!VarifiedBoundingBoxes/class_dictionary_20210921.csv")
tax_df <- tax_dict[tax_dict$common.name.general %in% unique(df$common.name.general),]

# remove extra rows
# tax_dict <- tax_dict %>%
#   group_by(common.name.general) %>%
#   arrange() %>%
#   filter(row_number()==1)

# join tax dict to labels
df <- dplyr::left_join(df, tax_df, by = "common.name.general")

# format existing df columns to other label columns
df <- df %>%
  dplyr::mutate(common.name = common.name.general)

# add new columns
df <- df %>%
  dplyr::mutate(bbox.area = (xmax - xmin) * (ymax - ymin)) %>%
  dplyr::mutate(partial.image = FALSE,
                bbox.origin = "UL",
                bbox.source = "MD") %>%
  dplyr::mutate(aspect.ratio = (xmax - xmin) / (ymax - ymin))

# move files to verified directory
files <- df %>% dplyr::select(c(filename_org, original.filename)) %>% distinct()
for(i in 1:nrow(files)){
  file.copy(files$filename_org[i], files$original.filename[i])
  if(i %% 100 == 0){
    print(paste0(i, " files transfered."))
  }
}

# save site-specific final labels
write.csv(df, "G:/!ML_training_datasets/!VarifiedBoundingBoxes/Site.Specific/varified.ARS_RRSRU_round1.bboxes.20231116.csv")

# join to existing annotatons
df <- df %>% select(-filename_org)
new_labels <- dplyr::bind_rows(new_labels, df)

data.table::fwrite(new_labels, "G:/!ML_training_datasets/!VarifiedBoundingBoxes/varified.bounding.boxes_for.training.2023-11-16.csv")
