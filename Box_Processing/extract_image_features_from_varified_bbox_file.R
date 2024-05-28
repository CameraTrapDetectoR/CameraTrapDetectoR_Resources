# Add image feature labels to varified bounding boxes
## CLEAN THIS UP!!!!

# clear environment
rm(list = ls())

# load functions
source(paste0(getwd(), "/install_load.R"))
source(paste0(getwd(), "/Box_Processing/get_image_features.R"))

# load libraries
install_load(c("imagefluency", "magick", "dplyr", "stringr", "purrr"))


# load annotations file - check for latest version
labels2 <- utils::read.csv("G:/!ML_training_datasets/!VarifiedBoundingBoxes/varified.bounding.boxes_for.training.2023-11-15.csv")

# load any checkpoints
checkpt <- utils::read.csv(paste0(getwd(), "/get_image_features_checkpoint.csv")) 
checkpt <- checkpt %>%
  select(-X)
checkpt <- checkpt %>%
  dplyr::mutate(filename = stringr::str_replace(img.path, "G:/!ML_training_datasets/!VarifiedPhotos/", "")) %>%
  dplyr::select(-img.path)

# join feats to labels
new_labels <- dplyr::left_join(labels2, checkpt, 
                               by = join_by(filename, xmin, ymin, xmax, ymax),
                               suffix = c("", ".y"))

# coalesce features
new_labels <- new_labels %>%
  dplyr::mutate(aspect.ratio = dplyr::coalesce(aspect.ratio, aspect.ratio.y),
                contrast = dplyr::coalesce(contrast, contrast.y),
                complexity = dplyr::coalesce(complexity, complexity.y),
                v.symmetry = dplyr::coalesce(v.symmetry, v.symmetry.y),
                h.symmetry = dplyr::coalesce(h.symmetry, h.symmetry.y),
                self.similarity = dplyr::coalesce(self.similarity, self.similarity.y)) %>%
  dplyr::select(-c(aspect.ratio.y:self.similarity.y))

# save new labels
data.table::fwrite(new_labels, "G:/!ML_training_datasets/!VarifiedBoundingBoxes/varified.bounding.boxes_for.training.2023-11-15.csv")



checkpt <- checkpt %>%
  mutate(filename = stringr::str_replace(filename, "//FERALSWINEDB.usda.net/Photos/!ML_training_datasets/!TrainingUpdatePhotos/Texas_NIFA_Study/", ""))
checkpt <- checkpt %>%
  mutate(original.filename = stringr::str_replace(img.path, "//FERALSWINEDB.usda.net/Photos/!ML_training_datasets/!TrainingUpdatePhotos/Texas_NIFA_Study/", 
                                                  "//FERALSWINEDB.usda.net/Photos/!ML_training_datasets/!VarifiedPhotos/"))
checkpt <- checkpt %>% select(-img.path)
checkpt <- checkpt %>% mutate(common.name.general = "American_Badger")
feats <- feats %>% rename(original.filename = img.path)
feats <- dplyr::bind_rows(feats, checkpt) %>% distinct()

checkpt1 <- utils::read.csv(paste0(getwd(), "/American_Kestrel_bbox_image_features.csv")) 
checkpt1 <- checkpt1 %>%
  select(-X) %>% 
  mutate(filename = stringr::str_replace(img.path, "//FERALSWINEDB.usda.net/Photos/!ML_training_datasets/!VarifiedPhotos/", ""),
         common.name.general = "American_Crow") %>%
  rename(original.filename = img.path)

feats <- dplyr::bind_rows(feats, checkpt1) %>% distinct()

feats <- feats %>%
  mutate(filename = stringr::str_replace(filename, "G:/!ML_training_datasets/!VarifiedPhotos/", "")) %>%
  mutate(filename = stringr::str_replace(filename, "H:/!ML_training_datasets/!VarifiedPhotos/", ""))


# filter already-run boxes from those still needed
labels_to_run <- dplyr::filter(labels, !(filename %in% feats$filename)) 
labels_to_run <- new_labels %>%
  dplyr::filter(if_all(c(self.similarity, h.symmetry, v.symmetry, complexity, contrast), is.na))

# group by classes
classes <- sort(unique(labels_to_run$common.name.general))
classes
labels_to_run %>% count(common.name.general) %>% arrange(n)

labels_to_run <- la

labels_to_run$filename <- paste0("G:/!ML_training_datasets/!VarifiedPhotos/", labels_to_run$filename)

# create placeholder for image features
class_features <- list()

## NOTE 10/30 : running parallel script locally starting with Prairie Chicken (classes[99])

# loop through classes 
for(i in 2:length(classes)) {
  
  # filter total labels by class
  class_labels <- dplyr::filter(labels_to_run, common.name.general==classes[i])
  
  class_labels <- class_labels %>% mutate(filename = paste0("G:/!ML_training_datasets/!VarifiedPhotos/", filename))
  
  print(paste0("Running image features for ", classes[i]))
  
  # get image features
  feature_df <- get_image_features(class_labels, checkpoint_frequency = 10)
  
  feature_df <- feature_df %>% 
    mutate(filename = stringr::str_replace(img.path, "G:/!ML_training_datasets/!VarifiedPhotos/", ""))
  
  # save for single class
  utils::write.csv(feature_df, paste0(getwd(), "/", classes[i], "_bbox_image_features.csv"))
  
  # put classwise df into list
  class_features[[i]] <- feature_df
  
  # join to feats df
  feats <- dplyr::bind_rows(feats, feature_df)
  
  # save image space
  save.image()
  
}

# combine list into single df of features
features_df <- purrr::reduce(class_features, bind_rows)

# join to annotations
annots <- dplyr::left_join(labels, features_df, 
                           by = join_by(filename==img.path, xmin, ymin, xmax, ymax))


# save updated annotations
