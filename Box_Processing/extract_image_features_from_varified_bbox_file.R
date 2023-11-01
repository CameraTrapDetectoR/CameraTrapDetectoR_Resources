# Add image feature labels to varified bounding boxes

# clear environment
rm(list = ls())

# load functions
source(paste0(getwd(), "/install_load.R"))
source(paste0(getwd(), "/Model_Results/get_image_features.R"))

# load libraries
install_load(c("imagefluency", "magick", "dplyr", "stringr", "purrr"))


# load annotations file - check for latest version
labels <- utils::read.csv("G:/!ML_training_datasets/!VarifiedBoundingBoxes/varified.bounding.boxes_for.training.final.2023-09-27.csv")

# attach new filename column with absolute filepath
labels$filename <- paste0("G:/!ML_training_datasets/!VarifiedPhotos/", labels$filename)

# get pre-existing image features
feats <- utils::read.csv("G:/!ML_training_datasets/!VarifiedBoundingBoxes/bbox_image_features_temp.csv")
feats <- feats %>% select(-c(X, X.1))
feats <- feats %>% rename(filename = img.path, img.path = filename)

#feats$filename <- paste0("G:/!ML_training_datasets/!VarifiedPhotos/", feats$filename)

# load any checkpoints
checkpt <- utils::read.csv(paste0(getwd(), "/wildpig_features_checkpoint.csv")) 
checkpt <- checkpt %>%
  select(-X) %>% 
  mutate(filename = stringr::str_replace(img.path, "G:/!ML_training_datasets/!VarifiedPhotos/", ""))

feats <- dplyr::bind_rows(feats, checkpt) %>% distinct()

# save formatted labels
utils::write.csv(feats, "G:/!ML_training_datasets/!VarifiedBoundingBoxes/bbox_image_features_temp.csv")

# filter already-run boxes from those still needed
labels_to_run <- dplyr::filter(labels, !(filename %in% feats$filename)) 

# group by classes
classes <- sort(unique(labels_to_run$common.name.general))
classes
labels_to_run %>% count(common.name.general) %>% arrange(n)

labels_to_run$filename <- paste0("G:/!ML_training_datasets/!VarifiedPhotos/", labels_to_run$filename)

# create placeholder for image features
class_features <- list()

## NOTE 10/30 : running parallel script locally starting with Prairie Chicken (classes[99])

# loop through classes 
for(i in 1:length(classes)) {
  
  # filter total labels by class
  class_labels <- dplyr::filter(labels_to_run, common.name.general==classes[i])
  
  # get image features
  feature_df <- get_image_features(class_labels, checkpoint_frequency = 10)
  
  feature_df <- feature_df %>% 
    rename(filename = img.path) %>% 
    mutate(img.path = stringr::str_replace(filename, "G:/!ML_training_datasets/!VarifiedPhotos/", ""))
  
  # save for single class
  utils::write.csv(feature_df, paste0(getwd(), "/", classes[i], "_bbox_image_features.csv"))
  
  # put classwise df into list
  class_features[[i]] <- feature_df
  
  # join to feats df
  feats <- dplyr::bind_rows(feats, feature_df)
  
  print(paste0("Image features finished for ", classes[i]))
  
  # save image space
  save.image()
  
}

# combine list into single df of features
features_df <- purrr::reduce(class_features, bind_rows)

# join to annotations
annots <- dplyr::left_join(labels, features_df, 
                           by = join_by(filename==img.path, xmin, ymin, xmax, ymax))

# return filename to original state
annots <- dplyr::mutate(annots, filename = stringr::str_replace(filename, "H:/!ML_training_datasets/!Varified_Photos/", ""))

# save updated annotations
