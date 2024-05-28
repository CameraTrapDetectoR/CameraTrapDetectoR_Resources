# Process MegaDetector results and plot image copies with bboxes drawn

# clear environment
rm(list = ls())

# load functions
source(paste0(getwd(), "/install_load.R"))
source(paste0(getwd(), "/Box_Processing/format_md_preds.R"))
source(paste0(getwd(), "/Box_Processing/plot_img_bbox.R"))

# load libraries
install_load(c("dplyr", "rjson"))

# format md results
boxes <- format_md_preds(json_path = "G:/!ML_training_datasets/ARS_RangelandResources_SystemsResearchUnit/Photos/ARS_RRSRU_MD_results.json",
                              score_threshold = 0.40)
boxes$filename <- stringr::str_replace_all(boxes$filename, "\\\\", "/")

# join class labels 
# labels <- utils::read.csv("G:/!ML_training_datasets/ARS_RangelandResources_SystemsResearchUnit/amy_raccoons_opossums.csv")
# labels$filename <- stringr::str_replace(labels$FilePath, "//FERALSWINEDB.usda.net/Photos", "G:")
# boxes <- dplyr::left_join(boxes, labels, by = "filename")

boxes <- boxes %>%
  dplyr::mutate(Species = stringr::str_split_i(filename, "/", -2))

# review boxes
boxes %>% group_by(Species) %>% count(class)

# separate files with only animal predictions
non_animal_files <- boxes %>%
  filter(class != "animal") %>%
  select(filename) %>%
  distinct()

boxes_to_plot <- boxes %>%
  filter(!(filename %in% non_animal_files$filename))

# plot histogram of confidence scores
hist(boxes_to_plot$conf)

# filter out any but highest confidence scores
boxes_to_plot_highscores <- boxes_to_plot %>%
  filter(conf > 0.6)

# loop through classes
for(i in 5:length(classes)){
  # create class folder
  class_dir <- paste0("G:/!ML_training_datasets/!TrainingUpdatePhotos/ARS_RRSRU/", classes[i])
  dir.create(class_dir)
  
  # filter boxes
  class_df <- dplyr::filter(boxes_to_plot_highscores,
                            Species == classes[i])
  
  # plot boxes
  plot_img_bbox(class_df, class_dir)
  
}

# plot boxes
#plot_img_bbox(boxes_to_plot_highscores, "G:/!ML_training_datasets/Amy_raccoons_opossums/boxes")


## After reviewing plots and finalizing boxes
### Extract features from final boxes
### Move images to !VarifiedPhotos dir
### Update annotations file



