# Check MegaDetector output and add to training data

rm(list = ls())

# import packages
library(rjson)
library(dplyr)

# load plotting function
code.path <- "C:/Users/amira.burns/OneDrive - USDA/Documents/CameraTrapDetectoR_Files/test_scripts/"
source(paste0(code.path, "plot_img_bbox.R"))

# set path
json_path <- "G:/!ML_training_datasets/Las_Cruces_ARS/Las_Cruces_MD_results.json"

# load detections
json_file <- rjson::fromJSON(file = json_path)
md_results <- json_file$images

# convert results to r data frame
md_df <- data.frame(matrix(nrow = 0, ncol = 7))
colnames(md_df) <- c("filename", "class", "conf", "xmin", "ymin", "xmax", "ymax")

# loop through image files
for(i in 1:length(md_results)){
  # isolate detections for a single image
  img <- md_results[i][[1]]
  
  # define variables
  detections <- img$detections
  filename <- img$file
  
  # create dummy observation for empty detections
  if(length(detections) == 0){
    class <- "empty"
    confidence <- 0
    xmin <- ymin <- xmax <- ymax <- 0
    det_row <- cbind(filename, class, conf, xmin, ymin, xmax, ymax)
    md_df <- rbind(md_df, det_row)
  }
  
  else {
    # loop through each prediction in a given image
    for(j in 1:length(detections)){
      
      # isolate individual detection
      pred <- detections[j][[1]]
      
      # define cat, conf, bbox
      class <- pred$category
      conf <- pred$conf
      xmin <- pred$bbox[1]
      ymin <- pred$bbox[2]
      w <- pred$bbox[3]
      h <- pred$bbox[4]
      xmax <- xmin + w
      ymax <- ymin + h
      
      # cat results into a vector
      det_row <- cbind(filename, class, conf, xmin, ymin, xmax, ymax)
      
      # add detection to df
      md_df <- rbind(md_df, det_row)
    }
  }
}

# do some data processing
md_df <- md_df %>%
  # convert numbers to numerics
  mutate(conf = as.numeric(conf), xmin = as.numeric(xmin),
         ymin = as.numeric(ymin), xmax = as.numeric(xmax), ymax = as.numeric(ymax)) %>%
  # convert class code to label
  mutate(class = ifelse(class == "1", "animal",
                        ifelse(class == "2", "person",
                               ifelse(class == "3", "vehicle", class)))) %>%
  # create a flag column
  mutate(flag = NA)

# look at class counts 
md_df %>% count(class)

# flag any detections that are not animals
md_df <- md_df %>%
  mutate(flag = ifelse(class != "animal", "class_flag", flag))

# flag detections below acceptable confidence threshold
md_df$conf <- round(md_df$conf, 2)

md_df <- md_df %>%
  mutate(flag = ifelse(conf < 0.80, paste(flag, "conf_flag", sep = ", "), flag))

# get unique image files 
md_flag_files <- md_df %>%
  select(c(filename, flag)) %>%
  filter(!is.na(flag)) %>% distinct()

# Think about what comes next in terms of verifying results