#' Extract Image Features
#' 
#' @description Calculate image feature scores for an image or a bounding box
#' 
#' @details This function extracts various image features from an image or detected object
#' within an image, defined by a normalized bounding box. This function assumes the bbox format 
#' (xmin, ymin, xmax, ymax) where: 0 <= xmin <= xmax <= 1 ; 
#' 0 <= ymin <= ymin <= 1 ; and (xmin, ymin) corresponds to the lower-left 
#' corner of the bounding box. 
#' 
#' The following features are calculated and saved: aspect ratio of bounding box, 
#' contrast, complexity, horizontal and vertical symmetry, and self-similarity. The aspect 
#' ratio is simply bbox (or image) width divided by bbox (or image) height. More details 
#' regarding the other image features are available in the `imagefluency` package
#' documentation: https://imagefluency.com/articles/getting-started.html
#' 
#' @param df dataframe containing list of absolute filepaths to images, and optionally 
#' coordinates for bounding boxes. Filepath column name should be the first column in the 
#' dataframe containing the character string 'file'. Bounding box coordinates should be in 
#' four columns labeled according to the naming convention and formatting requirements detailed 
#' in the `details` section.
#' @param checkpoint_frequency save df to checkpoint every x rows. A csv file will be saved to working directory
#' 
#' @returns data frame with columns for the image file path, (optional) bounding box coordinates,
#' and columns for each of the image features. 
#' 
#' @import imagefluency
#' @import magick
#' @import dplyr
#' 
#' 
#' @export
#' 
get_image_features <- function(df, checkpoint_frequency) {
  
  # extract relevant columns from df
  file_col <- colnames(df)[grepl("file", colnames(df), ignore.case=T)][1]
  xmin_col <- colnames(df)[grepl("xmin", colnames(df), ignore.case=T)][1]
  ymin_col <- colnames(df)[grepl("ymin", colnames(df), ignore.case=T)][1]
  xmax_col <- colnames(df)[grepl("xmax", colnames(df), ignore.case=T)][1]
  ymax_col <- colnames(df)[grepl("ymax", colnames(df), ignore.case=T)][1]

  
  if(length(xmin_col)==0 | length(xmax_col)==0 | length(ymin_col)==0 | length(ymax_col)==0){
    print("Cannot identify bounding box coordinates. Image features will be calculated 
          over the entire image.\n")
    input_df <- data.frame(img.path = df[ , file_col])
  } else {
    input_df <- data.frame(img.path = df[ , file_col],
                           xmin = df[, xmin_col],
                           ymin = df[, ymin_col],
                           xmax = df[, xmax_col],
                           ymax = df[, ymax_col])
  }
  
  # remove duplicate rows or any rows where there is no file path
  input_df <- dplyr::filter(input_df, !is.na(img.path))
  input_df <- dplyr::distinct(input_df)
  
  # create df to store output
  feature_df <- dplyr::mutate(input_df, 
                              aspect.ratio = NA,
                              contrast = NA,
                              complexity = NA,
                              v.symmetry = NA,
                              h.symmetry = NA,
                              self.similarity = NA)
  
  # create progress bar
  print(paste0("Running feature extraction on ", nrow(feature_df), "images or bboxes.\n"))
  pb = utils::txtProgressBar(min = 0, max = nrow(feature_df), initial = 0,
                             style=3, char="*")

  # loop through feature df 
  for(i in 1:nrow(feature_df)){
    
    # open image 
    img <- tryCatch(magick::image_read(feature_df$img.path[i]), 
                    error = function(e) "error")
    
    # manage errors
    if("error" %in% list(img)) {
      feature_df$aspect.ratio[i] <- "image error"
      feature_df$complexity[i] <- "image error"
      feature_df$contrast[i] <- "image error"
      feature_df$v.symmetry[i] <- "image error"
      feature_df$h.symmetry[i] <- "image error"
      feature_df$self.similarity[i] <- "image error"
    } else {
      
      # get image dimensions
      img_w <- magick::image_info(img)$width
      img_h <- magick::image_info(img)$height
      
      # feature extraction on bbox
      if(all(c("xmin", "ymin", "xmax", "ymax") %in% colnames(feature_df))) {
        # get box dimensions 
        box_w <- (feature_df$xmax[i] - feature_df$xmin[i]) * img_w
        box_h <- (feature_df$ymax[i] - feature_df$ymin[i]) * img_h
        
        # calculate aspect ratio
        feature_df$aspect.ratio[i] <- round(box_w / box_h, digits=3)
        
        # crop image to bbox
        bbox <- magick::image_crop(img, geometry = geometry_area(width=box_w, height=box_h, 
                                                                 x_off=feature_df$xmin[i]*img_w, 
                                                                 y_off=feature_df$ymin[i]*img_h))
        
        # save bbox as jpg so it can be reloaded with imagefluency package
        temp_path <- paste0(getwd(), "/temp_bbox.jpg")
        magick::image_write(bbox, path = temp_path)
        
        # reload bbox
        box_f <- imagefluency::img_read(temp_path)
        
        # add features to df
        feature_df$complexity[i] <- imagefluency::img_complexity(temp_path)
        feature_df$contrast[i] <- imagefluency::img_contrast(box_f)
        feature_df$v.symmetry[i] <- imagefluency::img_symmetry(box_f)[['vertical']]
        feature_df$h.symmetry[i] <- imagefluency::img_symmetry(box_f)[['horizontal']]
        feature_df$self.similarity[i] <- imagefluency::img_self_similarity(box_f)
        
        #remove temp file
        #file.remove(temp_path)
      } 
      
      # extract features for a full image
      if(all(c("xmin", "ymin", "xmax", "ymax") %in% colnames(feature_df))==FALSE) {
        
        # open image using imagefluency
        img_f <- imagefluency::img_read(feature_df$img.path[i])
        
        # extract features and add to df
        feature_df$aspect.ratio[i] <- round(img_w / img_h, digits=3)
        feature_df$complexity[i] <- imagefluency::img_complexity(feature_df$img.path[i])
        feature_df$contrast[i] <- imagefluency::img_contrast(img_f)
        feature_df$v.symmetry[i] <- imagefluency::img_symmetry(img_f)[['vertical']]
        feature_df$h.symmetry[i] <- imagefluency::img_symmetry(img_f)[['horizontal']]
        feature_df$self.similarity[i] <- imagefluency::img_self_similarity(img_f)
        
      }
    }
    
    # update progress bar
    utils::setTxtProgressBar(pb,i) 
    
    # save checkpoint
    if(i %% checkpoint_frequency == 0){
      utils::write.csv(feature_df, paste0(getwd(), "get_image_features_checkpoint.csv"))
    }
    
  }
  
  return(feature_df)
  
} ##END
