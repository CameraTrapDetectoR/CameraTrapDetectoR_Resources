#' Plot image with bounding box predictions
#' 
#' @description Create a copy of the original image with predicted bounding boxes,
#' confidence scores, and class labels. Save copy to user-defined directory
#' 
#' @param pred_df Prediction dataframe that is output from deployment
#' @param output_dir Desired directory to make plots
#' 
#' @returns jpg image file in output_dir with bboxes and labels plotted
#' 
#' @import magick
#' 
#' @export
#' 
plot_img_bbox<- function(pred_df,
                         output_dir){
  
  # extract unique files
  file_list <- unique(pred_df$filename)
  
  # start progress bar
  print(paste0("Plotting boxes on ", length(file_list), " images."))
  pb = utils::txtProgressBar(min = 0, max = length(file_list), initial = 0,
                             style=3, char="*")
  
  # loop through images
  for(i in 1:length(file_list)) {
    
    # get image filepath
    img_path <- file.path(file_list[i])
    
    # extract image info
    img_info <- pred_df[file.path(pred_df$filename) == img_path, ]
    
    # make new filename
    plt_path <- file.path(paste(output_dir, stringr::str_split_i(img_path, "/", -1), sep="/"))
    
    # open image
    img <- magick::image_read(img_path)
    
    # get image dimensions
    img_w <- magick::image_info(img)$width
    img_h <- magick::image_info(img)$height
    
    # make copy to draw boxes on
    plt_img <- magick::image_draw(img)
    
    # loop through detections in an image
    for(j in 1:nrow(img_info)){
      
      # scale bounding boxes to image dimensions
      xmin <- img_info$xmin[j] * img_w
      ymin <- img_info$ymin[j] * img_h
      xmax <- img_info$xmax[j] * img_w
      ymax <- img_info$ymax[j] * img_h
      conf <- img_info$conf[j]
      
      # plot box
      graphics::rect(xmin, ymin, xmax, ymax, border="red", lwd=2)
      
      # plot conf score
      graphics::text(xmin, ymax+10, paste0("conf = ", conf), col = "red", cex=2)
      
    }
    grDevices::dev.off()
    
    # save image
    magick::image_write(plt_img, path = plt_path, format = "jpg")
    
    # update progress bar
    utils::setTxtProgressBar(pb,i) 
  }

} ## END