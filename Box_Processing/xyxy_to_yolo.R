# convert xyxy coordinates to yolo format

# Note: function assumes xyxy coordinates have already been normalized

xyxy_to_yolo <- function(df) {
  # confirm coordinate columns are in df
  xyxy_cols <- c("xmin", "ymin", "xmax", "ymax")
  
  if(!all(xyxy_cols %in% colnames(df))) {
    stop("Cannot find coordinate columns `xmin`, `ymin`, `xmax`, `ymax` in your data.")
  }
  
  # add check here for normalized coords?
  
  # add yolo coords
  df <- df %>%
    dplyr::mutate(box_w = xmax - xmin,
                  box_h = ymax - ymin,
                  x_center = (xmin + xmax)/2,
                  y_center = (ymin + ymax)/2)
  
  return(df)
}
