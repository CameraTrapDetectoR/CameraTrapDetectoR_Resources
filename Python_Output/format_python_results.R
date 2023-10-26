#' Format Python Results
#' 
#' @description format results run in Python
#' 
#' @import dplyr
#' 
#' @export

format_python_results <- function(df){
  # remove unneeded columns
  df <- df %>%
    dplyr::select(-c(file_id)) %>%
    dplyr::rename(file_path = filename,
                  prediction = class_name)
  # pull out image name
  for(i in 1:nrow(df)){
    img_id <- utils::tail(strsplit(df$file_path[i], "/")[[1]], n = 2)
    df$image_name[i] <- paste(img_id[1], img_id[2], sep="/")
  }
  
  # write over low confidence predictions
  df <- df %>%
    mutate(prediction = ifelse(confidence < 0.40, "empty", prediction))
  
  # get prediction counts per image
  prediction_counts <- df %>% count(image_name)
  df <- df %>% dplyr::left_join(prediction_counts, by = "image_name")
  
  # separate single predictions and those with multiple predictions
  single_preds <- dplyr::filter(df, n == 1)
  multiple_preds <- dplyr::filter(df, n > 1) %>% group_by(image_name)
  
  # format single predictions
  single_preds <- single_preds %>%
    dplyr::rename(count = n) %>%
    dplyr::mutate(count = ifelse(prediction == "empty", 0, count))
  
  # filter multiples to one prediction if all are empty
  empt_preds <- dplyr::filter(multiple_preds, all(prediction == "empty"))
  empt_preds <- dplyr::slice(empt_preds, 1) 
  # update counts to 0
  empt_preds <- empt_preds %>% 
    rename(count = n) %>% 
    mutate(count = 0)
  
  # remove empty predictions from multiple_preds
  multiple_preds <- multiple_preds %>% 
    dplyr::filter(prediction != "empty") %>%
    dplyr::select(-n)
  
  # get counts by image name, prediction
  multi_cts <- multiple_preds %>% group_by(image_name, prediction) %>% count()
  
  # join counts to df
  multiple_preds <- multiple_preds %>%
    dplyr::left_join(multi_cts, by = dplyr::join_by(image_name, prediction)) %>%
    dplyr::rename(count = n) %>%
    dplyr::ungroup()
  
  # concatenate and format all predictions
  preds <- dplyr::bind_rows(empt_preds, single_preds, multiple_preds) %>%
    dplyr::arrange(image_name) %>%
    dplyr::relocate(image_name, .after = file_path) %>%
    dplyr::select(-bbox) # rethink removing this
  
  # add columns for ground truth classes and counts
  preds <- preds %>%
    mutate(true_class = "", true_count = "", comments = "")
  
  return(preds)
}

