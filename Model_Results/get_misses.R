#' Get misses
#' 
#' @description join and format results to review misclassifications
#' 
#' @import dplyr
#' 
#' @export
get_misses <- function(model_type) {
  
  # define subfunction for formatting individual results
  format_misses <- function(results){
    # define dataframe of pairwise predictions
    df <- results$df_counts
    
    # get count sums for predictions in each class
    class_sums <- df %>% 
      dplyr::group_by(class_name) %>% 
      dplyr::summarize(total = sum(n)) 
    
    # calculate proportions for each pair
    df <- df %>%
      dplyr::left_join(class_sums, by = "class_name") %>%
      dplyr::mutate(prop = n/total)
    
    # filter out correct preds and sum totals
    misses_df <- df %>% 
      dplyr::filter(class_name != prediction) %>%
      dplyr::select(-total)
    
    return(misses_df)
  }

  # run misses function on all score thresholds and rename columns
  misses_full <- format_misses(results_full) %>% dplyr::mutate(score_threshold = 0.1)
  misses_20 <- format_misses(results_20) %>% dplyr::mutate(score_threshold = 0.2)
  misses_30 <- format_misses(results_30) %>% dplyr::mutate(score_threshold = 0.3)
  misses_40 <- format_misses(results_40) %>% dplyr::mutate(score_threshold = 0.4)
  misses_50 <- format_misses(results_50) %>% dplyr::mutate(score_threshold = 0.5)
  misses_60 <- format_misses(results_60) %>% dplyr::mutate(score_threshold = 0.6)
  misses_70 <- format_misses(results_70) %>% dplyr::mutate(score_threshold = 0.7)
  misses_80 <- format_misses(results_80) %>% dplyr::mutate(score_threshold = 0.8)
  misses_90 <- format_misses(results_90) %>% dplyr::mutate(score_threshold = 0.9)
  
  # join dfs
  misses <- dplyr::bind_rows(misses_full, misses_20, misses_30,
                             misses_40, misses_50, misses_60,
                             misses_70, misses_80, misses_90) %>%
    dplyr::rename(count = n)
  
  if(model_type == "family"){
    # add predicted order and class, format as factors for plotting
    higher_preds <- group_lab_join %>%
      dplyr::rename(prediction_order = order, prediction_class = class)
    
    misses <- misses %>%
      dplyr::left_join(higher_preds, by = join_by(prediction == family)) %>%
      dplyr::mutate(prediction = factor(prediction, levels = rev(pred_order)))
    
    # handle missing joins
    misses <- misses %>%
      dplyr::mutate(prediction_order = ifelse(prediction == "Strigidae", "Strigiformes",
                                              ifelse(prediction == "vehicle", "vehicle", prediction_order)),
                    prediction_class = ifelse(prediction == "Strigidae", "Aves",
                                              ifelse(prediction == "vehicle", "vehicle", prediction_class))) %>%
      dplyr::mutate(prediction_order = factor(prediction_order, levels = rev(tax_order)))
    
  }
  
  if(model_type == "species"){
    higher_preds <- group_labs %>%
      dplyr::rename(prediction_family = family, prediction_order = order, prediction_class = class)
    
    misses <- misses %>%
      dplyr::left_join(higher_preds, by = join_by(prediction == common.name.general)) %>%
      dplyr::mutate(prediction = factor(prediction, levels = rev(pred_order)))
  }
  
  # add true pos rate to plot on secondary axis
  misses <- misses %>%
    dplyr::left_join(true_pos_df, by = dplyr::join_by(class_name == class, score_threshold==score_threshold)) %>%
    dplyr::left_join(false_pos_df, by = dplyr::join_by(class_name == class, score_threshold==score_threshold)) %>%
    dplyr::left_join(false_neg_df, by = dplyr::join_by(class_name == class, score_threshold==score_threshold))
  
  return(misses)
}
