#' misclassification heatmap
#' 
#' @description create heatmap of misclassifications by class across score threshold
#' 
#' @import dplyr
#' @import ComplexHeatmap
#' @import circlize
#' @import gridtext
#' 
#' @export
#' 
misclassed_heatmap <- function(class_i, model_type) {
  # filter df based on given class
  df <- misses %>% filter(class_name == class_i)
  
  # pivot to wide format for heatmap
  df_mat <-  df %>% 
    dplyr::select(-c(count, true_pos_rate, false_pos_rate, false_neg_rate)) %>% 
    tidyr::pivot_wider(names_from = score_threshold, names_prefix = "score_", values_from = prop) %>%
    dplyr::mutate(across(.cols = starts_with("score"), ~tidyr::replace_na(., 0))) %>%
    dplyr::mutate(across(.cols = starts_with("score"), ~round(.*10, 3)))
  
  # join color scheme and set row split by model type
  if(model_type == "family"){
    df_mat <- df_mat %>%
      dplyr::left_join(col_dict, by = join_by(prediction_order == pred_order))
  }
  if(model_type == "species"){
    df_mat <- df_mat %>%
      dplyr::left_join(col_dict, by = join_by(prediction_family == family))
  }
  
  # sort df to standardized order 
  df_mat <- df_mat[match(pred_order, df_mat$prediction),]
  df_mat <- df_mat[complete.cases(df_mat),]
  
  # set row split
  if(model_type == "family"){
    split_rows <- factor(df_mat$prediction_order, levels = rev(levels(df_mat$prediction_order)))
  }
  if(model_type == "species"){
    split_rows <- factor(df_mat$prediction_family, levels = rev(levels(df_mat$prediction_family)))
  }
  
  
  # ensure complete score threshold range for all classes
  score_cols <- c("score_0.1", "score_0.2", "score_0.3", "score_0.4", "score_0.5",
                  "score_0.6", "score_0.7", "score_0.8", "score_0.9")
  scores_present <- colnames(df_mat)[startsWith(colnames(df_mat), "score") == TRUE]
  scores_needed <- score_cols[which(score_cols %in% scores_present == FALSE)]
  
  if(length(scores_needed) > 0) {
    score_df <- data.frame(matrix(rep(0, length(scores_needed)), nrow=1))
    colnames(score_df) <- scores_needed
    df_mat <- df_mat %>%
      dplyr::bind_cols(score_df) %>%
      dplyr::relocate(scores_needed[1]:scores_needed[length(scores_needed)], .after = prediction_class)
  }
  
  # create matrix of proportion values
  prop_mat <- as.matrix(df_mat[,7:15])
  rownames(prop_mat) <- gsub("_", " ", df_mat$prediction)
  colnames(prop_mat) <- c("0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9")
  
  # set  col and row order
  ordered_rows <- rev(pred_order[pred_order %in% df_mat$prediction])
  ordered_cols <- colnames(prop_mat)
  
  # create below-plot annotation for eval metrics
  sidebar <- plot_df %>%
    dplyr::filter(class == class_i) %>%
    tidyr::pivot_wider(names_from = metric, values_from = rate) %>% 
    dplyr::rename(class_name = class) %>%
    dplyr::select(c(class_name, score_threshold, true_pos_rate, false_pos_rate, false_neg_rate)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(across(true_pos_rate:false_neg_rate, ~round(., 2)))

  bottom_annotation = ComplexHeatmap::HeatmapAnnotation(mt = ComplexHeatmap::anno_lines(matrix(cbind(sidebar$true_pos_rate,
                                                                                                     sidebar$false_pos_rate,
                                                                                                     sidebar$false_neg_rate), ncol = 3),
                                                                                        ylim=c(0, 1), add_points = TRUE, height = unit(2.5, "cm"),
                                                                                        pt_gp = gpar(pch = 16:18, col = c("#84BD00", "#5C88DA", "#CC0C00"), alpha = 0.5),
                                                                                        gp = gpar(col = c("#84BD00", "#5C88DA", "#CC0C00"), lwd = 1.5),
                                                                                        axis_param = list(
                                                                                          gp=gpar(fontsize=10),
                                                                                          at=seq(0, 1, 0.2), 
                                                                                          labels=seq(0, 1, 0.2))),
                                                       show_annotation_name=TRUE,
                                                       annotation_label = c("Evaluation Metrics"),
                                                       annotation_name_side ="left",
                                                       annotation_name_rot=0)
  
  # create manual legend for bottom annotation
  lgd = ComplexHeatmap::Legend(labels = c("True Positive Rate", "False Positive Rate", "False Negative Rate"), 
                               title = "Evaluation Metric",
                               legend_gp = gpar(fill = c("#84BD00", "#5C88DA", "#CC0C00")))
  
  
  # Get total number test targets
  test_n <- nrow(targets[target_df$class_name == class_i,])
  
  hmap <- ComplexHeatmap::Heatmap(prop_mat, name = "Proportion", rect_gp = gpar(type = "none"),
                                            column_title = gt_render(
                                              paste0("<span style='font-size:18pt; color:black'>", toupper(model_type),
                                                     " Model V2 Misclassification Matrix by Score Threshold</span>", 
                                                     "<br>", toupper(model_type), " = ", gsub("_", " ", class_i),
                                                     "<br>", "Test Images = ", test_n)),
                                            cluster_columns=FALSE, cluster_rows=FALSE, row_names_side = "left",
                                            column_names_side = "bottom", column_names_rot = 45,
                                            row_order = ordered_rows, 
                                            row_split = split_rows, border = TRUE,
                                            row_title = "Misclassification",
                                            cell_fun = function(j, i, x, y, width, height, fill){
                                              grid.rect(x = x, y = y, width = width, height = height, 
                                                        gp = gpar(col = "grey", fill = NA))
                                              grid.circle(x = x, y = y, r = abs(prop_mat[i, j])/2 * min(unit.c(width, height)), 
                                                          gp = gpar(fill = df_mat$col[i], col = NA, alpha = 0.7))
                                              if(prop_mat[i, j] > 0){
                                                grid.text(paste0(round(prop_mat[i, j]*10, 2), "%"), x, y, gp = gpar(fontsize = 10))
                                              }
                                            }, show_heatmap_legend = FALSE,
                                            bottom_annotation = bottom_annotation)
  
  # put heatmap and legend together in a list
  maplist$heatmap <- hmap
  maplist$legend <- lgd
  
  return(maplist)
}