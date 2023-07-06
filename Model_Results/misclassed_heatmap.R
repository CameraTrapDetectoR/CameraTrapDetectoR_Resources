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
    split_rows <- factor(df_mat$prediction_order, levels = unique(df_mat$prediction_order))
  }
  if(model_type == "species"){
    df_mat <- df_mat %>%
      dplyr::left_join(col_dict, by = join_by(prediction_family == family))
    split_rows <- factor(df_mat$prediction_family, levels = unique(df_mat$prediction_family))
  }
  
  # sort df to standardized order 
  df_mat <- df_mat[match(c(target_order, "empty"), df_mat$prediction),]
  df_mat <- df_mat[complete.cases(df_mat),]
  
  # create matrix of proportion values
  prop_mat <- as.matrix(df_mat[,7:15])
  rownames(prop_mat) <- gsub("_", " ", df_mat$prediction)
  colnames(prop_mat) <- c("0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9")
  
  # set  col and row order
  ordered_rows <- pred_order[pred_order %in% df_mat$prediction]
  ordered_cols <- colnames(prop_mat)
  
  # create below-plot annotation for true pos rate

  sidebar <- df %>%
    dplyr::select(c(class_name, score_threshold, true_pos_rate, false_pos_rate, false_neg_rate)) %>%
    dplyr::distinct()

  bottom_annotation = ComplexHeatmap::columnAnnotation(bar = ComplexHeatmap::anno_barplot(matrix(cbind(sidebar$true_pos_rate,
                                                                                                       sidebar$false_pos_rate,
                                                                                                       sidebar$false_neg_rate), ncol = 3),
                                                                                          beside = TRUE, attach = TRUE,
                                                                                        ylim=c(0, 1), 
                                                                                        gp = gpar(fill = c("#7F7F7F", "#B3B3B3", "#E3E3E3"), border=NA, lty="blank"),
                                                                                        axis_param = list(
                                                                                          gp=gpar(fontsize=10),
                                                                                          at=c(0, 0.5, 1), 
                                                                                          labels=c(0, 0.5, 1))),
                                                       show_annotation_name=TRUE,
                                                       annotation_label = c("Evaluation Metrics"),
                                                       annotation_name_side ="left",
                                                       annotation_name_rot=0)
  
  misses_heatmap <- ComplexHeatmap::Heatmap(prop_mat, name = "Proportion", rect_gp = gpar(type = "none"),
                                            column_title = gt_render(
                                              paste0("<span style='font-size:18pt; color:black'>", toupper(model_type),
                                                     " Model V2 Misclassification Matrix by Score Threshold</span>", 
                                                     "<br>", model_type, " = ", gsub("_", " ", class_i))),
                                            cluster_columns=FALSE, cluster_rows=FALSE, row_names_side = "left",
                                            column_names_side = "bottom", column_names_rot = 45,
                                            row_order = row.names(prop_mat), 
                                            row_split = split_rows, row_title = "Misclassification",
                                            cell_fun = function(j, i, x, y, width, height, fill){
                                              grid.rect(x = x, y = y, width = width, height = height, 
                                                        gp = gpar(col = "grey", fill = NA))
                                              grid.circle(x = x, y = y, r = abs(prop_mat[i, j])/2 * min(unit.c(width, height)), 
                                                          gp = gpar(fill = df_mat$col[i], col = NA))
                                              if(prop_mat[i, j] > 0){
                                                grid.text(paste0(round(prop_mat[i, j]*10, 2), "%"), x, y, gp = gpar(fontsize = 10))
                                              }
                                            }, show_heatmap_legend = FALSE,
                                            bottom_annotation = bottom_annotation)
  
  return(misses_heatmap)
}