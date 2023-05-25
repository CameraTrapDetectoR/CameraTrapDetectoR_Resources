#' Create object with detailed results metrics from test dataset 
#' 
#' @description calculates confusion matrix and visualization, evaluation metrics
#' 
#' @param target_df data frame of ground truths
#' @param pred_df data frame of predictions
#' @param score_threshold confidence score to filter predictions
#' 
#' @returns a list of evaluation objects
#' 
#' @import dplyr
#' @import tidyr
#' @import circlize
#' @import ComplexHeatmap
#' 
#' @export
#' 
threshold_results <- function(target_df, pred_df, score_threshold){
  
  # update prediction based on score_threshold
  pred_df <- pred_df %>% 
    dplyr::mutate(prediction = ifelse(confidence < score_threshold, "empty", prediction))
  
  # join targets to preds
  df <- dplyr::left_join(pred_df, target_df, by = "filename")
  #NOTE: as score_threshold rises, will need to add ifelse(...) code that creates an empty prediction for any images where all preds are removed
  
  # change classes to factors
  df <- dplyr::mutate(df, prediction = as.factor(prediction))
  df <- dplyr::mutate(df, class_name = as.factor(class_name))
  # classes <- unique(c(levels(df$prediction), levels(df$class_name)))
  # levels(df$prediction) <- classes
  # levels(df$class_name) <- classes
  
  # get counts of predictions for each target
  df_ct <- df %>% group_by(class_name) %>% count(prediction)
  
  # widen count df
  df_wide <- tidyr::pivot_wider(df_ct, names_from = prediction, values_from = n)
  
  # attach family names to df_wide and format
  df_wide <- df_wide %>%
    dplyr::left_join(group_labs, by = dplyr::join_by(class_name == common.name.general)) %>%
    dplyr::relocate(c(class, order, family), .before = class_name) %>%
    dplyr::distinct()
  
  # reorder cols and rows
  df_wide <- df_wide[match(target_order, df_wide$class_name), ]
  df_wide <- df_wide[, c("class", "family", "class_name", pred_order)]
  
  # replace NA values with 0
  df_wide <- df_wide %>% mutate_all(~replace(., is.na(.), 0))
  
  # get count row and col sums
  N <- ncol(df_wide)
  mat <- as.matrix(df_wide[,4:N])
  target_counts <- rowSums(mat)
  pred_counts <- colSums(mat)
  
  # get proportion matrix
  prop_mat <- round(mat / target_counts, 4)
  rownames(prop_mat) <- target_order
  
  # set family name as col/row splits
  class_split_rows <- factor(df_wide$class, levels = c("Aves", "Mammalia", "Reptilia"))
  split_rows <- factor(df_wide$family, levels = unique(df_wide$family))
  split_cols <- c(df_wide$family, "empty")
  split_cols <- factor(split_cols, levels = c(levels(split_rows), "empty"))
  
  # set col/row order
  ordered_rows <- df_wide$class_name
  ordered_cols <- c(ordered_rows, "empty")
  
  # get accuracy metrics
  mets <- data.frame(class_name = df_wide$class_name,
                     ct_total = target_counts,
                     pred_total = pred_counts[1:length(pred_counts)-1],
                     true_pos = diag(mat))
  mets <- mets %>%
    dplyr::mutate(true_pos_rate = true_pos / ct_total) %>%
    dplyr::mutate(false_pos = pred_total - true_pos) %>%
    dplyr::mutate(false_pos_rate = false_pos / pred_total) %>%
    dplyr::mutate(false_neg = ct_total - true_pos) %>%
    dplyr::mutate(false_neg_rate = false_neg / ct_total)
  
  # set color scheme
  vec<-unique(df_wide[df_wide$class=="Aves","family"])
  vec<-vec[is.na(vec)==FALSE]
  
  palette<-colorRampPalette(c("#113b5f","#d0d1e6"))
  aves.col<-palette(length(vec))
  aves.col<-sample(aves.col)
  names(aves.col)<-vec
  
  vec<-unique(df_wide[df_wide$class=="Mammalia","family"])
  vec<-vec[is.na(vec)==FALSE]
  
  palette<-colorRampPalette(c("#5f3511","#fee391"))
  mam.col<-palette(length(vec))
  mam.col<-sample(mam.col)
  names(mam.col)<-vec
  
  vec<-unique(df_wide[df_wide$class=="Reptilia","family"])
  vec<-vec[is.na(vec)==FALSE]
  
  rep.col<-"#95e4af"
    names(rep.col)<-vec
    
    fam_col<-c(aves.col,mam.col,rep.col)
    
    conf_steps <- seq(0, 1, 0.1)
    spec_col <- circlize::colorRamp2(conf_steps,
                                     c("seashell", "cornsilk", "wheat", "burlywood2", "gold2", "darkgoldenrod1",
                                                 "sandybrown", "tan1", "tan3", "sienna", "sienna4"))
    
    # create sidebar annotations
    left_annotation = ComplexHeatmap::rowAnnotation(
      Class=class_split_rows,
      Family=split_rows,
      show_annotation_name =TRUE,
      annotation_label = c("Class",
                           "Family"),
      annotation_name_side ="top",
      show_legend =FALSE,
      col = list(Class=c("Aves" = "#113b5f", "Mammalia" = "#5f3511", "Reptilia" = "#238b45"),
                 Family= fam_col),
      gap = unit(c(1), "mm")
    )
    right_annotation = ComplexHeatmap::rowAnnotation(bar = ComplexHeatmap::anno_barplot(mets$false_neg_rate,
                                                        axis=TRUE,
                                                        ylim=c(0,1),
                                                        gp = gpar(fill = "#737373", border=NA, lty="blank"),
                                                        axis_param = list(gp=gpar(fontsize=10),
                                                                          at=c(0,0.5,1), 
                                                                          labels=c(0,0.5,1))),
                                     show_annotation_name=TRUE,
                                     annotation_label = c("False Negative Rate"),
                                     annotation_name_side ="top")
    
    bottom_annotation = ComplexHeatmap::columnAnnotation(bar = ComplexHeatmap::anno_barplot(c(mets$false_pos_rate, 1),
                                                            axis=TRUE,
                                                            ylim=c(0, 1),
                                                            gp = gpar(fill = "#737373", border=NA, lty="blank"),
                                                            axis_param = list(
                                                              gp=gpar(fontsize=10),
                                                              at=c(0, 0.5, 1), 
                                                              labels=c(0, 0.5, 1))),
                                         show_annotation_name=TRUE,
                                         annotation_label = c("False Positive Rate"),
                                         annotation_name_side ="left",
                                         annotation_name_rot=0)
    
    # create heatmap
    heat_map <- ComplexHeatmap::Heatmap(prop_mat, name="Proportion", cluster_columns=FALSE, cluster_rows=FALSE,
                        column_names_side = "top", row_names_side = "left",
                        col = spec_col,
                        na_col = "white", 
                        row_order = ordered_rows, 
                        column_order = ordered_cols,
                        row_split = split_rows, column_split = split_cols,
                        row_title = "True Species", column_title = "Predicted Species",
                        row_names_gp = gpar(fontsize = 14),
                        row_gap = unit(0, "mm"), column_gap = unit(0, "mm"), border = TRUE,
                        left_annotation=left_annotation,
                        right_annotation=right_annotation,
                        bottom_annotation=bottom_annotation
    )
    
    # save outputs in a list and return as an object
    results_object <- list(df_counts = df_ct,
                           df_wide = df_wide,
                           metrics = mets,
                           heatmap = heat_map)
    
    return(results_object)
}