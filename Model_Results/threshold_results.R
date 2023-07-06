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
#' @import gridtext
#' 
#' @export
#' 
threshold_results <- function(target_df, pred_df, score_threshold, model_type){
  
  # update prediction based on score_threshold
  pred_df <- pred_df %>% 
    dplyr::mutate(prediction = ifelse(confidence < score_threshold, "empty", prediction))
  
  # join targets to preds
  df <- dplyr::left_join(pred_df, target_df, by = "filename")
  #NOTE: as score_threshold rises, will need to add ifelse(...) code that creates an empty prediction for any images where all preds are removed
  
  # change classes to factors
  df <- dplyr::mutate(df, prediction = as.factor(prediction))
  df <- dplyr::mutate(df, class_name = as.factor(class_name))
  
  # get counts of predictions for each target
  df_ct <- df %>% group_by(class_name) %>% count(prediction)
  
  # widen count df
  df_wide <- tidyr::pivot_wider(df_ct, names_from = prediction, values_from = n)
  
  # attach family names to df_wide and format
  if(model_type == "species"){
    df_wide <- df_wide %>%
      dplyr::left_join(group_labs, by = dplyr::join_by(class_name == common.name.general)) %>%
      dplyr::relocate(c(class, order, family), .before = class_name) %>%
      dplyr::distinct() %>%
      dplyr::ungroup()
    
    # join family names to df_ct
    df_ct <- df_ct %>%
      dplyr::left_join(group_labs, by = dplyr::join_by(class_name == common.name.general)) %>%
      dplyr::select(c(family, class_name, prediction, n))
    
    # reorder cols and rows
    df_wide <- df_wide[match(target_order, df_wide$class_name), ]
    df_wide <- df_wide[, c("class", "family", "class_name", pred_order)]
  }
  
  if(model_type == "family"){
    df_wide <- df_wide %>%
      dplyr::left_join(group_lab_join, by = dplyr::join_by(class_name == family)) %>%
      dplyr::relocate(c(class, order), .before = class_name) %>%
      dplyr::distinct() %>%
      dplyr::ungroup()
    
    # join family names to df_ct
    df_ct <- df_ct %>%
      dplyr::left_join(group_lab_join, by = dplyr::join_by(class_name == family))
    
    # reorder cols and rows
    df_wide <- df_wide[match(target_order, df_wide$class_name), ]
    df_wide <- df_wide[, c("class", "order", "class_name", pred_order)]
  }

  
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
  split_rows <- factor(df_wide$order, levels = unique(df_wide$order))
  split_cols <- c(df_wide$order, "empty")
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
  if(model_type == "species"){
    vec<-unique(df_wide[df_wide$class=="Aves","family"])
  }
  if(model_type == "family"){
    vec<-unique(df_wide[df_wide$class=="Aves","order"])
  }

  vec<-vec[is.na(vec)==FALSE]
  
  palette<-colorRampPalette(c("royalblue4","slategray2"))
  aves.col<-palette(length(vec))
  aves.col<-sample(aves.col)
  names(aves.col)<-vec
  
  if(model_type == "species"){
    vec<-unique(df_wide[df_wide$class=="Mammalia","family"])
  }
  if(model_type == "family"){
    vec<-unique(df_wide[df_wide$class=="Mammalia","order"])
  }
  
  vec<-vec[is.na(vec)==FALSE]
  
  palette<-colorRampPalette(c("chocolate4","peachpuff"))
  mam.col<-palette(length(vec))
  mam.col<-sample(mam.col)
  names(mam.col)<-vec
  
  if(model_type == "species"){
    vec<-unique(df_wide[df_wide$class=="Reptilia","family"])
  }
  if(model_type == "family"){
    vec<-unique(df_wide[df_wide$class=="Reptilia","order"])
  }

  vec<-vec[is.na(vec)==FALSE]
  
  rep.col<-"darkolivegreen2"
  names(rep.col)<-vec
    
  group_col<-c(aves.col,mam.col,rep.col)
  
  spec_col <- circlize::colorRamp2(c(0, 0.1, 0.5, 0.8, 1), c("wheat2", "snow", "skyblue", "slateblue2", "darkorchid4"))
  # create sidebar annotations
  if(model_type == "species"){
    left_annotation = ComplexHeatmap::rowAnnotation(
      Class=class_split_rows,
      Family=split_rows,
      show_annotation_name =TRUE,
      annotation_label = c("Class",
                           "Family"),
      annotation_name_side ="top",
      show_legend =FALSE,
      col = list(Class=c("Aves" = "royalblue4", "Mammalia" = "chocolate4", "Reptilia" = "darkolivegreen4"),
                 Family= group_col),
      gap = unit(c(1), "mm")
    )
  }
  if(model_type == "family"){
    left_annotation = ComplexHeatmap::rowAnnotation(
      Class=class_split_rows,
      Order=split_rows,
      show_annotation_name =TRUE,
      annotation_label = c("Class",
                           "Order"),
      annotation_name_side ="top",
      show_legend =FALSE,
      col = list(Class=c("Aves" = "royalblue4", "Mammalia" = "chocolate4", "Reptilia" = "darkolivegreen4"),
                 Order = group_col),
      gap = unit(c(1), "mm")
    )
  }

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
  if(model_type == "species"){
    heat_map <- ComplexHeatmap::Heatmap(prop_mat, name="Proportion", cluster_columns=FALSE, cluster_rows=FALSE,
                                        column_names_side = "top", row_names_side = "left",
                                        col = spec_col,
                                        na_col = "white", 
                                        row_order = ordered_rows, 
                                        column_order = ordered_cols,
                                        row_split = split_rows, column_split = split_cols,
                                        row_title = "True Species", 
                                        column_title = gt_render(
                                          paste0("<span style='font-size:18pt; color:black'>Species Model V2 Confusion Matrix</span>", 
                                                 "<br>Score Threshold = ", score_threshold,
                                                 "<br><br>Predicted Species"),
                                          r = unit(2, "mm"), padding = unit(c(2, 2, 2, 2), "pt")),
                                        row_names_gp = gpar(fontsize = 12),
                                        row_gap = unit(0, "mm"), column_gap = unit(0, "mm"), border = TRUE,
                                        left_annotation=left_annotation,
                                        right_annotation=right_annotation,
                                        bottom_annotation=bottom_annotation
    )
  }
  if(model_type == "family"){
    heat_map <- ComplexHeatmap::Heatmap(prop_mat, name="Proportion", cluster_columns=FALSE, cluster_rows=FALSE,
                                        column_names_side = "top", row_names_side = "left",
                                        col = spec_col,
                                        na_col = "white", 
                                        row_order = ordered_rows, 
                                        column_order = ordered_cols,
                                        row_split = split_rows, column_split = split_cols,
                                        row_title = "True Family", 
                                        column_title = gt_render(
                                          paste0("<span style='font-size:18pt; color:black'>Family Model V2 Confusion Matrix</span>", 
                                                 "<br>Score Threshold = ", score_threshold,
                                                 "<br><br>Predicted Family"),
                                          r = unit(2, "mm"), padding = unit(c(2, 2, 2, 2), "pt")),
                                        row_names_gp = gpar(fontsize = 12),
                                        row_gap = unit(0, "mm"), column_gap = unit(0, "mm"), border = TRUE,
                                        left_annotation=left_annotation,
                                        right_annotation=right_annotation,
                                        bottom_annotation=bottom_annotation
    )
  }
    
    
    # save outputs in a list and return as an object
    results_object <- list(df_counts = df_ct,
                           df_wide = df_wide,
                           metrics = mets,
                           heatmap = heat_map)
    
    return(results_object)
}