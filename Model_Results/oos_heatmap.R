#' Make confusion matrix for OOS reults
#' 
#' @description return heatmap confusion matrix of out of sample verified results
#' 
#' @param df data frame with results and preds joined
#' @param model character string identifying model to use for generating heatmap: "species", "family", or "general"
#' @param confidence_threshold minimum confidence threshold for filtered predictions. Any predictions below this threshold will be changed to "Empty"
#' @param source_id character string identifier for data source / location, will be used in title
#' 
#' @details
#' Provide formatted data frame, models to run, and confidence threshold
#'
#' @import dplyr
#' @import tidyr
#' @import circlize
#' @import ComplexHeatmap
#' @import gridtext
#' @import grid
#' @import oos_eval_metrics

oos_heatmap <- function(df, model, confidence_threshold, source_id) {
  
  # source metrics and df_wide functions
  #source(paste0(getwd(), "/Model_Results/oos_eval_metrics.R"))
  #source(paste0(getwd(), "/Model_Results/get_df_wide.R"))
  

  
  
  if(model == "species"){
    
    # get rid of image error predictions
    df <- df %>% dplyr::filter(species_prediction != "image_error")
    
    # get species df_wide
    df_wide <- get_df_wide(df=df, model="species", confidence_threshold = confidence_threshold)
    
    # get species metrics
    sp_mets <- oos_eval_metrics(df=df, model="species", 
                                confidence_threshold = confidence_threshold,
                                source_id = source_id)
    
    
    # get total row and col counts
    N <- ncol(df_wide)
    mat <- as.matrix(df_wide[,5:N])
    pred_counts <- colSums(mat)
    
    # make ordered dict for targets
    tar_cl <- df_wide %>%
      dplyr::select(c(true_species, true_family, true_order, true_class))
    
    # make ordered dict for preds
    pred_cl <- data.frame(species_prediction = colnames(df_wide)[5:N]) %>%
      dplyr::left_join(tax_dict, by=join_by("species_prediction"=="true_species"))
    
    # reorder true counts to match matrix
    true_ct <- sp_mets[match(df_wide$true_species, sp_mets$class_name),]
    # calculate proportion matrix
    prop_mat <- round(mat / true_ct$true_count, 4)
    rownames(prop_mat) <- true_ct$class_name
    
    # split rows and cols for heatmap
    class_split_rows <- factor(df_wide$true_class, levels = unique(tar_cl$true_class))
    split_rows <- factor(df_wide$true_family, levels = unique(tar_cl$true_family))
    split_cols <- factor(pred_cl$true_family, levels = unique(pred_cl$true_family))
    
    # get row and col order
    ordered_rows <- as.character(df_wide$true_species)
    ordered_cols <- colnames(df_wide)[5:N]
    
    # reorder FP and FN bars to match target and pred cols
    FPs <- sp_mets[match(pred_cl$species_prediction, sp_mets$class_name), "FP_rate"]
    FNs <- sp_mets[match(tar_cl$true_species, sp_mets$class_name), "FN_rate"]
    
    # set color scheme
    metric_col <- circlize::colorRamp2(c(0, 0.05, 0.1, 0.5, 0.8, 1), c("white", "snow", "wheat1", "skyblue", "slateblue2", "darkorchid4"))
    
    # set rightside annotation
    right_annotation = ComplexHeatmap::rowAnnotation(bar = ComplexHeatmap::anno_barplot(FNs,
                                                                                        axis=TRUE,
                                                                                        ylim=c(0,1),
                                                                                        gp = grid::gpar(fill = "#737373", border=NA, lty="blank"),
                                                                                        axis_param = list(gp=grid::gpar(fontsize=10),
                                                                                                          at=c(0,0.5,1), 
                                                                                                          labels=c(0,0.5,1))),
                                                     show_annotation_name=TRUE,
                                                     annotation_label = c("False Negative Rate"),
                                                     annotation_name_side ="top")
    
    # set bottom annotation
    bottom_annotation = ComplexHeatmap::columnAnnotation(bar = ComplexHeatmap::anno_barplot(FPs,
                                                                                            axis=TRUE,
                                                                                            ylim=c(0, 1),
                                                                                            gp = grid::gpar(fill = "#737373", border=NA, lty="blank"),
                                                                                            axis_param = list(
                                                                                              gp=grid::gpar(fontsize=10),
                                                                                              at=c(0, 0.5, 1), 
                                                                                              labels=c(0, 0.5, 1))),
                                                         show_annotation_name=TRUE,
                                                         annotation_label = c("False Positive Rate"),
                                                         annotation_name_side ="left",
                                                         annotation_name_rot=0)
    
    # print heatmap
    heat_map <- ComplexHeatmap::Heatmap(prop_mat, name="Proportion", cluster_columns=FALSE, cluster_rows=FALSE,
                                        column_names_side = "top", row_names_side = "left",
                                        col = metric_col,
                                        na_col = "white", 
                                        row_order = ordered_rows, 
                                        column_order = ordered_cols,
                                        row_split = split_rows, column_split = split_cols,
                                        row_title = "True Species", 
                                        column_title = ComplexHeatmap::gt_render(
                                          paste0("<span style='font-size:18pt; color:black'>Species Model V2 Confusion Matrix</span>",
                                                 "<br>Location = ", source_id,
                                                 "<br>Score Threshold = ", confidence_threshold,
                                                 "<br><br>Predicted Species"),
                                          r = unit(2, "mm"), padding = unit(c(2, 2, 2, 2), "pt")),
                                        row_names_gp = grid::gpar(fontsize = 12),
                                        row_gap = unit(0, "mm"), column_gap = unit(0, "mm"), border = TRUE,
                                        #left_annotation=left_annotation,
                                        right_annotation=right_annotation,
                                        bottom_annotation=bottom_annotation
    )

      
  }
  
  if(model == "family"){
    
    # get rid of image error predictions
    df <- df %>% dplyr::filter(family_prediction != "image_error")
    
    # get family df_wide
    df_wide <- get_df_wide(df=df, model="family", confidence_threshold = confidence_threshold)
    
    # get family metrics
    fm_mets <- oos_eval_metrics(df=df, model="family", 
                                confidence_threshold = confidence_threshold,
                                source_id = source_id)
    
    
    # get total row and col counts
    N <- ncol(df_wide)
    mat <- as.matrix(df_wide[,4:N])
    pred_counts <- colSums(mat)
    
    tar_cl <- df_wide %>%
      dplyr::select(c(true_family, true_order, true_class))
    
    family_dict <- tax_dict %>%
      dplyr::select(-true_species) %>%
      dplyr::distinct()
    
    # make ordered dict for preds
    pred_cl <- data.frame(family_prediction = colnames(df_wide)[4:N]) %>%
      dplyr::left_join(family_dict, by=join_by("family_prediction"=="true_family")) %>%
      dplyr::distinct()
    
    # reorder true counts to match matrix
    true_ct <- fm_mets[match(df_wide$true_family, fm_mets$class_name),]
    
    # calculate proportion matrix
    prop_mat <- round(mat / true_ct$true_count, 4)
    rownames(prop_mat) <- true_ct$class_name
    
    # split rows and cols for heatmap
    class_split_rows <- factor(df_wide$true_class, levels = unique(tar_cl$true_class))
    split_rows <- factor(df_wide$true_order, levels = unique(tar_cl$true_order))
    split_cols <- factor(pred_cl$true_order, levels = unique(pred_cl$true_order))
    
    # get row and col order
    ordered_rows <- as.character(tar_cl$true_family)
    ordered_cols <- colnames(df_wide)[4:N]
    
    # reorder FP and FN bars to match target and pred cols
    FPs <- fm_mets[match(pred_cl$family_prediction, fm_mets$class_name), "FP_rate"]
    FNs <- fm_mets[match(tar_cl$true_family, fm_mets$class_name), "FN_rate"]
    
    # set color scheme
    metric_col <- circlize::colorRamp2(c(0, 0.05, 0.1, 0.5, 0.8, 1), c("white", "snow", "wheat1", "skyblue", "slateblue2", "darkorchid4"))
    
    # set rightside annotation
    right_annotation = ComplexHeatmap::rowAnnotation(bar = ComplexHeatmap::anno_barplot(FNs,
                                                                                        axis=TRUE,
                                                                                        ylim=c(0,1),
                                                                                        gp = grid::gpar(fill = "#737373", border=NA, lty="blank"),
                                                                                        axis_param = list(gp=grid::gpar(fontsize=10),
                                                                                                          at=c(0,0.5,1), 
                                                                                                          labels=c(0,0.5,1))),
                                                     show_annotation_name=TRUE,
                                                     annotation_label = c("False Negative Rate"),
                                                     annotation_name_side ="top")
    
    # set bottom annotation
    bottom_annotation = ComplexHeatmap::columnAnnotation(bar = ComplexHeatmap::anno_barplot(FPs,
                                                                                            axis=TRUE,
                                                                                            ylim=c(0, 1),
                                                                                            gp = grid::gpar(fill = "#737373", border=NA, lty="blank"),
                                                                                            axis_param = list(
                                                                                              gp=grid::gpar(fontsize=10),
                                                                                              at=c(0, 0.5, 1), 
                                                                                              labels=c(0, 0.5, 1))),
                                                         show_annotation_name=TRUE,
                                                         annotation_label = c("False Positive Rate"),
                                                         annotation_name_side ="left",
                                                         annotation_name_rot=0)
    
    # print heatmap
    heat_map <- ComplexHeatmap::Heatmap(prop_mat, name="Proportion", cluster_columns=FALSE, cluster_rows=FALSE,
                                        column_names_side = "top", row_names_side = "left",
                                        col = metric_col,
                                        na_col = "white", 
                                        row_order = ordered_rows, 
                                        column_order = ordered_cols,
                                        row_split = split_rows, column_split = split_cols,
                                        row_title = "True Family", 
                                        column_title = ComplexHeatmap::gt_render(
                                          paste0("<span style='font-size:18pt; color:black'>Family Model V2 Confusion Matrix</span>",
                                                 "<br>Location = ", source_id,
                                                 "<br>Score Threshold = ", confidence_threshold,
                                                 "<br><br>Predicted Family"),
                                          r = unit(2, "mm"), padding = unit(c(2, 2, 2, 2), "pt")),
                                        row_names_gp = grid::gpar(fontsize = 12),
                                        row_gap = unit(0, "mm"), column_gap = unit(0, "mm"), border = TRUE,
                                        #left_annotation=left_annotation,
                                        right_annotation=right_annotation,
                                        bottom_annotation=bottom_annotation
    )
    
  }
  
  if(model == "general"){
    
    # get rid of image error predictions
    df <- df %>% dplyr::filter(general_prediction != "image_error")
    
    # get family df_wide
    df_wide <- get_df_wide(df=df, model="general", confidence_threshold = confidence_threshold)
    
    # get family metrics
    gn_mets <- oos_eval_metrics(df=df, model="general", 
                                confidence_threshold = confidence_threshold,
                                source_id = source_id)
    
    
    # get total row and col counts
    N <- ncol(df_wide)
    mat <- as.matrix(df_wide[,2:N])
    pred_counts <- colSums(mat)
    
    # make ordered dict for targets
    tar_cl <- data.frame(true_class = df_wide$true_class) 
    
    # make ordered dict for preds
    pred_cl <- data.frame(general_prediction = colnames(df_wide)[2:N])
    
    # reorder true counts to match matrix
    true_ct <- gn_mets[match(df_wide$true_class, gn_mets$class_name),]
    # calculate proportion matrix
    prop_mat <- round(mat / true_ct$true_count, 4)
    rownames(prop_mat) <- true_ct$class_name
    
    # get row and col order
    ordered_rows <- factor(tar_cl$true_class, levels = unique(tar_cl$true_class))
    ordered_cols <- pred_cl$general_prediction
    
    # reorder FP and FN bars to match target and pred cols
    FPs <- gn_mets[match(pred_cl$general_prediction, gn_mets$class_name), "FP_rate"]
    FNs <- gn_mets[match(tar_cl$true_class, gn_mets$class_name), "FN_rate"]
    
    # set color scheme
    metric_col <- circlize::colorRamp2(c(0, 0.05, 0.1, 0.5, 0.8, 1), c("white", "snow", "wheat1", "skyblue", "slateblue2", "darkorchid4"))
    
    # set rightside annotation
    right_annotation = ComplexHeatmap::rowAnnotation(bar = ComplexHeatmap::anno_barplot(FNs,
                                                                                        axis=TRUE,
                                                                                        ylim=c(0,1),
                                                                                        gp = grid::gpar(fill = "#737373", border=NA, lty="blank"),
                                                                                        axis_param = list(gp=grid::gpar(fontsize=10),
                                                                                                          at=c(0,0.5,1), 
                                                                                                          labels=c(0,0.5,1))),
                                                     show_annotation_name=TRUE,
                                                     annotation_label = c("False Negative Rate"),
                                                     annotation_name_side ="top")
    
    # set bottom annotation
    bottom_annotation = ComplexHeatmap::columnAnnotation(bar = ComplexHeatmap::anno_barplot(FPs,
                                                                                            axis=TRUE,
                                                                                            ylim=c(0, 1),
                                                                                            gp = grid::gpar(fill = "#737373", border=NA, lty="blank"),
                                                                                            axis_param = list(
                                                                                              gp=grid::gpar(fontsize=10),
                                                                                              at=c(0, 0.5, 1), 
                                                                                              labels=c(0, 0.5, 1))),
                                                         show_annotation_name=TRUE,
                                                         annotation_label = c("False Positive Rate"),
                                                         annotation_name_side ="left",
                                                         annotation_name_rot=0)
    
    # print heatmap
    heat_map <- ComplexHeatmap::Heatmap(prop_mat, name="Proportion", cluster_columns=FALSE, cluster_rows=FALSE,
                                        column_names_side = "top", row_names_side = "left",
                                        col = metric_col,
                                        na_col = "white", 
                                        row_order = ordered_rows, 
                                        column_order = ordered_cols,
                                        #row_split = split_rows, column_split = split_cols,
                                        row_title = "True Class", 
                                        column_title = ComplexHeatmap::gt_render(
                                          paste0("<span style='font-size:18pt; color:black'>General Model V2 Confusion Matrix</span>",
                                                 "<br>Location = ", source_id,
                                                 "<br>Score Threshold = ", confidence_threshold,
                                                 "<br><br>Predicted Class"),
                                          r = unit(2, "mm"), padding = unit(c(2, 2, 2, 2), "pt")),
                                        row_names_gp = grid::gpar(fontsize = 12),
                                        row_gap = unit(0, "mm"), column_gap = unit(0, "mm"), border = TRUE,
                                        #left_annotation=left_annotation,
                                        right_annotation=right_annotation,
                                        bottom_annotation=bottom_annotation
    )
    
  }
  
  
  
  return(heat_map)

  
}
