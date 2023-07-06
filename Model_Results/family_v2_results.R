## -- Detailed review of Family v2 model predictions

# start with fresh environment
rm(list = ls())

# load functions
source(paste0(getwd(), "/install_load.R"))
source(paste0(getwd(), "/Model_Results/group_labels.R"))
source(paste0(getwd(), "/Model_Results/threshold_results.R"))
source(paste0(getwd(), "/Model_Results/plot_df.R"))
source(paste0(getwd(), "/Model_Results/get_misses.R"))

# load libraries
install_load("dplyr", "stringr", "ComplexHeatmap", "circlize", "ggplot2", "ggsci", "gridtext")


## -- set output dir

output_dir <- "C:/Users/amira.burns/OneDrive - USDA/Projects/CameraTrapDetectoR/output/family_fasterRCNN_resnet_20230606_1122"

## -- load and format data
# load predictions
preds <- utils::read.csv(paste0(output_dir, "/pred_df_50epochs.csv"))

# load labels
targets <- utils::read.csv(paste0(output_dir, "/target_df.csv"))

# format label dictionary
group_labs <- group_labels()


# set label orders
class_order <- unique(group_labs$family)
pred_order <- c(unique(class_order[class_order %in% targets$class_name]), "empty")
target_order <- unique(class_order[class_order %in% targets$class_name])

# join fam names to targets
group_lab_join <- group_labs %>% select(!common.name.general) %>% distinct()
targets <- dplyr::left_join(targets, group_lab_join, by = join_by("class_name" == "family"))

# rename prediction col
pred_df <- dplyr::rename(preds, prediction = class_name)

# drop bboxes/counts for now and just look at classification
pred_df <- dplyr::select(pred_df, c(filename, prediction, confidence))
target_df <- targets %>% select(-c(X, bbox)) %>% distinct()

# Run the metrics function at different score_thresholds

results_full <- threshold_results(target_df, pred_df, 0, "family")
results_20 <- threshold_results(target_df, pred_df, 0.20, "family")
results_30 <- threshold_results(target_df, pred_df, 0.30, "family")
results_40 <- threshold_results(target_df, pred_df, 0.40, "family")
results_50 <- threshold_results(target_df, pred_df, 0.50, "family")
results_60 <- threshold_results(target_df, pred_df, 0.60, "family")
results_70 <- threshold_results(target_df, pred_df, 0.70, "family")
results_80 <- threshold_results(target_df, pred_df, 0.80, "family")
results_90 <- threshold_results(target_df, pred_df, 0.90, "family")

# set path to visualization directory
viz_path <- paste0(getwd(), "/Viz/")

# save confusion matrix as pdf
# Note: manually update score_threshold in name
pdf(file=paste0(viz_path, "family_v2_heatmap_", "0.9_score_threshold",".pdf"),
    width=16, height=14)
results_90$heatmap
dev.off()

# combine metrics for plotting
# get relevant df metrics for all thresholds
true_pos_df <- plot_df("true_pos_rate")
false_pos_df <- plot_df("false_pos_rate")
false_neg_df <- plot_df("false_neg_rate")

# combine metrics into one df
plot_df <- true_pos_df %>%
  dplyr::left_join(false_pos_df, by = dplyr::join_by(class, score_threshold)) %>%
  dplyr::left_join(false_neg_df, by = dplyr::join_by(class, score_threshold)) %>%
  tidyr::pivot_longer(true_pos_rate:false_neg_rate, names_to = "metric", values_to = "rate") %>%
  dplyr::left_join(group_lab_join, by = join_by(class == family)) %>%
  dplyr::select(!class.y) %>%
  dplyr::relocate(order, .before = class)

# get list of orders for looping plots
tax_order <- unique(plot_df$order)

# turn this into a loop function
plot_list <- list() 

for(i in 1:length(tax_order)){
  # filter df by family
  plotting_df <- dplyr::filter(plot_df, order == tax_order[i])
  
  # make plot
  p <- ggplot2::ggplot(plotting_df, aes(x = score_threshold, y = rate, colour = metric)) + 
    geom_point(size=2) + geom_line() + 
    facet_wrap(vars(class)) + 
    scale_x_continuous(breaks = seq(0, 1, 0.1)) + 
    scale_y_continuous(breaks = seq(0, 1, 0.1)) + 
    theme_bw() + 
    labs(title = paste0(tax_order[i], " Metrics by Family \nOver Score Threshold"),
         x = "Score Threshold", y = "Rate") + 
    theme(text = element_text(size = 14)) + 
    theme(plot.title = element_text(hjust = 0.5, size = 18)) + 
    ggsci::scale_color_startrek()
  
  # save plot to list
  plot_list[[i]] <- p
}

# save plots to pdf
pdf(file=paste0(viz_path, "family_classwise_metrics_by_score_threshold.pdf"),
    width=12, height=9)
for(i in 1:length(tax_order)){
  print(plot_list[[i]])
}
dev.off()

# Plot misclassifications

misses <- get_misses(model_type = "family")

# add color dict to group labels
order_factor <- factor(na.omit(unique(group_lab_join$order)))
color_factor <- paletteer::paletteer_d("ggsci::default_igv")
color_factor <- sample(color_factor, size = length(order_factor), replace = FALSE)
col_dict <- data.frame(pred_order = order_factor, color = color_factor)

# build out heatmap with example family
class_i <- "Aramidae"
df <- misses %>% filter(class_name == "Aramidae")

# pivot to wide format for heatmap
df_mat <-  df %>% 
  dplyr::select(-c(count, true_pos_rate, false_pos_rate, false_neg_rate)) %>% 
  tidyr::pivot_wider(names_from = score_threshold, names_prefix = "score_", values_from = prop) %>%
  dplyr::mutate(across(.cols = score_0.1:score_0.9, ~tidyr::replace_na(., 0))) %>%
  dplyr::mutate(across(.cols = score_0.1:score_0.9, ~round(.*100, 3)))

# sort df to standardized order 
df_mat <- df_mat[match(c(target_order, "empty"), df_mat$prediction),]
df_mat <- df_mat[complete.cases(df_mat),]

# join color scheme to df_mat
df_mat <- df_mat %>%
  dplyr::left_join(col_dict, by = join_by(prediction_order == pred_order))

# create matrix of proportion values
prop_mat <- as.matrix(df_mat[,7:15])
rownames(prop_mat) <- df_mat$prediction

# set taxonomic order as row splits, set col and row order
split_rows <- factor(df_mat$prediction_order, levels = unique(df_mat$prediction_order))
ordered_rows <- as.factor(df_mat$prediction)
ordered_cols <- colnames(prop_mat)

# create below-plot annotation for true pos rate
true_pos_sidebar <- df %>%
  dplyr::select(c(order, class_name, score_threshold, true_pos_rate)) %>%
  dplyr::distinct()
bottom_annotation = ComplexHeatmap::columnAnnotation(bar = ComplexHeatmap::anno_lines(c(true_pos_sidebar$true_pos_rate),
                                                                                      ylim=c(0, 1),
                                                                                      gp = gpar(fill = "#707091", border=NA, lty="blank"),
                                                                                      axis_param = list(
                                                                                        gp=gpar(fontsize=10),
                                                                                        at=c(0, 0.5, 1), 
                                                                                        labels=c(0, 0.5, 1))),
                                                     show_annotation_name=TRUE,
                                                     annotation_label = c("True Positive Rate"),
                                                     annotation_name_side ="right",
                                                     annotation_name_rot=0)

misses_heatmap <- ComplexHeatmap::Heatmap(prop_mat, name = "Proportion", rect_gp = gpar(type = "none"),
                                          cluster_columns=FALSE, cluster_rows=FALSE,
                                          column_names_side = "bottom", row_names_side = "left",
                                          column_title = gt_render(
                                            paste0("<span style='font-size:18pt; color:black'>Family Model V2 Misclassification Matrix</span>", 
                                                   "<br>Family = ", class_i)),
                                          #row_order = ordered_rows, column_order = ordered_cols,
                                          row_split = split_rows, row_title = "Misclassification",
                                          cell_fun = function(j, i, x, y, width, height, fill){
                                            grid.rect(x = x, y = y, width = width, height = height, 
                                                      gp = gpar(col = "grey", fill = NA))
                                            grid.circle(x = x, y = y, r = abs(prop_mat[i, j])/2 * min(unit.c(width, height)), 
                                                        gp = gpar(fill = df_mat$color[i], col = NA))
                                            
                                          }, show_heatmap_legend = FALSE,
                                          bottom_annotation = bottom_annotation)

pdf(file=paste0(viz_path, "family_v2_misclassification_", class_i, "_draft",".pdf"),
    width=16, height=14)
misses_heatmap
dev.off()



