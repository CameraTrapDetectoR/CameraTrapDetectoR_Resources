## -- Detailed review of Family v2 model predictions

# start with fresh environment
rm(list = ls())

# load functions
source(paste0(getwd(), "/install_load.R"))
source(paste0(getwd(), "/Model_Results/group_labels.R"))
source(paste0(getwd(), "/Model_Results/threshold_results.R"))
source(paste0(getwd(), "/Model_Results/plot_df.R"))
source(paste0(getwd(), "/Model_Results/get_misses.R"))
source(paste0(getwd(), "/Model_Results/misclassed_heatmap.R"))

# load libraries
install_load("dplyr", "stringr", "ComplexHeatmap", "circlize", "ggplot2", "ggsci", "gridtext")


## -- set output dir

output_dir <- "C:/Users/amira.burns/OneDrive - USDA/Projects/CameraTrapDetectoR/output/family_fasterRCNN_resnet_20230606_1122"

## -- load and format data
# load predictions
preds <- utils::read.csv(paste0(output_dir, "/pred_df_50epochs.csv"))
preds <- dplyr::mutate(preds,
                       class_name = ifelse(class_name == "vehicle", "Vehicle", 
                                           ifelse(class_name == "empty", "Empty", class_name)),
                       class_name = ifelse(class_name == "Hominidae", "Human", class_name))

# load labels
targets <- utils::read.csv(paste0(output_dir, "/target_df.csv"))
targets <- dplyr::mutate(targets,
                         class_name = ifelse(class_name == "vehicle", "Vehicle", class_name),
                         class_name = ifelse(class_name == "Hominidae", "Human", class_name))

# format label dictionary
group_labs <- group_labels()
group_labs <- group_labs[complete.cases(group_labs),]

# set label orders
class_order <- unique(group_labs$family)
pred_order <- c(unique(class_order[class_order %in% targets$class_name]), "Empty")
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
heatmaps <- list(results_full$heatmap,
                 results_20$heatmap,
                 results_30$heatmap,
                 results_40$heatmap,
                 results_50$heatmap,
                 results_60$heatmap,
                 results_70$heatmap,
                 results_80$heatmap,
                 results_90$heatmap)

# Note: manually update score_threshold in name
pdf(file=paste0(viz_path, "family_v2_heatmaps.pdf"),
    width=16, height=14)
for(i in 1:length(heatmaps)){
  print(heatmaps[[i]])
}
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

# Plot delta of each metric across classes to look for patterns

delta_df <- plot_df %>%
  group_by(metric, class) %>%
  mutate(rate = round(rate, 3)) %>%
  mutate(rate_delta = rate - lag(rate, default = rate[1]),
         metric = ifelse(metric == "true_pos_rate", "True Positive Rate",
                         ifelse(metric == "false_pos_rate", "False Positive Rate", "False Negative Rate")))

delta_plot <- ggplot(delta_df, aes(x = score_threshold, y = rate_delta, group = class)) + 
  geom_line(aes(col = order), lwd = 1.1, alpha = 0.7) + 
  scale_x_continuous(breaks = seq(0.1, 0.9, 0.1)) + 
  theme_bw() + 
  facet_wrap(vars(metric)) +
  scale_color_d3(palette = "category20") +
  labs(x = "Score Threshold", y = "Rate Change", title = "Change in Evaluation Rate by Score Threshold Across Family")

# save plots to pdf
pdf(file=paste0(viz_path, "familyv2_metric_deltas_by_score_threshold.pdf"),
    width=12, height=10)
delta_plot
dev.off()


## -- Track misclassifications

misses <- get_misses(model_type = "family")

# add color dict to group labels
order_factor <- factor(na.omit(unique(group_lab_join$order)))
color_factor <- paletteer::paletteer_d("ggsci::default_igv")
color_factor <- sample(color_factor, size = length(order_factor), replace = FALSE)
col_dict <- data.frame(pred_order = order_factor, col = color_factor)

# collect heatmaps
families <- unique(misses$class_name)
misses_heatmap <- list()

for(i in 1:length(families)){
  misses_heatmap[[i]] <- misclassed_heatmap(families[i], model_type = "family")
}


# --- END

pdf(file=paste0(viz_path, model_type='family', "_v2_misclassification_heatmap.pdf"),
    width=16, height=14)
for(i in 1:length(families)){
  draw(misses_heatmap[[i]]$heatmap, annotation_legend_list = misses_heatmap[[i]]$legend, annotation_legend_side = "bottom")
}
dev.off()



