# Detailed review of Species v2 model predictions

# start fresh
rm(list = ls())

# load functions
source(paste0(getwd(), "/install_load.R"))
source(paste0(getwd(), "/Model_Results/group_labels.R"))
source(paste0(getwd(), "/Model_Results/threshold_results.R"))
source(paste0(getwd(), "/Model_Results/plot_df.R"))
source(paste0(getwd(), "/Model_Results/get_misses.R"))
source(paste0(getwd(), "/Model_Results/misclassed_heatmap.R"))

# load libraries
install_load("dplyr", "stringr", "ComplexHeatmap", "circlize", "ggplot2", "ggsci")

## -- set output dir

output_dir <- "C:/Users/amira.burns/OneDrive - USDA/Projects/CameraTrapDetectoR/output/fasterRCNN_resnet_20230221_1612"

## -- load and format data
# load predictions
preds <- utils::read.csv(paste0(output_dir, "/50epochs/pred_df_50epochs.csv"))
preds <- dplyr::mutate(preds,
                       class_name = ifelse(class_name == "vehicle", "Vehicle", 
                                           ifelse(class_name == "empty", "Empty", class_name)))

# load labels
targets <- utils::read.csv(paste0(output_dir, "/target_df.csv"))
targets <- dplyr::mutate(targets,
                         class_name = ifelse(class_name == "vehicle", "Vehicle", class_name))

# format label dictionary
group_labs <- group_labels() 

# set label order
class_order <- group_labs$common.name.general
pred_order <- c(unique(class_order[class_order %in% targets$class_name]), "Empty")
target_order <- unique(class_order[class_order %in% targets$class_name])

# join fam names to targets
targets <- dplyr::left_join(targets, group_labs, by = join_by("class_name" == "common.name.general"))

# rename prediction col
pred_df <- dplyr::rename(preds, prediction = class_name)

# drop bboxes/counts for now and just look at classification
pred_df <- dplyr::select(pred_df, c(filename, prediction, confidence))
target_df <- targets %>% select(-c(X, file_id, bbox)) %>% distinct()

# Run the metrics function at different score_thresholds
results_full <- threshold_results(target_df, pred_df, 0, "species")
results_20 <- threshold_results(target_df, pred_df, 0.20, "species")
results_30 <- threshold_results(target_df, pred_df, 0.30, "species")
results_40 <- threshold_results(target_df, pred_df, 0.40, "species")
results_50 <- threshold_results(target_df, pred_df, 0.50, "species")
results_60 <- threshold_results(target_df, pred_df, 0.60, "species")
results_70 <- threshold_results(target_df, pred_df, 0.70, "species")
results_80 <- threshold_results(target_df, pred_df, 0.80, "species")
results_90 <- threshold_results(target_df, pred_df, 0.90, "species")

# run loop of smaller increments of score threshold 
finer_metrics <- list()
finer_cts <- list()
for(i in seq(0.4, 0.90, 0.01)){
  results <- threshold_results(target_df, pred_df, i, "species")
  finer_metrics[i] <- results$metrics
}

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
pdf(file=paste0(viz_path, "species_v2_heatmaps.pdf"),
    width=16, height=14)
for(i in 1:length(heatmaps)){
  print(heatmaps[[i]])
}
dev.off()


##-- Plot class-wise true positive, false positive, false negative rates to look at potential associations between the metrics


# get relevant df metrics for all thresholds
true_pos_df <- plot_df("true_pos_rate")
false_pos_df <- plot_df("false_pos_rate")
false_neg_df <- plot_df("false_neg_rate")

# combine metrics into one df
plot_df <- true_pos_df %>%
  dplyr::left_join(false_pos_df, by = dplyr::join_by(class, score_threshold)) %>%
  dplyr::left_join(false_neg_df, by = dplyr::join_by(class, score_threshold)) %>%
  tidyr::pivot_longer(true_pos_rate:false_neg_rate, names_to = "metric", values_to = "rate") %>%
  dplyr::left_join(group_labs, by = join_by(class == common.name.general)) %>%
  dplyr::select(-c(order, class.y)) %>%
  dplyr::relocate(family, .before = class)

# get list of families for looping plots
families <- unique(plot_df$family)

# turn this into a loop function
plot_list <- list() 

for(i in 1:length(families)){
  # filter df by family
  plotting_df <- dplyr::filter(plot_df, family == families[i])
  
  # make plot
  p <- ggplot2::ggplot(plotting_df, aes(x = score_threshold, y = rate, colour = metric)) + 
            geom_point() + geom_line() + 
            facet_wrap(vars(class)) + 
            scale_x_continuous(breaks = seq(0, 1, 0.1)) + 
            scale_y_continuous(breaks = seq(0, 1, 0.1)) + 
            theme_bw() + 
            ggtitle(paste0(families[i], " Metrics by Species Over Score Threshold")) + 
            theme(text = element_text(size = 14)) + 
            theme(plot.title = element_text(hjust = 0.5, size = 18)) + 
            ggsci::scale_color_startrek()
  
  # save plot to list
  plot_list[[i]] <- p
}

# save plots to pdf
pdf(file=paste0(viz_path, "classwise_metrics_by_score_threshold.pdf"),
        width=12, height=9)
for(i in 1:length(families)){
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
  geom_line(aes(col = family), lwd = 1.1, alpha = 0.7) + 
  scale_x_continuous(breaks = seq(0.1, 0.9, 0.1)) + 
  theme_bw() + 
  facet_wrap(vars(metric)) +
  scale_color_igv() +
  labs(x = "Score Threshold", y = "Rate Cahnge", title = "Change in Evaluation Rate by Score Threshold Across Species")

# save plots to pdf
pdf(file=paste0(viz_path, "speciesv2_metric_deltas_by_score_threshold.pdf"),
    width=12, height=10)
delta_plot
dev.off()


## -- track misclassifications

# format misses df
misses <- get_misses(model_type = "species")


##-- create misclassification heatmaps

# set color palette by total families
fam_factor <- factor(na.omit(unique(group_labs$family)))
color_factor <- unique(c(paletteer::paletteer_d("ggsci::springfield_simpsons"),
                paletteer::paletteer_d("ggsci::default_aaas"),
                paletteer::paletteer_d("ggsci::uniform_startrek"),
                paletteer::paletteer_d("ggsci::default_nejm"),
                paletteer::paletteer_d("ggsci::nrc_npg"),
                paletteer::paletteer_d("ggsci::default_jama")))
col_dict <- data.frame(family = fam_factor, col = sample(color_factor, size = length(fam_factor), replace = FALSE))
                
misclassed_plots <- list()

for(i in 1:length(target_order)){
  misclassed_plots[[i]] <- misclassed_heatmap(class_i = target_order[i], model_type = "species")
}
  
pdf(file=paste0(viz_path, model_type, "_v2_misclassification_heatmap.pdf"),
    width=16, height=14)
for(i in 1:length(target_order)){
  draw(misclassed_plots[[i]]$heatmap, annotation_legend_list = misclassed_plots[[i]]$legend, annotation_legend_side = "bottom")
}
dev.off()




  