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

# load labels
targets <- utils::read.csv(paste0(output_dir, "/target_df.csv"))

# format label dictionary
group_labs <- group_labels() 

# set label order
class_order <- group_labs$common.name.general
pred_order <- c(unique(class_order[class_order %in% targets$class_name]), "empty")
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

# set path to visualization directory
viz_path <- paste0(getwd(), "/Viz/")

# save confusion matrix as pdf
# Note: manually update score_threshold in name
pdf(file=paste0(viz_path, "species_v2_heatmap_", "0.9_score_threshold",".pdf"),
    width=16, height=14)
results_90$heatmap
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






  