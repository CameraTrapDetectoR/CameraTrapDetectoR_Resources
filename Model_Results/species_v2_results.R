# Detailed review of Species v2 model predictions

# start fresh
rm(list = ls())

# load libraries
library(dplyr)
library(stringr)
library(ComplexHeatmap)
library(circlize)
library(ggplot2)
library(ggsci)

install_load("dplyr", "stringr", "ComplexHeatmap", "circlize", "ggplot2", "ggsci")

## -- set output dir

output_dir <- "C:/Users/amira.burns/OneDrive - USDA/Projects/CameraTrapDetectoR/output/fasterRCNN_resnet_20230221_1612"

## -- load and format data
# load predictions
preds <- utils::read.csv(paste0(output_dir, "/50_epochs/pred_df_50epochs.csv"))

# load labels
targets <- utils::read.csv(paste0(output_dir, "/target_df.csv"))

# load annotations to get family names
labels <- read.csv("C:/Users/amira.burns/OneDrive - USDA/Projects/CameraTrapDetectoR/labels/varified.bounding.boxes_for.training.final.2022-10-19.csv")
group_labs <- labels %>% 
  dplyr::select( c(common.name.general, genus, family, order, class)) %>%
  distinct()

# format common names in label dict
group_labs <- group_labs %>%
  mutate(common.name.general = ifelse(grepl("squirrel", group_labs$common.name.general, ignore.case = TRUE), "squirrel_spp", common.name.general)) %>%
  mutate(common.name.general = ifelse(grepl("dove", group_labs$common.name.general, ignore.case = TRUE), "dove_spp", common.name.general)) %>%
  mutate(family = ifelse(common.name.general == "Mouse_Rat", "Muridae", family)) %>%
  mutate(family = ifelse(common.name.general == "Owl", "Tytonidae_Strigidae", family)) %>%
  mutate(family = ifelse(common.name.general == "Heron", "Ardeidae_Aramidae", family)) %>%
  distinct()

# get species sort order from higher taxonomies
group_labs <- group_labs %>%
  arrange(class, order, family, genus) 
group_labs <- group_labs %>%
  dplyr::select(-genus) %>%
  dplyr::distinct()

class_order <- group_labs$common.name.general
pred_order <- c(unique(class_order[class_order %in% targets$class_name]), "empty")
target_order <- unique(class_order[class_order %in% targets$class_name])
group_labs[nrow(group_labs)+1,] <- rep("empty", ncol(group_labs))

# join fam names to targets
targets <- dplyr::left_join(targets, group_labs, by = join_by("class_name" == "common.name.general"))

# rename prediction col
pred_df <- dplyr::rename(preds, prediction = class_name)

# drop bboxes/counts for now and just look at classification
pred_df <- dplyr::select(pred_df, c(filename, prediction, confidence))
target_df <- targets %>% select(-c(X, file_id, bbox)) %>% distinct()

# load threshold results function
source(paste0(getwd(), "/Model_Results/threshold_results.R"))

# Run the metrics function at different score_thresholds

results_full <- threshold_results(target_df, pred_df, 0)
results_20 <- threshold_results(target_df, pred_df, 0.20)
results_30 <- threshold_results(target_df, pred_df, 0.30)
results_40 <- threshold_results(target_df, pred_df, 0.40)
results_50 <- threshold_results(target_df, pred_df, 0.50)
results_60 <- threshold_results(target_df, pred_df, 0.60)
results_70 <- threshold_results(target_df, pred_df, 0.70)
results_80 <- threshold_results(target_df, pred_df, 0.80)
results_90 <- threshold_results(target_df, pred_df, 0.90)

# set path to visualization directory
viz_path <- paste0(getwd(), "/Viz/")

# save confusion matrix as pdf
# Note: manually update score_threshold in name
pdf(file=paste0(viz_path, "species_v2_heatmap_", "0.9_score_threshold",".pdf"),
    width=16, height=14)
results_90$heatmap
dev.off()


##-- Plot class-wise true positive, false positive, false negative rates to look at potential associations between the metrics

plot_df <- function(plot_metric){
  # build dataframe from results
  df <- data.frame(class = results_full$metrics$class_name,
                   score_10 = results_full$metrics[, plot_metric],
                   score_20 = results_20$metrics[, plot_metric],
                   score_30 = results_30$metrics[, plot_metric],
                   score_40 = results_40$metrics[, plot_metric],
                   score_50 = results_50$metrics[, plot_metric],
                   score_60 = results_60$metrics[, plot_metric],
                   score_70 = results_70$metrics[, plot_metric],
                   score_80 = results_80$metrics[, plot_metric],
                   score_90 = results_90$metrics[, plot_metric])
  
  # pivot df to long format for plotting
  df <- df %>%
    tidyr::pivot_longer(!class, names_to = "score_threshold", values_to = plot_metric)
  
  # convert character score thresholds to numerics
  df <- df %>%
    dplyr::mutate(score_threshold = ifelse(score_threshold == "score_10", 0.1,
                                    ifelse(score_threshold == "score_20", 0.2, 
                                    ifelse(score_threshold == "score_30", 0.3,
                                    ifelse(score_threshold == "score_40", 0.4,
                                    ifelse(score_threshold == "score_50", 0.5,
                                    ifelse(score_threshold == "score_60", 0.6,
                                    ifelse(score_threshold == "score_70", 0.7,
                                    ifelse(score_threshold == "score_80", 0.8, 0.9)
                                    ))))))))
  
  return(df)
}

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
get_misses <- function(results) {
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
  misses <- df %>% 
    dplyr::filter(class_name != prediction) %>%
    dplyr::select(-total)
}

misses_full <- get_misses(results_full) %>% dplyr::mutate(score_threshold = 0.1)
misses_20 <- get_misses(results_20) %>% dplyr::mutate(score_threshold = 0.2)
misses_30 <- get_misses(results_30) %>% dplyr::mutate(score_threshold = 0.3)
misses_40 <- get_misses(results_40) %>% dplyr::mutate(score_threshold = 0.4)
misses_50 <- get_misses(results_50) %>% dplyr::mutate(score_threshold = 0.5)
misses_60 <- get_misses(results_60) %>% dplyr::mutate(score_threshold = 0.6)
misses_70 <- get_misses(results_70) %>% dplyr::mutate(score_threshold = 0.7)
misses_80 <- get_misses(results_80) %>% dplyr::mutate(score_threshold = 0.8)
misses_90 <- get_misses(results_90) %>% dplyr::mutate(score_threshold = 0.9)

# join dfs
misses <- dplyr::bind_rows(misses_full, misses_20, misses_30,
                           misses_40, misses_50, misses_60,
                           misses_70, misses_80, misses_90) %>%
  dplyr::rename(count = n)

# add prediction families, format as factors for plotting
pred_fams <- group_labs %>%
  dplyr::select(c(common.name.general, family)) %>%
  dplyr::rename(pred_family = family)

misses <- misses %>%
  dplyr::left_join(pred_fams, by = join_by(prediction == common.name.general)) %>%
  dplyr::relocate(pred_family, .after=prediction) %>%
  dplyr::mutate(prediction = factor(prediction, levels = rev(pred_order))) %>%
  dplyr::mutate(pred_family = as.factor(pred_family))

# add true pos rate to plot on secondary axis
misses <- misses %>%
  dplyr::left_join(true_pos_df, by = dplyr::join_by(class_name == class, score_threshold==score_threshold))


# create empty list to hold plots
misplots <- list()

##-- test misclass heatmap on single species

# set color palette by total families
fam_factor <- factor(na.omit(unique(pred_fams$pred_family)))
fam_colors <- unique(c(paletteer::paletteer_d("ggsci::springfield_simpsons"),
                paletteer::paletteer_d("ggsci::default_aaas"),
                paletteer::paletteer_d("ggsci::uniform_startrek"),
                paletteer::paletteer_d("ggsci::default_nejm"),
                paletteer::paletteer_d("ggsci::nrc_npg"),
                paletteer::paletteer_d("ggsci::default_jama")))
col_dict <- data.frame(family = fam_factor, col = fam_colors[1:length(fam_factor)])
                

# filter df to single species
df <- misses %>% 
  dplyr::filter(class_name == "Domestic_Cow") 

# pivot to wide format for heatmap
df_mat <-  df %>% 
  dplyr::select(-c(count, true_pos_rate)) %>% 
  tidyr::pivot_wider(names_from = score_threshold, names_prefix = "score_", values_from = prop) %>%
  dplyr::mutate(across(.cols = score_0.1:score_0.9, ~tidyr::replace_na(., 0))) %>%
  dplyr::mutate_at(5:13, round, 3)

# join color codes
df_mat <- df_mat %>%
  dplyr::left_join(col_dict, by = join_by("pred_family" == "family"))

# sort df to standardized order 
df_mat <- df_mat[match(c(target_order, "empty"), df_mat$prediction),]
df_mat <- df_mat[complete.cases(df_mat),]

# create matrix of proportion values
prop_mat <- as.matrix(df_mat[,5:13])
rownames(prop_mat) <- df_mat$prediction

# set family name as row splits, set col and row order
split_rows <- factor(df_mat$pred_family, levels = unique(df_mat$pred_family))
ordered_rows <- as.factor(df_mat$prediction)
ordered_cols <- colnames(prop_mat)

# set color scheme based on family
plot_cols <- dplyr::filter(col_dict, family %in% split_rows)

# create below-plot annotation for true pos rate
true_pos_sidebar <- df %>%
  dplyr::select(c(family, class_name, score_threshold, true_pos_rate)) %>%
  dplyr::distinct()
bottom_annotation = ComplexHeatmap::columnAnnotation(bar = ComplexHeatmap::anno_lines(c(true_pos_sidebar$true_pos_rate),
                                                                                        ylim=c(0, 1),
                                                                                        gp = gpar(fill = "#707370", border=NA, lty="blank"),
                                                                                        axis_param = list(
                                                                                          gp=gpar(fontsize=10),
                                                                                          at=c(0, 0.5, 1), 
                                                                                          labels=c(0, 0.5, 1))),
                                                     show_annotation_name=TRUE,
                                                     annotation_label = c("True Positive Rate"),
                                                     annotation_name_side ="left",
                                                     annotation_name_rot=0)
misses_heatmap <- ComplexHeatmap::Heatmap(prop_mat, name = "Proportion", rect_gp = gpar(type = "none"),
                                          cluster_columns=FALSE, cluster_rows=FALSE,
                                          column_names_side = "bottom", row_names_side = "left",
                                          #row_order = ordered_rows, column_order = ordered_cols,
                                          row_split = split_rows, row_title = "Misclassification",
                                          cell_fun = function(j, i, x, y, width, height, fill){
                                            grid.rect(x = x, y = y, width = width, height = height, 
                                                      gp = gpar(col = "grey", fill = NA))
                                            grid.circle(x = x, y = y, r = abs(prop_mat[i, j])/2 * min(unit.c(width, height)), 
                                                        gp = gpar(fill = df_mat$col[i], col = NA))
                                            
                                          }, show_heatmap_legend = FALSE,
                                          bottom_annotation = bottom_annotation)


for(i in 1:length(target_order)){
  # filter misses by true class
  df <- dplyr::filter(misses, class_name == target_order[i]) %>% 
    dplyr::group_by(pred_family)
  
  # create plot
  # NOTE: redo this using ComplexHeatmap with grid.circle
  plt <- ggplot(df, aes(x = prediction, y = score_threshold, color = pred_family)) + 
    geom_point(aes(size=prop), alpha = 0.7) + 
    scale_y_continuous(breaks = seq(0, 1, 0.1)) + 
    theme_bw() + ggsci::scale_color_igv(palette = "default") + 
    theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
    labs(color = "Prediction Family", size = "Proportion") + 
    ggtitle(paste0(target_order[i], " Misclassifications by Prediction Over Score Threshold"))
  
  # add to list
  misplots[[i]] <- plt
  
}

# save plots to pdf
pdf(file=paste0(viz_path, "misclassifications_by_species_score_threshold.pdf"),
    width=12, height=9)
for(i in 1:length(target_order)){
  print(misplots[[i]])
}
dev.off()

  