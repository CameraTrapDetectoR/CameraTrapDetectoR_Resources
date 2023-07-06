#' Plot df
#' 
#' @description join and format metrics for plotting
#' 
#' @export
#' 
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
