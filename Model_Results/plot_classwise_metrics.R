#' Get eval metrics for OOS data
#' 
#' @description plot classswise metrics by score threshold
#' 
#' @param df full eval df already pivoted to long format
#' @param sourcei character string wih name of source to evaluate; must match value in eval_df
#' @param modeli character string with model type to plot: takes values of: c("species", "family", "general")
#' 
#' @details
#' Provide formatted long data frame, models to plot
#' 
#' @returns a list of plots, faceted by metric, that show the classwise evaluation metrics for a single data source
#' 
#' @import dplyr
#' @import ggplot2
#' @import stringr
#' @import ggsci
#'
#' 
plot_classwise_metrics <- function(df, sourcei, modeli){
  
  # filter dataframe by source 
  source_df <- dplyr::filter(df, source_id == sourcei)
  
  m_df <- dplyr::filter(source_df, model == modeli)
    
    # filter df to species present in the dataset
  m_df <- dplyr::filter(m_df, true_count > 0)
    
  # build plot
  m_plot <- ggplot2::ggplot(m_df, aes(x = score_threshold, y = rate)) + 
    geom_line(aes(col = class_name), lwd=0.8) +
    facet_wrap(vars(Metric)) + 
    theme_bw() + 
    ggsci::scale_color_igv() + 
    scale_x_continuous(breaks = seq(0, 1, 0.2)) + 
    scale_y_continuous(breaks = seq(0, 1, 0.2)) + 
    labs(x = "Score Threshold", y = "Metric Rate", col = modeli,
         title = paste0(stringr::str_to_title(modeli), " Model Evaluation Metrics \nLocation = ", as.character(sourcei)))
  
  return(m_plot)  
  
}