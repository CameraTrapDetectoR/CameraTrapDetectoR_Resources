#' Combine Python Results
#' 
#' @description combine results run in Python with formatted df 
#' 
#' @import dplyr
#' 
#' @export
combine_results <- function(df_pred, df_incomplete){
  # join incompletes 
  
  df_rtd <- df_incomplete %>%
    dplyr::select(c(image_name, prediction, confidence, true_class, true_count, Comments))
  
  df_formatted <- format_python_results(df_pred)
  
  df_formatted <- df_formatted %>%
    dplyr::left_join(df_rtd, by = dplyr::join_by(image_name, prediction, confidence),
                     suffix = c(".x", "")) %>%
    dplyr::select(!true_class.x:Comments.x) %>%
    mutate_all(~replace(., is.na(.), ""))
  
  return(df_formatted)
}