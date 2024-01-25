#' Get df_wide for OOS data
#' 
#' @description get wide dataframe of preds and targets. 
#' 
#' @param df data frame with results and preds joined
#' @param models model type: takes one of c("species", "family", "general") values
#' @param confidence_threshold minimum confidence threshold for filtered predictions. Any predictions below this threshold will be changed to "Empty"
#' @details Helper function for oos_eval_metrics and oos_heatmap
#' 
#' @import dplyr
#' @import stringr
#' @import tidyr
#' 
get_df_wide <- function(df, models, confidence_threshold) {
  
  # define any global variables here
  alt_cats <- c("Human", "Vehicle", "Empty")
  
  if("species" %in% models){
    
    # isolate species preds
    sp_df <- df %>%
      dplyr::select(c(true_class, true_order, true_family, true_species, image_id, species_prediction, species_confidence))
    
    # filter out unknown birds
    sp_df <- sp_df %>%
      dplyr::filter(true_species != "bird_spp")
    
    # update predictions based on confidence threshold
    sp_df <- dplyr::mutate(sp_df, species_prediction = if_else(species_confidence < confidence_threshold, "Empty", species_prediction))
    sp_df <- dplyr::distinct(sp_df)
    
    # change classes to factors
    sp_df <- dplyr::mutate(sp_df, across(c(species_prediction, true_species), ~ as.factor(.)))
    
    # make ordered dict for targets
    tar_cl <- sp_df %>%
      dplyr::select(true_species) %>%
      dplyr::distinct() %>%
      dplyr::left_join(tax_dict, by = "true_species") %>%
      dplyr::arrange(true_class, true_order, true_family)
    alt_cl <- tar_cl %>%
      dplyr::filter(true_class %in% alt_cats) %>%
      dplyr::arrange(desc(true_class))
    tar_cl <- tar_cl %>%
      dplyr::filter(!(true_class %in% alt_cats)) %>%
      dplyr::bind_rows(alt_cl)
    
    # mark ordered dict for preds
    pred_cl <- sp_df %>%
      dplyr::select(species_prediction) %>%
      dplyr::distinct() %>%
      dplyr::left_join(tax_dict, by=join_by("species_prediction"=="true_species")) %>%
      dplyr::arrange(true_class, true_order, true_family) 
    alt_cl <- pred_cl %>%
      dplyr::filter(true_class %in% alt_cats) %>%
      dplyr::arrange(desc(true_class))
    pred_cl <- pred_cl %>%
      dplyr::filter(!(true_class %in% alt_cats)) %>%
      dplyr::bind_rows(alt_cl) %>%
      filter(if_any(everything(), ~ !is.na(.)))
    
    # get true counts for each target 
    true_ct <- sp_df %>%
      dplyr::select(c(image_id, true_species)) %>%
      dplyr::distinct() %>%
      dplyr::count(true_species) %>%
      dplyr::rename(true_count = n)
    
    # get counts of predictions for each target
    df_ct <- sp_df %>% 
      dplyr::select(c(image_id, true_class, true_order, true_family, true_species, species_prediction)) %>%
      dplyr::distinct() %>%
      dplyr::group_by(true_class, true_order, true_family, true_species) %>% 
      dplyr::count(species_prediction)
    
    # widen count df
    df_wide <- tidyr::pivot_wider(df_ct, names_from = species_prediction, values_from = n)
    
    # replace NA values with 0
    df_wide <- df_wide %>% mutate_all(~replace(., is.na(.), 0))
    
    # reorder columns and rows
    df_wide <- df_wide[match(tar_cl$true_species, df_wide$true_species), ]
    df_wide <- df_wide[, c("true_class", "true_order", "true_family", "true_species", unique(pred_cl$species_prediction))]

  }
  
  if("family" %in% models){
    
    # isolate family preds
    fm_df <- df %>%
      dplyr::select(c(true_class, true_order, true_family, image_id, family_prediction, family_confidence)) %>%
      dplyr::distinct()
    
    # update predictions based on confidence threshold
    fm_df <- dplyr::mutate(fm_df, family_prediction = if_else(family_confidence < confidence_threshold, "Empty", family_prediction))
    
    # change classes to factors
    fm_df <- dplyr::mutate(fm_df, across(c(family_prediction, true_family), ~ as.factor(.)))
    
    # make ordered dict for targets
    tar_cl <- fm_df %>%
      dplyr::select(c(true_family, true_order, true_class)) %>%
      dplyr::distinct() %>%
      dplyr::arrange(true_class, true_order, true_family)
    alt_cl <- tar_cl %>%
      dplyr::filter(true_class %in% alt_cats) %>%
      dplyr::arrange(desc(true_class))
    tar_cl <- tar_cl %>%
      dplyr::filter(!(true_class %in% alt_cats)) %>%
      dplyr::bind_rows(alt_cl)
    
    # mark ordered dict for preds
    pred_cl <- fm_df %>%
      dplyr::select(family_prediction) %>%
      dplyr::distinct() %>%
      dplyr::left_join(tax_dict, by=join_by("family_prediction"=="true_family")) %>%
      dplyr::select(c(true_class, true_order, family_prediction)) %>%
      dplyr::distinct() %>%
      dplyr::arrange(true_class, true_order, family_prediction) 
    alt_cl <- pred_cl %>%
      dplyr::filter(true_class %in% alt_cats) %>%
      dplyr::arrange(desc(true_class))
    pred_cl <- pred_cl %>%
      dplyr::filter(!(true_class %in% alt_cats)) %>%
      dplyr::bind_rows(alt_cl)
    
    # get true counts for each target 
    true_ct <- fm_df %>%
      dplyr::select(c(image_id, true_family)) %>%
      dplyr::distinct() %>%
      dplyr::count(true_family) %>%
      dplyr::rename(true_count = n)
    
    # get counts of predictions for each target
    df_ct <- fm_df %>% 
      dplyr::select(c(image_id, true_class, true_order, true_family, family_prediction)) %>%
      dplyr::distinct() %>%
      dplyr::group_by(true_class, true_order, true_family) %>% 
      dplyr::count(family_prediction)
    
    # widen count df
    df_wide <- tidyr::pivot_wider(df_ct, names_from = family_prediction, values_from = n) 
    
    # replace NA values with 0
    df_wide <- df_wide %>% mutate_all(~replace(., is.na(.), 0))
    
    # reorder columns and rows
    df_wide <- df_wide[match(tar_cl$true_family, df_wide$true_family), ]
    df_wide <- df_wide[, c("true_class", "true_order","true_family", unique(pred_cl$family_prediction))]

  }
  
  
  if("general" %in% models){
    
    # isolate general preds
    gn_df <- df %>%
      dplyr::select(c(true_class, image_id, general_prediction, general_confidence)) %>%
      dplyr::distinct()
    
    # update predictions based on confidence threshold
    gn_df <- dplyr::mutate(gn_df, general_prediction = if_else(general_confidence < confidence_threshold, "Empty", general_prediction))
    
    # change classes to factors
    gn_df <- dplyr::mutate(gn_df, across(c(general_prediction, true_class), ~ as.factor(.)))
    
    # make ordered dict for targets
    tar_cl <- gn_df %>%
      dplyr::select(true_class) %>%
      dplyr::distinct() %>%
      dplyr::arrange(true_class)
    alt_cl <- tar_cl %>%
      dplyr::filter(true_class %in% alt_cats) %>%
      dplyr::arrange(desc(true_class))
    tar_cl <- tar_cl %>%
      dplyr::filter(!(true_class %in% alt_cats)) %>%
      dplyr::bind_rows(alt_cl)
    
    # mark ordered dict for preds
    pred_cl <- gn_df %>%
      dplyr::select(general_prediction) %>%
      dplyr::distinct() %>%
      dplyr::arrange(general_prediction) 
    alt_cl <- pred_cl %>%
      dplyr::filter(general_prediction %in% alt_cats) %>%
      dplyr::arrange(desc(general_prediction))
    pred_cl <- pred_cl %>%
      dplyr::filter(!(general_prediction %in% alt_cats)) %>%
      dplyr::bind_rows(alt_cl)
    
    # get true counts for each target 
    true_ct <- gn_df %>%
      dplyr::select(c(image_id, true_class)) %>%
      dplyr::distinct() %>%
      dplyr::count(true_class) %>%
      dplyr::rename(true_count = n)
    
    # get counts of predictions for each target
    df_ct <- gn_df %>% 
      dplyr::select(c(true_class, general_prediction, image_id)) %>%
      dplyr::distinct() %>%
      dplyr::group_by(true_class) %>% 
      dplyr::count(general_prediction)
    
    # widen count df
    df_wide <- tidyr::pivot_wider(df_ct, names_from = general_prediction, values_from = n)
    
    # replace NA values with 0
    df_wide <- df_wide %>% mutate_all(~replace(., is.na(.), 0))
    
    # reorder columns and rows
    df_wide <- df_wide[match(tar_cl$true_class, df_wide$true_class), ]
    df_wide <- df_wide[, c("true_class", as.character(pred_cl$general_prediction))]
  }
  
  return(df_wide)
} #END