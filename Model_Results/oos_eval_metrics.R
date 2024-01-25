#' Get eval metrics for OOS data
#' 
#' @description calculate classwise eval metrics for out of sample data
#' 
#' @param df data frame with results and preds joined
#' @param models model types to get metrics for: takes c("species", "family", "general") values
#' @param confidence_threshold minimum confidence threshold for filtered predictions. Any predictions below this threshold will be changed to "Empty"
#' @param source_id character string identifier for data source / location
#' @details
#' Provide formatted data frame, models to run, and confidence threshold
#' 
#' @import dplyr
#' @import stringr
#' @import tidyr

oos_eval_metrics <- function(df, models, confidence_threshold, source_id) {
  ## Eval each model separately
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
    
    # get total row and col counts
    N <- ncol(df_wide)
    mat <- as.matrix(df_wide[,5:N])
    pred_counts <- colSums(mat)
    
    # get metrics for species in the dataset
    tar_mets <- data.frame(class_name = df_wide$true_species,
                          TP = 0, TP_rate = 0, FN = 0, FN_rate = 0,
                          FP = 0, FP_rate = 0)
    tar_mets <- dplyr::left_join(tar_mets, true_ct, 
                                 by = dplyr::join_by("class_name"=="true_species"))
    tar_mets <- dplyr::relocate(tar_mets,
                                true_count, .after=class_name)
    # since we don't have a square matrix, loop through truths 
    for(i in 1:nrow(tar_mets)){
      sp_name <- as.character(tar_mets$class_name[i])
      if(sp_name %in% colnames(df_wide)){
        # true positives
        tar_mets$TP[i] = as.numeric(df_wide[df_wide$true_species==sp_name, sp_name])
        tar_mets$TP_rate[i] = round(tar_mets$TP[i] / tar_mets$true_count[i], 3)
        
        # false negatives
        tar_mets$FN[i] = tar_mets$true_count[i] - tar_mets$TP[i]
        tar_mets$FN_rate[i] = round(tar_mets$FN[i] / tar_mets$true_count[i], 3)
      }
    }
    
    # turn pred counts into df
    pre_mets <- as.data.frame(pred_counts)
    pre_mets <- tibble::rownames_to_column(pre_mets, var = "species_prediction")
    
    # join pred counts to target metrics
    mets <- dplyr::full_join(tar_mets, pre_mets, by = dplyr::join_by("class_name"=="species_prediction"))
    mets <- dplyr::relocate(mets, pred_counts, .after=true_count)
    mets <- dplyr::mutate_all(mets, ~replace(., is.na(.), 0))
    
    # calculate FP values
    mets <- dplyr::mutate(mets, FP = pred_counts - TP)
    mets <- dplyr::mutate(mets, FP_rate = round(if_else(pred_counts > 0, FP / pred_counts, 0), 3))
    
    # create model-specific df with model type and score threshold listed
    sp_mets <- dplyr::mutate(mets, model = "species", score_threshold = confidence_threshold)
  } else {sp_mets <- data.frame()}
  
  if("family" %in% models){
    
    # filter out unknown birds
    fm_df <- df %>%
      dplyr::filter(true_species != "bird_spp")
    
    # isolate family preds
    fm_df <- fm_df %>%
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
    
    # get total row and col counts
    N <- ncol(df_wide)
    mat <- as.matrix(df_wide[,4:N])
    pred_counts <- colSums(mat)
    
    # get metrics for families in the dataset
    tar_mets <- data.frame(class_name = df_wide$true_family,
                           TP = 0, TP_rate = 0, FN = 0, FN_rate = 0,
                           FP = 0, FP_rate = 0)
    tar_mets <- dplyr::left_join(tar_mets, true_ct, 
                                 by = dplyr::join_by("class_name"=="true_family"))
    tar_mets <- dplyr::relocate(tar_mets,
                                true_count, .after=class_name)
    tar_mets <- dplyr::filter(tar_mets, !is.na(class_name))
    
    # since we don't have a square matrix, loop through truths 
    for(i in 1:nrow(tar_mets)){
      fm_name <- as.character(tar_mets$class_name[i])
      if(fm_name %in% colnames(df_wide)){
        # true positives
        tar_mets$TP[i] = as.numeric(df_wide[df_wide$true_family==fm_name, fm_name])
        tar_mets$TP_rate[i] = round(tar_mets$TP[i] / tar_mets$true_count[i], 3)
        
        # false negatives
        tar_mets$FN[i] = tar_mets$true_count[i] - tar_mets$TP[i]
        tar_mets$FN_rate[i] = round(tar_mets$FN[i] / tar_mets$true_count[i], 3)
      }
    }
    
    # turn pred counts into df
    pre_mets <- as.data.frame(pred_counts)
    pre_mets <- tibble::rownames_to_column(pre_mets, var = "family_prediction")
    
    # join pred counts to target metrics
    mets <- dplyr::full_join(tar_mets, pre_mets, by = dplyr::join_by("class_name"=="family_prediction"))
    mets <- dplyr::relocate(mets, pred_counts, .after=true_count)
    mets <- dplyr::mutate_all(mets, ~replace(., is.na(.), 0))
    
    # calculate FP values
    mets <- dplyr::mutate(mets, FP = pred_counts - TP)
    mets <- dplyr::mutate(mets, FP_rate = round(if_else(pred_counts > 0, FP / pred_counts, 0), 3))
    
    # create model-specific df with model type and score threshold listed
    fm_mets <- dplyr::mutate(mets, model = "family", score_threshold = confidence_threshold)
  } else {fm_mets <- data.frame()}
  
  
  if("general" %in% models){
    
    # isolate species preds
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
    
    # get total row and col counts
    N <- ncol(df_wide)
    mat <- as.matrix(df_wide[,2:N])
    pred_counts <- colSums(mat)
    
    # get metrics for species in the dataset
    tar_mets <- data.frame(class_name = df_wide$true_class,
                           TP = 0, TP_rate = 0, FN = 0, FN_rate = 0,
                           FP = 0, FP_rate = 0)
    tar_mets <- dplyr::left_join(tar_mets, true_ct, 
                                 by = dplyr::join_by("class_name"=="true_class"))
    tar_mets <- dplyr::relocate(tar_mets,
                                true_count, .after=class_name)
    # since we don't have a square matrix, loop through truths 
    for(i in 1:nrow(tar_mets)){
      gn_name <- as.character(tar_mets$class_name[i])
      if(gn_name %in% colnames(df_wide)){
        # true positives
        tar_mets$TP[i] = as.numeric(df_wide[df_wide$true_class==gn_name, gn_name])
        tar_mets$TP_rate[i] = round(tar_mets$TP[i] / tar_mets$true_count[i], 3)
        
        # false negatives
        tar_mets$FN[i] = tar_mets$true_count[i] - tar_mets$TP[i]
        tar_mets$FN_rate[i] = round(tar_mets$FN[i] / tar_mets$true_count[i], 3)
      }
    }
    
    # turn pred counts into df
    pre_mets <- as.data.frame(pred_counts)
    pre_mets <- tibble::rownames_to_column(pre_mets, var = "general_prediction")
    
    # join pred counts to target metrics
    mets <- dplyr::full_join(tar_mets, pre_mets, by = dplyr::join_by("class_name"=="general_prediction"))
    mets <- dplyr::relocate(mets, pred_counts, .after=true_count)
    mets <- dplyr::mutate_all(mets, ~replace(., is.na(.), 0))
    
    # calculate FP values
    mets <- dplyr::mutate(mets, FP = pred_counts - TP)
    mets <- dplyr::mutate(mets, FP_rate = round(if_else(pred_counts > 0, FP / pred_counts, 0), 3))
    
    # create model-specific df with model type and score threshold listed
    gn_mets <- dplyr::mutate(mets, model = "general", score_threshold = confidence_threshold)
  } else {gn_mets <- data.frame()}
  
  # combine into single df
  eval_df <- dplyr::bind_rows(sp_mets, fm_mets, gn_mets)
  
  # add col for source id
  eval_df <- dplyr::mutate(eval_df, source_id = source_id)
  

  return(eval_df)
}
#END