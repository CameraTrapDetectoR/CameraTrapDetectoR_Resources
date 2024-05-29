# Build weighted confusion matrix from hyperparameter tuning


# set directory
oos_dir <- "path/to/oos_dir"

# define standard matrix col and row names
rows <- cols <- c("Common_Raccoon", "Common_Raven", "Mountain_Lion", "White-Tailed_Deer", 
                  "Wild_Pig", "Wild_Turkey", "background")

# define sample sizes
train_samps <- c(2000, 300, 2000, 2000, 2000, 2000, 0)
val_samps <- c(200, 100, 200, 200, 200, 200, 0)

## --- Build matrices for each train run

# write function to build matrix
build_confmat <- function(dat) {
  cf_mat <- matrix(data = dat, nrow = 7, ncol = 7, byrow = TRUE, dimnames = list(rows, cols))
  
  return(cf_mat)
}

# input data and define matrices
wbase <- build_confmat(c(0.74, 0, 0, 0, 0.01, 0.01, 0.39,
                         0, 0.90, 0, 0, 0, 0, 0,
                         0, 0, 0.94, 0.01, 0, 0, 0.07,
                         0, 0.01, 0, 0.92, 0, 0.02, 0.10,
                         0.24, 0, 0.05, 0.02, 0.97, 0.03, 0.38,
                         0, 0, 0, 0, 0, 0.89, 0.06,
                         0.02, 0.09, 0, 0.05, 0.01, 0.05, 0))
wbase_augs <- build_confmat(c(0.82, 0.01, 0, 0, 0.03, 0, 0.38,
                              0, 0.89, 0, 0, 0, 0, 0.01,
                              0, 0, 0.78, 0, 0, 0, 0.09,
                              0, 0, 0, 0.91, 0, 0.05, 0.05,
                              0.14, 0.03, 0.18, 0.03, 0.93, 0.04, 0.38,
                              0, 0, 0, 0, 0, 0.83, 0.08,
                              0.03, 0.07, 0.02, 0.05, 0.04, 0.08, 0))
wbase_augs_hg_0 <- build_confmat(c(0.87, 0.01, 0, 0, 0.05, 0.01, 0.45,
                                   0, 0.91, 0, 0, 0, 0, 0,
                                   0, 0, 0.97, 0.02, 0.02, 0.01, 0.11,
                                   0.01, 0, 0, 0.94, 0, 0, 0.15,
                                   0.11, 0.01, 0.02, 0.01, 0.89, 0, 0.22,
                                   0, 0, 0, 0, 0, 0.92, 0.07,
                                   0.02, 0.07, 0.01, 0.04, 0.03, 0.06, 0))
wbase_augs_hg_1 <- build_confmat(c(0.9, 0.01, 0.01, 0, 0.02, 0, 0.31,
                                   0, 0.93, 0, 0, 0, 0, 0,
                                   0.01, 0.01, 0.96, 0.01, 0.04, 0.01, 0.16,
                                   0.01, 0, 0.01, 0.95, 0, 0, 0.13,
                                   0.04, 0, 0, 0, 0.90, 0.90, 0.27,
                                   0, 0, 0, 0, 0, 0.92, 0.13,
                                   0.05, 0.05, 0, 0.03, 0.04, 0.05, 0))
wbase_augs_hg_4 <- build_confmat(c(0.90, 0.01, 0, 0.01, 0.08, 0.02, 0.32,
                                   0, 0.94, 0, 0, 0, 0, 0,
                                   0.03, 0.01, 0.99, 0.06, 0.11, 0.02, 0.30,
                                   0, 0, 0, 0.85, 0, 0, 0.05,
                                   0.01, 0, 0, 0, 0.72, 0, 0.3,
                                   0, 0, 0, 0, 0, 0.88, 0.03,
                                   0.06, 0.04, 0, 0.07, 0.08, 0.08, 0))
wbase_augs_hg_5 <- build_confmat(c(0.87, 0, 0, 0, 0.02, 0, 0.18,
                                   0, 0.92, 0, 0, 0, 0, 0, 
                                   0, 0.01, 0.87, 0, 0, 0, 0.24,
                                   0.01, 0, 0, 0.95, 0.01, 0.01, 0.06,
                                   0.04, 0, 0.05, 0, 0.89, 0, 0.47,
                                   0, 0, 0, 0, 0, 0.89, 0.06,
                                   0.08, 0.07, 0.07, 0.05, 0.07, 0.1, 0))
wbase_augs_hg_6 <- build_confmat(c(0.84, 0, 0, 0, 0.04, 0, 0.2,
                                   0, 0.9, 0, 0, 0, 0, 0,
                                   0, 0, 0.77, 0.01, 0, 0, 0.28,
                                   0, 0, 0.01, 0.90, 0.02, 0.01, 0.08,
                                   0.03, 0.01, 0.13, 0.01, 0.86, 0, 0.44,
                                   0, 0, 0, 0, 0, 0.9, 0,
                                   0.13, 0.09, 0.07, 0.07, 0.08, 0.09, 0))
wbase_augs_hg_7 <- build_confmat(c(0.75, 0, 0, 0, 0.03, 0, 0.16,
                                   0, 0.92, 0, 0, 0, 0, 0,
                                   0, 0, 0.82, 0, 0, 0, 0.44,
                                   0, 0, 0, 0.92, 0.01, 0, 0.08,
                                   0.08, 0, 0.10, 0.01, 0.89, 0, 0.32,
                                   0, 0, 0, 0, 0, 0.91, 0,
                                   0.17, 0.08, 0.08, 0.07, 0.05, 0.09, 0))
wbase_augs_hg_8 <- build_confmat(c(0.69, 0, 0, 0, 0.01, 0, 0.09,
                                   0, 0.9, 0, 0, 0, 0, 0.02,
                                   0.01, 0, 0.85, 0.01, 0.01, 0, 0.34,
                                   0, 0, 0, 0.88, 0.01, 0.01, 0.06,
                                   0.13, 0, 0.11, 0.02, 0.91, 0.01, 0.47,
                                   0, 0, 0, 0, 0, 0.89, 0.02,
                                   0.17, 0.10, 0.03, 0.09, 0.05, 0.09, 0))
wbase_hg_1 <- build_confmat(c(0.93, 0, 0, 0, 0.03, 0.02, 0.26,
                              0, 0.84, 0, 0, 0, 0, 0.05,
                              0, 0, 0.97, 0.01, 0, 0, 0.18,
                              0, 0.1, 0.1, 0.95, 0.01, 0, 0.11,
                              0.03, 0.03, 0.01, 0.01, 0.94, 0.01, 0.32,
                              0, 0.02, 0, 0, 0, 0.93, 0.08,
                              0.03, 0.10, 0, 0.03, 0.01, 0.04, 0))
wbase_hg_10 <- build_confmat(c(0.87, 0, 0.01, 0, 0.04, 0.01, 0.44,
                               0, 0.81, 0, 0, 0, 0, 0.02,
                               0.01, 0, 0.92, 0, 0, 0, 0.09,
                               0, 0, 0.02, 0.94, 0.02, 0.01, 0.09,
                               0.07, 0.01, 0.01, 0, 0.89, 0.01, 0.33,
                               0, 0, 0, 0, 0, 0.90, 0.02,
                               0.04, 0.18, 0.02, 0.05, 0.05, 0.07, 0))
wbase_hg_11 <- build_confmat(c(0.84, 0.01, 0.01, 0, 0.02, 0.01, 0.37,
                               0, 0.82, 0, 0, 0, 0, 0,
                               0.01, 0, 0.94, 0, 0.03, 0, 0.19,
                               0, 0, 0.02, 0.92, 0.01, 0, 0.12,
                               0.05, 0, 0.01, 0.01, 0.87, 0, 0.28,
                               0, 0.01, 0, 0, 0, 0.89, 0.03,
                               0.10, 0.16, 0.01, 0.07, 0.08, 0.09, 0))
wbase_hg_12 <- build_confmat(c(0.85, 0.01, 0, 0, 0, 0, 0.22,
                               0, 0.74, 0, 0, 0, 0, 0,
                               0, 0, 0.90, 0.01, 0.01, 0, 0.22,
                               0.01, 0, 0.01, 0.89, 0, 0, 0.15,
                               0.05, 0.01, 0.04, 0.02, 0.89, 0.02, 0.37,
                               0, 0, 0, 0, 0, 0.87, 0.04,
                               0.09, 0.24, 0.03, 0.07, 0.10, 0.11, 0))
wbase_hg_13 <- build_confmat(c(0.80, 0.01, 0, 0, 0.01, 0, 0.34,
                               0, 0.83, 0, 0, 0, 0, 0,
                               0, 0, 0.93, 0.01, 0, 0, 0.17,
                               0, 0, 0, 0.90, 0.01, 0, 0.17,
                               0.08, 0, 0.02, 0.02, 0.92, 0.01, 0.31,
                               0, 0, 0, 0, 0, 0.87, 0,
                               0.12, 0.16, 0.03, 0.07, 0.06, 0.13, 0))

mdbase_0 <- build_confmat(c(0.80, 0, 0, 0, 0, 0, 0.37,
                            0, 0.79, 0, 0, 0, 0, 0,
                            0, 0, 0.71, 0, 0, 0, 0.08,
                            0, 0, 0.06, 0.96, 0, 0.01, 0.10,
                            0.17, 0.10, 0.12, 0.01, 0.98, 0.03, 0.42,
                            0, 0, 0, 0, 0, 0.90, 0.04,
                            0.03, 0.11, 0.10, 0.02, 0.02, 0.06, 0))
mdbase_2 <- build_confmat(c(0.91, 0, 0, 0, 0, 0, 0.25,
                            0, 0.80, 0, 0, 0, 0, 0,
                            0, 0, 0.89, 0, 0, 0, 0.07,
                            0.01, 0, 0.05, 0.97, 0, 0, 0.32,
                            0.06, 0.08, 0.03, 0.01, 0.98, 0, 0.25,
                            0, 0.03, 0, 0, 0, 0.96, 0.11,
                            0.03, 0.09, 0.02, 0.01, 0.01, 0.04, 0))
mdbase_8 <- build_confmat(c(0.84, 0.01, 0, 0, 0.01, 0, 0.21,
                            0, 0.64, 0, 0, 0, 0, 0.02, 
                            0.01, 0, 0.83, 0, 0, 0, 0.14,
                            0.01, 0, 0.08, 0.95, 0.02, 0.02, 0.15,
                            0.10, 0.05, 0.05, 0, 0.95, 0, 0.40,
                            0, 0.12, 0, 0, 0, 0.93, 0.09,
                            0.04, 0.18, 0.03, 0.04, 0.01, 0.04, 0))



# average the matrices
md_tot <- mdbase_0 + mdbase_2 + mdbase_8
md_avg <- md_tot/3

w_tot <- wbase + wbase_augs + wbase_augs_hg_0 + wbase_augs_hg_1 + wbase_augs_hg_4 + wbase_augs_hg_5 + 
  wbase_augs_hg_6 + wbase_augs_hg_7 + wbase_augs_hg_8 + wbase_hg_1 + wbase_hg_10 + wbase_hg_11 + wbase_hg_12 + 
  wbase_hg_13
w_avg <- w_tot/14

cf_tot <- wbase + wbase_augs + wbase_augs_hg_0 + wbase_augs_hg_1 + wbase_augs_hg_4 + wbase_augs_hg_5 + 
  wbase_augs_hg_6 + wbase_augs_hg_7 + wbase_augs_hg_8 + wbase_hg_1 + wbase_hg_10 + wbase_hg_11 + wbase_hg_12 + 
  wbase_hg_13 + mdbase_0 + mdbase_2 + mdbase_8
cf_avg <- cf_tot/17



# plot the matrix
library(ComplexHeatmap)

val_confmat <- function(mat) {
  # set color range
  col_fun <- circlize::colorRamp2(c(0, 0.01, 0.5, 1), c("white", "#c5fff4", "#4fdbc2", "#0a6c5a"))
  
  hmap <- Heatmap(mat, name = "Proportion", cluster_columns=FALSE, cluster_rows=FALSE,
          column_names_side = "top", row_names_side = "left",
          row_title = "Predicted Species", col = col_fun,
          column_title = gt_render(
            paste0("<br><br>True Species"),
            r = unit(2, "mm"), padding = unit(c(2, 2, 2, 2), "pt")),
          cell_fun = function(j, i, x, y, width, height, fill) {
            grid.text(sprintf("%.2f", mat[i, j]), x, y, gp = gpar(fontsize = 12))
          })
  
  return(hmap)
}

val_confmat(md_avg)
val_confmat(w_avg)
val_confmat(cf_avg)

### -- OUT OF SAMPLE

# make dict of true species
oos_imgs <- list.files(oos_dir, pattern = ".jpg", recursive = T, ignore.case = T)
true_oos <- data.frame(filename = oos_imgs) %>%
  dplyr::mutate(TrueSpecies = stringr::str_split_i(filename, "/", 1),
                ImageID = stringr::str_split_i(filename, "/", 2)) %>%
  dplyr::mutate(ImageID = stringr::str_replace(ImageID, ".jpg", ".JPG")) %>%
  dplyr::mutate(ImageID = stringr::str_replace(ImageID, ".jpeg", ".JPG")) %>%
  dplyr::mutate(ImageID = stringr::str_replace(ImageID, ".JPEG", ".JPG")) %>%
  dplyr::mutate(TrueSpecies = stringr::str_replace(TrueSpecies, "background", "Empty")) %>%
  dplyr::filter(!stringr::str_detect(ImageID, "labels"))



# build function to extract label name, image name, and measure classification accuracy
format_labels <- function(df) {
  
  # extract image name
  df <- df %>%
    dplyr::mutate(ImageID = stringr::str_split_i(filename, "/", -1)) %>%
    dplyr::mutate(ImageID = stringr::str_replace(ImageID, ".txt", ".JPG")) %>%
    dplyr::filter(!stringr::str_detect(ImageID, "labels.csv"))
  
  # join true dict
  df <- df %>% 
    dplyr::left_join(true_oos, by = "ImageID") %>%
    dplyr::filter(!is.na(TrueSpecies)) %>%
    dplyr::mutate(ClassMatch = if_else(Prediction == TrueSpecies, 1, 0))
  
  # convert preds/truths into factors
  df <- df %>%
    dplyr::mutate(Prediction = factor(Prediction, levels = c("Common_Raccoon", "Common_Raven",
                                                             "Mountain_Lion", "White-Tailed_Deer", "Wild_Pig",
                                                             "Wild_Turkey", "Empty")),
                  TrueSpecies = factor(TrueSpecies, levels = c("Common_Raccoon", "Common_Raven",
                                                               "Mountain_Lion", "White-Tailed_Deer", "Wild_Pig",
                                                               "Wild_Turkey", "Empty")))
  
  # create normalized confusion matrix
  cmat <- caret::confusionMatrix(data = df$Prediction, reference = df$TrueSpecies)
  nmat <- as.matrix(cmat$table / rowSums(cmat$table))
  cmat$table.norm <- nmat
  
  return(cmat)
}

# read in out of sample labels
wbase_augs_preds <- read.csv(paste0(oos_dir, "results/", "wbase_augs_predictions.csv"))
wbase_augs_0_preds <- read.csv(paste0(oos_dir, "results/", "wbase_augs_hypgrid_0_predictions.csv"))
wbase_augs_1_preds <- read.csv(paste0(oos_dir, "results/", "wbase_augs_hypgrid_1_predictions.csv"))
wbase_augs_4_preds <- read.csv(paste0(oos_dir, "results/", "wbase_augs_hypgrid_4_predictions.csv"))
wbase_augs_5_preds <- read.csv(paste0(oos_dir, "results/", "wbase_augs_hypgrid_5_predictions.csv"))

wbase_5_preds <- read.csv(paste0(oos_dir, "results/", "wbase_hypgrid_5_predictions.csv"))
wbase_6_preds <- read.csv(paste0(oos_dir, "results/", "wbase_hypgrid_6_predictions.csv"))
wbase_8_preds <- read.csv(paste0(oos_dir, "results/", "wbase_hypgrid_8_predictions.csv"))
wbase_9_preds <- read.csv(paste0(oos_dir, "results/", "wbase_hypgrid_9_predictions.csv"))


# build oos matrices
oos_wbase <- format_labels(wbase_augs_preds)
oos_wbase_augs_0 <- format_labels(wbase_augs_0_preds)
oos_wbase_augs_1 <- format_labels(wbase_augs_1_preds)
oos_wbase_augs_4 <- format_labels(wbase_augs_4_preds)
oos_wbase_augs_5 <- format_labels(wbase_augs_5_preds)
oos_wbase_5 <- format_labels(wbase_5_preds)
oos_wbase_6 <- format_labels(wbase_6_preds)
oos_wbase_8 <- format_labels(wbase_8_preds)
oos_wbase_9 <- format_labels(wbase_9_preds)

# average the matrices
mtot <- oos_wbase$table.norm + oos_wbase_augs_0$table.norm + oos_wbase_augs_1$table.norm + 
  oos_wbase_augs_4$table.norm + oos_wbase_augs_5$table.norm + oos_wbase_5$table.norm + 
  oos_wbase_6$table.norm + oos_wbase_8$table.norm + oos_wbase_9$table.norm
  

mavg <- mtot/9

oos_confmat <- function(mat){
  
  # set color range
  oos_col_fun <- circlize::colorRamp2(c(0, 0.01, 0.5, 1), c("white", "#fcb4f6", "#c820ba", "#99068d"))
  
  # build heatmap
  hmap <- Heatmap(mat, name = "Proportion", cluster_columns=FALSE, cluster_rows=FALSE,
          column_names_side = "top", row_names_side = "left",
          row_title = "Predicted Species", col = oos_col_fun,
          column_title = gt_render(
            paste0("<br><br>True Species"),
            r = unit(2, "mm"), padding = unit(c(2, 2, 2, 2), "pt")),
          cell_fun = function(j, i, x, y, width, height, fill) {
            grid.text(sprintf("%.2f", mat[i, j]), x, y, gp = gpar(fontsize = 12))
          })
  
  return(hmap)
}

oos_confmat(mavg)

