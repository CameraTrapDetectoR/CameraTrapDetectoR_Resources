
#####################
#'
#' composite.empty.image.score
#'
#' Generate composite prediction of empty images
#' using general, family and species models. Returns
#' probability that image is empty (1 - confidence.score).
#' Function is best run on CameraTrapDetectoR results that
#' set the confidence threshold to 0 and either use the default
#' overlap threshold or set the overlap thershold to zero.
#'
#' Written By: Ryan Miller
#'
#' Last Updated: 31 Oct 2023
#'
#' @param predictions.path full file path to location containing predictions for general, family and species models
#' @param composite.score.threshold composite score threshold to use for labeling image as empty
#' @param model.count.threshold Minumum number of models identifying image as empty to consider empty.
#' If model.count.threshold=0 only composite.score.threshold will be used to label images as empty.
#' 
###################   

composite.empty.image.score <- function(predictions.path, composite.score.threshold=0.90, model.count.threshold=2){
  
  require(operators)
  
  #--Read predictions
  dat.gen <- read.csv(file.path(predictions.path,"general_v2_model_predictions.csv"))
  dat.fam <- read.csv(file.path(predictions.path,"family_v2_model_predictions.csv"))
  dat.spc <- read.csv(file.path(predictions.path,"species_v2_model_predictions.csv"))
  
  #--Add model flag
  dat.gen$model <- "general"
  dat.fam$model <- "family"
  dat.spc$model <- "species"
  
  
  #--Get empty images using all three models
  empty.img <- c(dat.gen[dat.gen$prediction=="Empty","filename"],
                 dat.fam[dat.fam$prediction=="Empty","filename"],
                 dat.spc[dat.spc$prediction=="Empty","filename"])
  
  empty.img <- unique(empty.img)
  
  empty.img <- rbind.data.frame(dat.gen[dat.gen$filename %in% empty.img,],
                                dat.fam[dat.fam$filename %in% empty.img,],
                                dat.spc[dat.spc$filename %in% empty.img,])
  
  #--Count number of models predicting empty
  cnt <- plyr::count(empty.img[empty.img$prediction=="Empty","filename"])
  colnames(cnt) <- c("filename","model.count")
  
  #--Generate composite confidence score
  score <- aggregate(confidence_score~filename, data=empty.img[empty.img$confidence_score!=1,], FUN=max)
  colnames(score) <- c("filename","composite.confidence")
  
  score[score$composite.confidence!=1, "composite.confidence"] = 1 - score[score$composite.confidence!=1, "composite.confidence"]
  
  #--Merge counts and scores
  score <- merge(cnt, score, by="filename", all.x=TRUE, all.y=TRUE)
  score[is.na(score$composite.confidence)==TRUE, "composite.confidence"] <- 1
  
  #--Add individual model scores
  tmp <- aggregate(confidence_score~filename+model, data=empty.img[empty.img$confidence_score!=1,], FUN=max)
  tmp$confidence_score <- 1 - tmp$confidence_score
  tmp <- tidyr::spread(tmp, key="model", value="confidence_score")
  tmp[is.na(tmp)==TRUE] <- 1
  
  #--Merge individual model results with composite score
  score <- merge(tmp, score, by="filename")
  
  #--Apply user constraints
  score[score$composite.confidence>=composite.score.threshold & score$model.count>=model.count.threshold,"composite.prediction"] <- "Empty"
  score[is.na(score$composite.prediction)==TRUE, "composite.prediction"] <- "Not Empty"
  
  #--Order by file name
  score <- score[order(score$filename),]
  
  #--Generate user feedback
  print(paste0("Percentage of empty images classified as empty: ", round(nrow(score[score$composite.prediction=="Empty",]) / nrow(score),3)*100, "%"))

  return(score)
}#END Function

#--END END
