
#####################
#
# Run.All.CameraTrapDetector.Models.Parallel.R
#
# Wrapper function to run all three models for a set of images
#
# Written by: Ryan Miller
#
# Last updated: 31 Oct 2023
#
#####################




Run.All.CameraTrapDetector.Models.Parallel <- function(dir.names, latitude, longitude, overlap_threshold = 0.75, score_threshold = 0, num.cores){

#---- Set Options ----
options(timeout=1000)


#---- Load Libraries ----

library(torchvisionlib)
library(CameraTrapDetectoR)
library(progress)
library(operators)
library(foreach)
library(doParallel)

#---- END ----


#---- Run CameraTrapDetectoR by Directories ----

#--Loop over directories
pb <- progress_bar$new(
  format = "Running Camera Trap Detector [:bar] :percent eta: :eta",
  total = length(dir.names), clear = FALSE, width= 60)

for(i in 1:length(dir.names)){
  pb$tick()
  
  #--Set Image Directory
  img.path <- dir.names[i]
  
  #--Set Root Path for Results
  root.path <- dirname(dir.names[i])
  
  #--Set Camera Name  
  camera.name <- basename(dir.names[i])
  
  
  #--Determine which models need to be run
  models.to.run <- c('general','family','species')
  run.model <- vector()
  pred.files <- c("general_v2_model_predictions.csv","family_v2_model_predictions.csv","species_v2_model_predictions.csv")
  
  for(i in 1:length(pred.files)){
  
  if(file.exists(file.path(root.path, "Predictions",camera.name,pred.files[i]))==TRUE){
    chk.file <- read.csv(file.path(root.path, "Predictions",camera.name,pred.files[i]), stringsAsFactors=FALSE)
    
    if(length(unique(chk.file$filename))!=length(list.files(file.path(img.path), pattern=".JPG", recursive=TRUE))){
      run.model[i]<-TRUE
    }else(run.model[i]<-FALSE)
  }else(run.model[i]<-TRUE)
  
  }
  models.to.run <- models.to.run[run.model==TRUE]
  
  
  #--Check if any models need to be run
  if(length(models.to.run)>1){
  
    #--Provide user output
    print(paste0("Running ", paste(models.to.run, collapse = " "), " for ", camera.name, " ..."))
    
    #--Set up cores to use
    doParallel::registerDoParallel(num.cores)
    
    
    #--Run all models in parallel
    foreach::foreach(i=iter(models.to.run), .inorder=TRUE) %dopar% {
      
      CameraTrapDetectoR::deploy_model(
          data_dir = file.path(img.path),
          output_dir = file.path(root.path,"Predictions",camera.name),
          model_type = i,
          recursive = TRUE,
          file_extensions = c(".jpg", ".JPG"),
          make_plots = FALSE,
          plot_label = FALSE,
          sample50 = FALSE, 
          write_bbox_csv = TRUE, 
          overlap_correction = TRUE,
          overlap_threshold = overlap_threshold,
          score_threshold = score_threshold,
          latitude=latitude,
          longitude=longitude,
          h=307,
          w=408,
          lty=1,
          lwd=2, 
          col='red',
          write_metadata=FALSE
          #labeled = FALSE
        )
      
    }#END loop
    
    #--Close cluster
    stopImplicitCluster()
  
  }#END Logical
  
}#END Loop

}#END function

#---- END ----

