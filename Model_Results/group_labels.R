#' Group Labels
#' 
#' load and format group labels
#' 
#' @import dplyr
#' 
#' @export
#' 
group_labels <- function(){
  
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
    mutate(class = ifelse(common.name.general == "Human", "Human", class)) %>%
    distinct()
  
  # change all higher order classifications for Vehicle and human
  group_labs[group_labs[,"common.name.general"] == "Vehicle", c("genus", "family", "order","class")] <- "Vehicle"
  group_labs[group_labs[,"common.name.general"] == "Human", c("genus", "family", "order","class")] <- "Human"
  
  # add row for strigidae family
  group_labs[nrow(group_labs)+1, ] <- c("Typical_owl", "Typical_Owl", "Strigidae", "Strigiformes", "Aves")
  
  # get species sort order from higher taxonomies
  group_labs <- group_labs %>%
    arrange(class, order, family, genus) 
  group_labs <- group_labs %>%
    dplyr::select(-genus) %>%
    dplyr::distinct()
  
  # update order for human
  group_labs <- group_labs %>% arrange(common.name.general == "Human", .before = "Vehicle")
  
  # add row for empties
  group_labs[nrow(group_labs)+1,] <- rep("Empty", ncol(group_labs))
  
  
  return(group_labs)
}