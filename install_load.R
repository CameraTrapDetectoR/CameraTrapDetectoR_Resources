#' Install and Load
#' 
#' @description load listed libraries; install any that are not already installed
#' 
#' @export
#' 
#' 
install_load <- function(...) {
  libs <- unlist(list(...))
  req <- unlist(lapply(libs, require, character.only = TRUE))
  need <- libs[req == FALSE]
  if(length(need) > 0) {
    install.packages(need)
    lapply(need, require, character.only = TRUE)
  }
}
