#' query fishery catch data
#'
#' @param year max year to retrieve data from 
#' @param species species group code e.g., "DUSK"
#' @param area sample area "GOA", "AI" or "BS" (or combos)
#' @param akfin data server to connect to
#' @param save saves a file to the data/raw folder, otherwise sends output to global enviro (default: TRUE)
#'
#' @return
#' @export
#'
#' @examples
q_catch <- function(year, species, area, akfin, save = TRUE) {
  
  area = toupper(area)
  
  species_switch(species, area)
  
  if(length(species) > 1){
    pre_species = "IN"
  }
  
  if(length(area) > 1){
    pre_area = "IN"
  }
  
  sql_read("fsh_catch.sql")
  
  
}