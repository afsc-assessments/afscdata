#' query fishery catch data
#'
#' Note that this function will produce results that are confidential. They can be hidden from git/GitHub by adding fsh_catch_data.csv to your .gitignore file
#'
#' @param year max year to retrieve data from 
#' @param species species group code e.g., "DUSK"
#' @param area sample area "GOA", "AI" or "BS" (or combos)
#' @param db data server to connect to
#' @param use_afsc_code use 3 digit species code to filter instead of species group code (default: FALSE)
#' @param save saves a file to the data/raw folder, otherwise sends output to global enviro (default: TRUE)
#'
#' @return a fsh_catch_data.csv file to the data/raw folder
#' @export
#'
#' @examples
#' \dontrun{
#' # query northern rockfish in the goa and save the results in default location
#' q_catch(year = 2022, species = 'NORK', area = 'GOA', db = akfin, save = TRUE)
#' }
q_catch <- function(year, species, area, db, use_afsc_code = FALSE, save = TRUE) {
  
  area = toupper(area)
  
  # species_switch(species, area)
  
  catch = sql_read("fsh_catch.sql")
  if(isTRUE(use_afsc_code)) {
    stringr::str_replace(catch, "AND species_group_code", "AND agency_species_code")
  }
  catch = sql_filter(sql_precode = "<=", year, sql_code = catch, flag = "-- insert year")
  catch = sql_filter(x = area, sql_code = catch, flag = "-- insert area")
  catch = sql_filter(x = species, sql_code = catch, flag = "-- insert species")
  
  if(isTRUE(save)){
    sql_run(db, catch) %>%
      vroom::vroom(here::here(year, "data", "raw", "fsh_catch_data.csv"), 
                   delim = ",")
  } else {
    sql_run(db, catch)
  }
  
}
