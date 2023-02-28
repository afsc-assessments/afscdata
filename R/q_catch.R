#' query fishery catch data
#'
#' @param year max year to retrieve data from 
#' @param species species group code e.g., "DUSK". If group_code is set to
#'   FALSE, input agency species code e.g., "145" = yelloweye rockfish
#' @param area FMP sample area "GOA" or "BSAI" (or both c("GOA", "BSAI"))
#' @param db data server to connect to
#' @param group_code defaults to TRUE
#' @param save saves a file to the data/raw folder, otherwise sends output to global enviro (default: TRUE)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' q_catch(year = 2022, species = 'NORK', area = 'GOA', db = akfin, save = TRUE)
#' }
q_catch <- function(year, species, area, db, group_code = TRUE, save = TRUE) {
  
  area = toupper(area)
  
  # species_switch(species, area)
  
  if(isTRUE(group_code)) {
    catch = sql_read("fsh_catch.sql")
  } else {
    catch = sql_read("fsh_catch_species_code.sql")
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
