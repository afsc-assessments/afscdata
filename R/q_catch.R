#' query fishery catch data
#'
#' @param year max year to retrieve data from 
#' @param species species group code e.g., "DUSK"
#' @param area sample area "GOA", "AI" or "BS" (or combos)
#' @param db data server to connect to
#' @param save saves a file to the data/raw folder, otherwise sends output to global enviro (default: TRUE)
#'
#' @export
#'
#' @examples
#' q_catch(year = 2022, species = 'NORK', area = 'GOA', db = akfin, save = TRUE)
q_catch <- function(year, species, area, db, save = TRUE) {
  
  area = toupper(area)
  
  species_switch(species, area)
  
  sql = sql_read("fsh_catch.sql")
  sql = sql_filter(sql_precode = "<=", year, sql_code = sql, flag = "-- insert year")
  sql = sql_filter(sql_precode = "IN", area, sql_code = sql, flag = "-- insert region")
  sql = sql_filter(sql_precode = "IN", species, sql_code = sql, flag = "-- insert species")
  
  if(isTRUE(save)){
    sql_run(db, sql) %>%
      vroom::vroom(here::here(year, "data", "raw", "fish_catch_data.csv"), delim = ",")
  } else {
    sql_run(db, sql)
  }
  
}