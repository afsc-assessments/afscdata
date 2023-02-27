#' fishery observer data query
#'
#' @param year assessment year
#' @param fishery default is fsh, change if multiple fisheries
#' @param norpac_species norpac species code
#' @param area goa or bsai
#' @param db the database to query
#' @param save save the file in designated folder or output to global environment
#'
#' @export
#'
#' @examples
#' \dontrun{
#' q_fish_obs(2022, norpac_species = 303, area = 'GOA', db = akfin, save = TRUE)
#' }
#'
q_fish_obs <- function(year, fishery = "fsh", norpac_species, area, db, save = TRUE){
  
  obs = sql_read("fsh_obs.sql")
  obs = sql_filter(sql_precode = "", x = year-3, sql_code = obs, flag = "-- insert year")
  obs = sql_filter(sql_precode = "", x = year-1, sql_code = obs, flag = "-- year2")
  
  if(length(area) == 1){
    obs = sql_filter(x = area, sql_code = obs, flag = "-- insert region")
  } else {
    obs = sql_filter(sql_precode = "IN", x = area,
                      sql_code = obs, flag = "-- insert region")
  }
  
  if(length(norpac_species) == 1){
    obs = sql_filter(x = norpac_species, sql_code = obs, flag = "-- insert species")
  } else {
    obs = sql_filter(sql_precode = "IN", x = norpac_species,
                      sql_code = obs, flag = "-- insert species")
  }
  
  if(isTRUE(save)){
    sql_run(db, obs) %>%
      vroom::vroom(here::here(year, "data", "raw", paste0(fishery, "_obs_data.csv")), 
                   delim = ",")
  } else {
    sql_run(db, obs)
  }
  
  
}
