#' raw data query for GOA northern rockfish
#'
#' @param year assessment year
#' @param off_yr if this is an off-year assessment change to TRUE
#'
#' @return a suite of raw data .csv files and a time stamp of when the query was done 
#' @export goa_nork
#'
#' @examples
#' \dontrun{
#' goa_nork(year = 2022, off_yr = FALSE)
#'}
goa_nork <- function(year, off_yr = FALSE){
  # globals 
  area = "GOA"
  species = "NORK"
  afsc_species = 30420
  norpac_species = 303
  
  akfin = connect()
  
  if(isTRUE(off_yr)) {
  q_catch(year, species=species, area=area, db=akfin)
  # q_obs(year, species=species, area=area, db=akfin)
  q_bts_biomass(year, area=area, species=afsc_species, by='total', db=akfin) 
  disconnect(akfin)    
  
  } else {

    q_catch(year, species=species, area=area, db=akfin)
    # commented functions are currently in development
    # q_obs(year, species=species, area=area, db=akfin)
    q_bts_biomass(year, area=area, species=afsc_species, by='total', db=akfin) 
    # q_fsh_specimen(year, species, area, db, print_sql=FALSE, save=TRUE)
    # q_fsh_length(year, species, area, db, print_sql=FALSE, save=TRUE)  
    disconnect(akfin)  
    
    afsc = connect("afsc")
    q_bts_specimen(year, species, area, db, print_sql=FALSE, save=TRUE)
    q_bts_length(year, species, area, db, print_sql=FALSE, save=TRUE)  
    disconnect(afsc)
  }
  
  afscdata::goa_nork_catch_1961_1992 %>%
    vroom::vroom_write(here::here(year, "data", "user_input", "goa_nork_catch_1961_1992.csv"))
  
  q_date(year)
}