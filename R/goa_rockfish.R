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
  
  q_catch(year, species=species, area=area, db=akfin)
  # commented functions are currently in development
  # q_obs(year, species=norpac_species, area=area, db=akfin)
  q_bts_biomass(year, area=area, species=afsc_species, by='total', db=akfin) 
   
  if(isTRUE(off_yr)) {
    disconnect(akfin) 
  } else {
    # q_fsh_specimen(year, species, area, db, print_sql=FALSE, save=TRUE)
    # q_fsh_length(year, species, area, db, print_sql=FALSE, save=TRUE)  
    disconnect(akfin)  
    
    afsc = connect("afsc")
    q_bts_specimen(year, species=afsc_species, area, db=afsc)
    q_bts_length(year, species=afsc_species, area, db=afsc)  
    disconnect(afsc)
  }
  
  # read in archived catch data
  afscdata::goa_nork_catch_1961_1992 %>%
    vroom::vroom_write(here::here(year, "data", "user_input", "goa_nork_catch_1961_1992.csv"))
  
  # timestamp
  afscdata:::q_date(year)
}

#' raw data query for GOA dusky rockfish
#'
#' @param year assessment year
#' @param off_yr if this is an off-year assessment change to TRUE
#'
#' @return a suite of raw data .csv files and a time stamp of when the query was done 
#' @export goa_dusk
#'
#' @examples
#' \dontrun{
#' goa_dusk(year = 2022, off_yr = FALSE)
#'}
goa_dusk <- function(year, off_yr = FALSE){
  # globals 
  area = "GOA"
  species = c("PEL7", "PELS", "DUSK")
  afsc_species = c(30150, 30152)
  norpac_species = 330
  
  akfin = connect()
  
  q_catch(year=year, species=species, area=area, db=akfin)
  # commented functions are currently in development
  # q_obs(year=year, species=norpac_species, area=area, db=akfin)
  q_bts_biomass(year=year, area=area, species=afsc_species, by='total', db=akfin) 
  
  if(isTRUE(off_yr)) {
    disconnect(akfin) 
  } else {
    # q_fsh_specimen(year=year, species=afsc_species, area=area, db=akfin)
    # q_fsh_length(year=year, species=afsc_species, area=area, db=akfin)  
    disconnect(akfin)  
    
    afsc = connect("afsc")
    q_bts_specimen(year=year, species=afsc_species, area=area, db=afsc)
    q_bts_length(year=year, species=afsc_species, area=area, db=afsc)  
    disconnect(afsc)
  }
  
  # read in archived catch data
  afscdata::goa_dusk_catch_1977_1990 %>%
    vroom::vroom_write(here::here(year, "data", "user_input", "goa_dusk_catch_1977_1990.csv"))
  
  # timestamp
  afscdata:::q_date(year)
}