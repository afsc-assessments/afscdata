#' raw data query for GOA flathead sole
#'
#' @param year assessment year
#' @param off_yr if this is an off-year assessment change to TRUE
#'
#' @return a suite of raw data .csv files and a time stamp of when the query was done 
#' @export goa_fhs
#'
#' @examples
#' \dontrun{
#' goa_fhs(year = 2022, off_yr = FALSE)
#'}
goa_fhs <- function(year, off_yr = FALSE){
  # globals 
  area = "GOA"
  species = "FSOL"
  afsc_species = 10130
  norpac_species = 103
  
  akfin = connect()
  
  q_catch(year, species=species, area=area, db=akfin)
  q_fish_obs(year, species=norpac_species, area=area, db=akfin)
  q_bts_biomass(year, area=area, species=afsc_species, type='total', db=akfin) 
  
  if(isTRUE(off_yr)) {
    disconnect(akfin) 
  } else {
    q_fsh_specimen(year, species, area, db=akfin, print_sql=FALSE, save=TRUE)
    q_fsh_length(year, species, area, db=akfin, print_sql=FALSE, save=TRUE)  
    disconnect(akfin)  
    
    afsc = connect("afsc")
    q_bts_specimen(year, species=afsc_species, area, db=afsc)
    q_bts_length(year, species=afsc_species, area, db=afsc)  
    disconnect(afsc)
  }
  
  # read in archived catch data
  afscdata::goa_fhs_catch_1978_1990 %>%
    vroom::vroom_write(here::here(year, "data", "user_input", "goa_fhs_catch_1978_1990.csv"))
  
  # timestamp
  q_date(year)
}

#' raw data query for GOA arrowtooth founder
#' 
#' @param year assessment year
#' @param off_yr if this is an off-year assessment change to TRUE
#'
#' @return a suite of raw data .csv files and a time stamp of when the query was done 
#' @export goa_atf
#'
#' @examples
#' \dontrun{
#' goa_atf(year = 2022, off_yr = FALSE)
#'}
goa_atf <- function(year, off_yr = FALSE){
  # globals 
  area = "GOA"
  species = "ARTH"
  afsc_species = 10110
  norpac_species = 103
  
  akfin = connect()
  
  q_catch(year, species=species, area=area, db=akfin)
  q_fish_obs(year, species=norpac_species, area=area, db=akfin)
  q_bts_biomass(year, area=area, species=afsc_species, type='total', db=akfin) 
  
  if(isTRUE(off_yr)) {
    disconnect(akfin) 
  } else {
    q_fsh_specimen(year, species, area, db=akfin, print_sql=FALSE, save=TRUE)
    q_fsh_length(year, species, area, db=akfin, print_sql=FALSE, save=TRUE)  
    disconnect(akfin)  
    
    afsc = connect("afsc")
    q_bts_specimen(year, species=afsc_species, area, db=afsc)
    q_bts_length(year, species=afsc_species, area, db=afsc)  
    disconnect(afsc)
  }
  
  # read in archived catch data
  afscdata::goa_fhs_catch_1978_1990 %>%
    vroom::vroom_write(here::here(year, "data", "user_input", "goa_fhs_catch_1978_1990.csv"))
  
  # timestamp
  q_date(year)
}