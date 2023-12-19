#' raw data query for the BSAI Atka mackerel assessment
#'
#' @param year assessment year
#' @param off_yr if this is an off-year assessment change to TRUE
#'
#' @return a suite of raw data .csv files and a time stamp of when the query was done 
#' @export bsai_amak
#'
#' @examples
#' \dontrun{
#' bsai_amak(year = 2023, off_yr = TRUE)
#'}
bsai_amak <- function(year, off_yr = FALSE){
  
  # globals 
  area = "BSAI"
  species = "AMCK"
  afsc_species = 21921
  norpac_species = 193
  
  akfin = afscdata::connect()
  
  q_catch(year=year, species=species, area=area, db=akfin)
  q_fish_obs(year=year, species=norpac_species, area=area, db=akfin)
  q_bts_biomass(year=year, area=area, species=afsc_species, type='area', db=akfin) #
  
  if(isTRUE(off_yr)) {
    disconnect(akfin) 
  } else {
    q_fsh_specimen(year=year, species=afsc_species, area=area, db=akfin)
    q_fsh_length(year=year, species=afsc_species, area=area, db=akfin)
    disconnect(akfin)  
    
    afsc = connect("afsc")
    q_bts_specimen(year=year, species=afsc_species, area=area, db=afsc)
    q_bts_length(year=year, species=afsc_species, area=area, db=afsc)  
    q_bts_agecomp(year = year, species = afsc_species, area = area, db = afsc)
    q_bts_sizecomp(year = year, species = afsc_species, area = area, db = afsc)
    disconnect(afsc)
  }
  
  # read in archived catch data
  afscdata::bsai_amak_catch_1977_1990 %>%
    vroom::vroom_write(here::here(year, "data", "user_input", "bsai_amak_catch_1977_1990.csv"), ",")
  
  # timestamp
  q_date(year)
}

#' raw data query for the GOA Atka mackerel assessment
#'
#' @param year assessment year
#'
#' @return a suite of raw data .csv files and a time stamp of when the query was done 
#' @export goa_amak
#'
#' @examples
#' \dontrun{
#' goa_amak(year = 2023)
#'}
goa_amak <- function(year) {
  # globals 
  area = "GOA"
  species = "AMCK"
  afsc_species = 21921
  norpac_species = 193
  
  akfin = afscdata::connect()
  q_catch(year=year, species=species, area=area, db=akfin)
  q_bts_biomass(year, area=area, species=afsc_species, type='total', db=akfin) 
  disconnect(akfin)
  # timestamp
  q_date(year)
}