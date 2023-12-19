#' raw data query for BSAI other rockfish
#'
#' @param year assessment year
#' @param off_yr if this is an off-year assessment change to TRUE
#' @param catch_report is this a catch report year, default FALSE
#'
#' @return a suite of raw data .csv files and a time stamp of when the query was done 
#' @export bsai_orox
#'
#' @examples
#' \dontrun{
#' bsai_orox(year = 2022, off_yr = FALSE)
#'}
bsai_orox <- function(year, off_yr = FALSE, catch_report = FALSE){
  # globals 
  area = "BSAI"
  norpac_species = c(137, 138, 139, 142, 143, 145, 146, 147, 148, 149, 150, 153, 154, 155, 156, 157, 158, 159, 166, 172, 175, 176, 178, 179, 182, 184)
  
  
  akfin = connect()
  
  if(isTRUE(catch_report)) {
    q_catch(year, species=species, area=area, db=akfin)
  } else {
    q_catch(year, species=species, area=area, db=akfin)
    q_fish_obs(year, species=norpac_species, area=area, db=akfin)
    q_bts_biomass(year, area=area, species=afsc_species, type='total', db=akfin) 
  }
  
  if(isTRUE(off_yr) | isTRUE(catch_report)) {
    disconnect(akfin) 
  } else {
    q_fsh_specimen(year, species, area, db=akfin, print_sql=FALSE, save=TRUE)
    q_fsh_length(year, species, area, db=akfin, print_sql=FALSE, save=TRUE)  
    disconnect(akfin)  
    
    afsc = connect("afsc")
    q_bts_specimen(year, species=afsc_species, area, db=afsc)
    q_bts_length(year, species=afsc_species, area, db=afsc)  
    q_bts_agecomp(year = year, species = afsc_species, area = area, db = afsc)
    q_bts_sizecomp(year = year, species = afsc_species, area = area, db = afsc)
    disconnect(afsc)
  }
  
  # read in archived catch data
  # afscdata::goa_nork_catch_1961_1992 %>%
  #   vroom::vroom_write(here::here(year, "data", "user_input", "goa_nork_catch_1961_1992.csv"), ",")
  
  # timestamp
  q_date(year)
}