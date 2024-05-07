#' raw data query for BSAI arrowtooth flounder
#' 
#' @param year assessment year
#' @param off_yr if this is an off-year assessment change to TRUE
#'
#' @return a suite of raw data .csv files and a time stamp of when the query was done 
#' @export bsai_atf
#'
#' @examples
#' \dontrun{
#' bsai_atf(year = 2022, off_yr = FALSE)
#'}
bsai_atf <- function(year, off_yr = FALSE){
  # globals 
  area = "BSAI"
  species = "ARTH"
  afsc_species = 10110
  norpac_species = 141
  
  akfin = connect()
  
  q_catch(year, species=species, area=area, db=akfin)
  q_fish_obs(year, species=norpac_species, area=area, db=akfin)
  q_bts_biomass(year, area="BS", species=afsc_species, db=akfin, save=F) %>% 
    dplyr::mutate(species_code = as.numeric(species_code)) %>% 
    dplyr::select(survey, year, species_code, haul_count=haulcount, catch_count=catcount, 
                  mean_wgt_cpue = meanwgtcpue, var_wgt_cpue = varmnwgtcpue, 
                  mean_num_cpue = meannumcpue, var_num_cpue = varmnnumcpue, 
                  total_biomass = biomass, biomass_var=varbio, 
                  min_biomass=lowerb, max_biomass = upperb, total_pop = population,
                  pop_var = varpop, min_pop = lowerp, max_pop = upperp, akfin_load_date) %>% 
    dplyr::bind_rows(q_bts_biomass(year, area="AI", species=afsc_species, db=akfin, save=F)) %>% 
    vroom::vroom_write(here::here(year, "data", "raw", "bts_biomass_data.csv"), delim = ",")
  
  if(isTRUE(off_yr)) {
    disconnect(akfin) 
  } else {
    q_fsh_specimen(year, species=norpac_species, area, db=akfin, print_sql=FALSE, save=TRUE)
    q_fsh_length(year, species=norpac_species, area, db=akfin, print_sql=FALSE, save=TRUE)  
    disconnect(akfin)  
    
    afsc = connect("afsc")
    q_bts_specimen(year, species=afsc_species, area, db=afsc)
    q_bts_length(year, species=afsc_species, area, db=afsc)  
    disconnect(afsc)
  }
  
  # read in archived catch data
  # afscdata::goa_atf_catch_1961_1990 %>%
  # vroom::vroom_write(here::here(year, "data", "user_input", "goa_atf_catch_1961_1990.csv"), ",")
  
  # timestamp
  q_date(year)
}

#' raw data query for BSAI flathead sole
#'  
#' @param year assessment year
#' @param off_yr if this is an off-year assessment change to TRUE
#'
#' @return a suite of raw data .csv files and a time stamp of when the query was done 
#' @export bsai_fhs
#'
#' @examples
#' \dontrun{
#' bsai_fhs(year = 2022, off_yr = FALSE)
#'}
bsai_fhs<- function(year, off_yr = FALSE){
  

  # globals 
  area = "BSAI"
  # species = "FSOL" 
  # afsc_species = c(10130, 10140)  # fhs, bering flounder
  norpac_species = c(104, 120) 
  
  akfin = connect()
  
  q_catch(year, species=species, area=area, db=akfin)
  q_fish_obs(year, species=norpac_species, area=area, db=akfin)
  

  if(isTRUE(off_yr)) {
    disconnect(akfin) 
  } else {
    q_fsh_specimen(year, species=norpac_species, area, db=akfin, print_sql=FALSE, save=TRUE, add_fields='sex')
    q_fsh_length(year, species=norpac_species, area, db=akfin, print_sql=FALSE, save=TRUE) 
    disconnect(akfin)  
  }
  
  # timestamp
  q_date(year)
  
}

#' raw data query for BSAI northern rocksole
#' 
#' @param year assessment year
#' @param off_yr if this is an off-year assessment change to TRUE
#'
#' @return a suite of raw data .csv files and a time stamp of when the query was done 
#' @export bsai_nrs
#'
#' @examples
#' \dontrun{
#' bsai_atf(year = 2022, off_yr = FALSE)
#'}
bsai_nrs <- function(year, off_yr = FALSE){
  # globals 
  area = "BSAI"
  species = "RSOL"
  # flag - what's this code supposed to be?
  # afsc_species = 10110
  norpac_species = c(104, 120)
  
  akfin = connect()
  
  q_catch(year, species=species, area=area, db=akfin, save = FALSE)
  q_fish_obs(year, species=norpac_species, area=area, db=akfin)
  q_bts_biomass(year, area="BS", species=afsc_species, db=akfin, save=F) %>% 
    dplyr::mutate(species_code = as.numeric(species_code)) %>% 
    dplyr::select(survey, year, species_code, haul_count=haulcount, catch_count=catcount, 
                  mean_wgt_cpue = meanwgtcpue, var_wgt_cpue = varmnwgtcpue, 
                  mean_num_cpue = meannumcpue, var_num_cpue = varmnnumcpue, 
                  total_biomass = biomass, biomass_var=varbio, 
                  min_biomass=lowerb, max_biomass = upperb, total_pop = population,
                  pop_var = varpop, min_pop = lowerp, max_pop = upperp, akfin_load_date) %>% 
    dplyr::bind_rows(q_bts_biomass(year, area="AI", species=afsc_species, db=akfin, save=F)) %>% 
    vroom::vroom_write(here::here(year, "data", "raw", "bts_biomass_data.csv"), delim = ",")
  
  if(isTRUE(off_yr)) {
    disconnect(akfin) 
  } else {
    q_fsh_specimen(year, species=norpac_species, area, db=akfin, print_sql=FALSE, save=TRUE)
    q_fsh_length(year, species=norpac_species, area, db=akfin, print_sql=FALSE, save=TRUE)  
    disconnect(akfin)  
    
    afsc = connect("afsc")
    q_bts_specimen(year, species=afsc_species, area, db=afsc)
    q_bts_length(year, species=afsc_species, area, db=afsc)  
    disconnect(afsc)
  }
  
  # read in archived catch data
  # afscdata::goa_atf_catch_1961_1990 %>%
  # vroom::vroom_write(here::here(year, "data", "user_input", "goa_atf_catch_1961_1990.csv"), ",")
  
  # timestamp
  q_date(year)
}
