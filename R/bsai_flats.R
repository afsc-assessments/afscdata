#' raw data query for BSAI arrowtooth founder
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
    q_fsh_specimen(year, species, area, db=akfin, print_sql=FALSE, save=TRUE)
    q_fsh_length(year, species, area, db=akfin, print_sql=FALSE, save=TRUE)  
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
  species = "FSOL" 
  afsc_species = c(10130, 10140)  # fhs, bering flounder
  norpac_species = c(103,145) # fhs, bering flounder
  
  akfin = connect()
  
  q_catch(year, species=species, area=area, db=akfin)
  q_fish_obs(year, species=norpac_species, area=area, db=akfin)
  
  message("using GAP_PRODUCTS for survey biomass, length and age compositions")
  message("This returns comp values for FHS only, excluding the NW EBS")
  message("This routine will fail unless you have Oracle access to RACE_DATA")
  
  sql_channel <- gapindex::get_connected()

  
  production_data <- gapindex::get_data(
    year_set = 1982:2023,
    survey_set = "EBS",
    spp_codes = 10130,
    pull_lengths = TRUE, 
    haul_type = 3, 
    abundance_haul = "Y",
    sql_channel = sql_channel)
  
  save(production_data, file = here::here(year, 'data','raw','gapindex_production_data.rdata'))
  save(production_data_forbio, file = here::here(year, 'data','raw','gapindex_production_data_biomass.rdata'))
  
  ## Remove hauls and data associated with hauls in strata 82 and 90 (applicable to EBS only)
  ebs_standard_hauls <- with(production_data$haul, HAULJOIN[!(STRATUM %in% c(82, 90))])
  
  production_data$haul <- subset(x = production_data$haul, subset = HAULJOIN %in% ebs_standard_hauls)
  production_data$catch <- subset(x = production_data$catch, subset = HAULJOIN %in% ebs_standard_hauls)
  production_data$size <- subset(x = production_data$size, subset = HAULJOIN %in% ebs_standard_hauls)
  production_data$specimen <-  subset(x = production_data$specimen, subset = HAULJOIN %in% ebs_standard_hauls)
  production_data$strata <- subset(x = production_data$strata, subset = !(STRATUM %in% c(82, 90)))
  
  ## Remove subareas associated with the EBS + NW region
  ## these are not in the standard pull to being with
  production_data$subarea <- subset(x = production_data$subarea, subset = !(AREA_ID %in% c(7, 8, 9, 100, 200, 300, 99900)))
  
  ## Calculate and zero-fill CPUE
  production_cpue <- gapindex::calc_cpue(racebase_tables = production_data)
  production_data_forbio <- gapindex::calc_cpue(racebase_tables = production_data_forbio)
  
  ## Calculate biomass/abundance (w/variance), mean/variance CPUE across strata
  production_biomass_stratum <- gapindex::calc_biomass_stratum(racebase_tables = production_data,
      cpue = production_cpue)
  production_biomass_stratum_forbio <- gapindex::calc_biomass_stratum(racebase_tables = production_data_ai,
                                                               cpue = production_data_forbio) 
  
 
  ## Calculate size composition by stratum. Since the two regions have
  ## different functions, sizecomp_fn toggles which function to use
  ## and then it is called in the do.call function.
  production_sizecomp_stratum <- 
    gapindex::calc_sizecomp_stratum(
      racebase_tables = production_data,
      racebase_cpue = production_cpue,
      racebase_stratum_popn = production_biomass_stratum,
      spatial_level = "stratum",
      fill_NA_method = "BS")
  
  # Calculate regional ALK only including hauls in the EBS Standard Region
  production_alk <- subset(x = gapindex::calc_alk(
      racebase_tables = production_data,
      unsex = c("all", "unsex")[1], 
      global = FALSE),
      subset = AGE_FRAC > 0)
  
  ## Calculate age composition by stratum
  production_agecomp_stratum <- 
    gapindex::calc_agecomp_stratum(
      racebase_tables = production_data,
      alk = production_alk,
      size_comp = production_sizecomp_stratum)
  

  ## Aggregate `production_agecomp_stratum` to subareas and regions
  production_agecomp_region <-  gapindex::calc_agecomp_region(
      racebase_tables = production_data,
      age_comps_stratum = production_agecomp_stratum)
  
  # Change "STRATUM" field name to "AREA_ID"
  names(x = production_agecomp_stratum$age_comp)[
    names(x = production_agecomp_stratum$age_comp) == "STRATUM"] <- "AREA_ID"
  
  production_agecomp <- rbind(production_agecomp_region,
          production_agecomp_stratum$age_comp[, names(x = production_agecomp_region)]) %>%
    left_join(., production_data$specimen, by = c('YEAR','SEX','AGE','LENGTH','CRUISE'))
  
  
  ## Convert CRUISE to YEAR
  production_data$specimen$YEAR <- floor(x = production_data$specimen$CRUISE / 100)
  production_data$size$YEAR <- floor(x = production_data$size$CRUISE / 100)
  
  write.csv( production_data$size, file = here::here(year, 'data','raw','production_data_size.csv'), row.names = FALSE)
  write.csv( production_data$specimen, file = here::here(year, 'data','raw','production_data_specimen.csv'), row.names = FALSE)
  
  ## Calculate unique number of hauls with otolith and age data for each year (these are for CAALs) 
  nsamp_age <- aggregate(HAULJOIN ~ YEAR,
                         data = production_data$specimen,
                         FUN = function(x) length(x = unique(x = x)))
  write.csv(nsamp_age, file = here::here(year, 'data','raw','nsamp_age.csv'), row.names = FALSE)
  
  production_data$size <- merge(production_data$size, production_data$haul[,c('HAULJOIN','CRUISE')], by = 'HAULJOIN') %>%
    dplyr::mutate(YEAR = floor(x = CRUISE / 100))
  
  nsamp_len <- aggregate(HAULJOIN ~ YEAR,
                         data = production_data$size,
                         FUN = function(x) length(x = unique(x = x)))
  
  write.csv(nsamp_len, file = here::here(year, 'data','raw','nsamp_len.csv'), row.names = FALSE)

  
  ## download EBS & AI raw survey data
  q_bts_biomass(year, area="BS", species=afsc_species, db=akfin, save=F) %>%
    dplyr::mutate(species_code = as.numeric(species_code)) %>%
    dplyr::select(survey, year, species_code, haul_count=haulcount, catch_count=catcount,
                  mean_wgt_cpue = meanwgtcpue, var_wgt_cpue = varmnwgtcpue,
                  mean_num_cpue = meannumcpue, var_num_cpue = varmnnumcpue,
                  total_biomass = biomass, biomass_var=varbio,
                  min_biomass=lowerb, max_biomass = upperb, total_pop = population,
                  pop_var = varpop, min_pop = lowerp, max_pop = upperp, akfin_load_date) %>%
    dplyr::bind_rows(q_bts_biomass(year, area="AI", species=afsc_species, db=akfin, save=F)) %>%
    vroom::vroom_write(here::here(year, "data", "raw", "bsai_total_bts_biomass_data.csv"), delim = ",")
  
  vroom::vroom(here::here(year, "data", "raw", "bsai_total_bts_biomass_data.csv"), delim = ",")
  
  
  
  
  
  if(isTRUE(off_yr)) {
    disconnect(akfin) 
  } else {
    q_fsh_specimen(year, species=norpac_species, area, db=akfin, print_sql=FALSE, save=TRUE, add_fields='sex')
    q_fsh_length(year, species=norpac_species, area, db=akfin, print_sql=FALSE, save=TRUE) 
    disconnect(akfin)  
    
    afsc = connect("afsc")
    q_bts_specimen(year, species=afsc_species, area, db=afsc)
    q_bts_length(year, species=afsc_species, area, db=afsc)  
    disconnect(afsc)
  }
  
  # timestamp
  q_date(year)
  
  }