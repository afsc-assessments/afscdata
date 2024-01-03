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
  afsc_species = 10130 
  norpac_species = c(103,145) # fhs, bering flounder
  
  akfin = connect()
  
  q_catch(year, species=species, area=area, db=akfin)
  q_fish_obs(year, species=norpac_species, area=area, db=akfin,)
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
  
  ## placeholder -- perform AI/EBS linear model here, reformat and save.
  ## this supercedes use of afscassess::bts_biomass for this stock.
  
  ## calculate variance by region,
  
  vroom::vroom(here::here(year, "data", "raw", "bsai_total_bts_biomass_data.csv"), delim = ",")
   cpue %>% 
     dplyr::group_by(year, survey) %>%
     dplyr::summarise(biomass= sum(total_biomass),
               sd = sqrt(sum(biomass_var))/1000) %>%
     tidyr::pivot_wider(names_from=survey, values_from=c(biomass, sd)) -> bts_sparse
   
   z1 <- subset(bts_sparse, !is.na(biomass_AI))
   z2 <- subset(bts_sparse, is.na(biomass_AI))
   ## linear model to interpolate AI biomass & sd in off years
   lm_bio <-lm(biomass_AI~biomass_EBS_SHELF, data = z1)
   lm_var <- lm(sd_AI~sd_EBS_SHELF, data = z1)
   ## backfill all AI values
   z2$biomass_AI <- as.numeric(predict(lm_bio, newdata=z2))
   z2$sd <- as.numeric(predict(lm_var, newdata=z2))
   ## calculate total index and variance
   ## Recombine and add together biomass and variances
    rbind(z1,z2) %>% 
     dplyr::group_by(year) %>%
     dplyr::summarize(biomass=round(biomass_AI+biomass_EBS_SHELF,5),
               variance=sum(sd_AI^2,sd_EBS_SHELF^2,na.rm = TRUE),
               .groups='drop') %>%
     ## SE on log scale, which SS requires, is sqrt(log(1+CV^2))
     dplyr::mutate(se_log=round(sqrt(log(1+variance/biomass^2)),5)) %>%
     dplyr::select(-variance)
   

  
  index_ebs <-  read.csv(here(year,'data','raw',"2023-12-19-biomass_survey_ebs.csv")) %>%
    select(year=YEAR, biomass=BIOMASS,
           variance=VARBIO) %>% cbind(survey='ebs')
  index_ai <- read.csv(here(year, 'data','raw','2023-12-19-biomass_survey_ai.csv')) %>%
    mutate(species=gsub(" ", "_",COMMON_NAME)) %>%
    select(year=YEAR, biomass=TOTAL_BIOMASS,
           variance=BIOMASS_VAR) %>% cbind(survey='AI')
  index_raw <- rbind(index_ebs, index_ai) %>%
    pivot_wider(names_from=survey, values_from=c(biomass, variance))
  
  ## Do a linear regression to get missing AI years
  index_raw  <- index_raw %>% mutate(sd_ebs=sqrt(variance_ebs), sd_AI=sqrt(variance_AI))
  interpyr <- index_raw$year[which(is.na(index_raw$biomass_AI))]
  z1 <- subset(index_raw, !is.na(biomass_AI))
  z2 <- subset(index_raw, is.na(biomass_AI))
  lmbio <- lm(biomass_AI~biomass_ebs, data=z1)
  z2$biomass_AI <- as.numeric(predict(lmbio, newdata=z2))
  lmvar <- lm(sd_AI~sd_ebs, data=z1)
  z2$sd_AI <- as.numeric(predict(lmvar, newdata=z2))
  
  ## Recombine and add together biomass and variances
  index <- rbind(z1,z2) %>% group_by(year) %>%
    summarize(biomass=round(biomass_AI+biomass_ebs,5),
              variance=sum(sd_AI^2,sd_ebs^2,na.rm = TRUE),
              .groups='drop') %>%
    ## SE on log scale, which SS requires, is sqrt(log(1+CV^2))
    mutate(se_log=round(sqrt(log(1+variance/biomass^2)),5)) %>%
    select(-variance)
  
  SS_index <- data.frame(year=index$year, seas=7, index=2, 
                         obs=index$biomass, se_log=index$se_log)
  write.csv(x=SS_index, file= here(year,'data','output','SS_survey_index.csv'),
            row.names=FALSE)
  
  
  
  
  if(isTRUE(off_yr)) {
    disconnect(akfin) 
  } else {
    q_fsh_specimen(year, species=norpac_species, area, db=akfin, print_sql=FALSE, save=TRUE, add_fields='sex')
    q_fsh_length(year, species=norpac_species, area, db=akfin, print_sql=FALSE, save=TRUE, add_fields='sex') 
    disconnect(akfin)  
    
    afsc = connect("afsc")
    q_bts_specimen(year, species=afsc_species, area, db=afsc, add_fields='sex')
    q_bts_length(year, species=afsc_species, area, db=afsc, add_fields='sex') 
    disconnect(afsc)
  }
  
  # timestamp
  q_date(year)
  
  }