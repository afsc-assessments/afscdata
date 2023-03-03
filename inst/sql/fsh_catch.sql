SELECT 
  year,
  agency_species_code,
  species_group_name,
  species_name as species,
  species_group_code,
  retained_or_discarded,
  trip_target_code,
  trip_target_name,  
  cdq_flag as cdq,
  fmp_gear as gear,
  agency_gear_code,
  fmp_area,
  fmp_subarea,
  reporting_area_code as nmfs_area,
  week_end_date,
  weight_posted as tons,
  vessel_id,
  ves_akr_length,
  sampling_strata,
  sampling_strata_name,
  sampling_strata_deployment_category,
  sampling_strata_selection_rate,
  deployment_trip_pk,
  deployment_trip_start_date,
  deployment_trip_end_date,
  adfg_stat_area_code,
  akr_state_federal_waters_code
FROM council.comprehensive_blend_ca
WHERE fmp_area
-- insert area
AND year
-- insert year
AND species_group_code
-- insert species
