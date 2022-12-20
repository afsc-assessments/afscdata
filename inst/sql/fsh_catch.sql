SELECT 
  year,
  species_group_name,
  species_name as species,
  species_group_code,
  retained_or_discarded,
  trip_target_code,
  cdq_flag as cdq,
  fmp_gear as gear,
  fmp_area,
  fmp_subarea,
  week_end_date,
  weight_posted as tons
FROM council.comprehensive_blend_ca
WHERE fmp_area
-- insert area
AND year
-- insert year
AND species_group_code
-- insert species
