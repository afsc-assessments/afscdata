#' Sablefish fmp area key by location
#'
#' A dataset containing area definitions
#'
#' @format A data frame with 923 observations and 2 variables:
#' \describe{
#' \item{loc}{long_lat}
#' \item{FMP_SUBAREA}{defined area for location}
#' }
"fmp_key"

#' GOA arrowtooth flounder historical catch data
#'
#' A dataset containing trawl gear catch by year for 1961-1990
#'
#' @format A data frame with 30 observations and 2 variables:
#' \describe{
#' \item{Year}{year of catch}
#' \item{Catch}{weight of catch in 1,000 t}
#' }
"goa_atf_catch_1961_1990"

#' GOA dusky rockfish historical catch data
#'
#' A dataset containing trawl gear catch by year for 1977-1990
#'
#' @format A data frame with 14 observations and 2 variables:
#' \describe{
#' \item{year}{year of catch}
#' \item{catch}{weight of catch in 1,000 t}
#' }
"goa_dusk_catch_1977_1990"

#' GOA northern rockfish historical catch data
#'
#' A dataset containing trawl gear catch by year for 1961-1992
#'
#' @format A data frame with 32 observations and 2 variables:
#' \describe{
#' \item{Year}{year of catch}
#' \item{Catch}{weight of catch in 1,000 t}
#' }
"goa_nork_catch_1961_1992"

#' GOA POP historical catch data
#'
#' A dataset containing trawl gear catch by year for 1961-1990
#'
#' @format A data frame with 30 observations and 2 variables:
#' \describe{
#' \item{year}{year of catch}
#' \item{catch}{weight of catch in 1,000 t}
#' }
"goa_pop_catch_1960_1990"

#' GOA POP historical fishery length comp data
#'
#' A dataset containing length comp collection data by year for 1963-1977
#'
#' @format A data frame with 765 observations and 4 variables:
#' \describe{
#' \item{Length}{length in cm}
#' \item{value}{number of lengths collected}
#' \item{year}{1963-1977}
#' \item{hauls_sampled}{number of hauls sampled}
#' }
"goa_pop_fixed_fish_length_comp"

#'GOA rougheye/blackspotted rockfish historical catch data
#'
#'A dataset containing trawl and longline gear catch by year for 1977-2004
#'
#'The catches from 1977-1992 were from Soh (1998), which reconstructs the catch
#'history using an information weighting factor (λ) to combine catch histories
#'from both survey and fishery information. The catches from 1993-2004 were
#'constructed  used observer catch data from the FMA Observer Program (Clausen
#'et al. 2004, Appendix A). Observed catches were available from the FMA
#'database by area, gear, and species for hauls sampled by observers. This
#'information was used to calculate proportions of RE/BS catch by gear type.
#'These proportions were then applied to the combined shortraker/rougheye catch
#'from the NMFS Alaska Regional Office to yield estimates of total catch for
#'RE/BS rockfish.
#'
#'Clausen, D. M., D.H. Hanselman, J.T. Fujioka, and J. Heifetz.  2004. Gulf of
#'Alaska shortraker/rougheye and other slope rockfish. In Stock assessment and
#'fishery evaluation report for the groundfish resources of the Gulf of Alaska,
#'p. 413 – 463. North Pacific Fishery Management Council, 605 W 4th Ave, Suite
#'306, Anchorage AK 99501. https://apps-afsc.fisheries.noaa.gov/refm/docs/2004/GOAsloperf.pdf
#'
#'Soh, Sung Kwon. 1998. The use of harvest refugia in the management of
#'shortraker and rougheye rockfish (Sebastes borealis/Sebastes aleutianus) in
#'the Gulf of Alaska. Ph.D. Thesis – University of Washington. 194 pp.
#'
#'
#'@format A data frame with 28 observations and 2 variables:
#' \describe{
#' \item{year}{year of catch}
#' \item{catch}{weight of catch in t}
#' }
"goa_rebs_catch_1977_2004"

#' GOA POP historical size at age parameters data
#'
#' A dataset containing size at age parameters for POP <= 1960
#'
#' @format A data frame with 1 observations and 5 variables:
#' \describe{
#' \item{linf}{von B parameter}
#' \item{k}{von B parameter}
#' \item{t0}{von B parameter}
#' \item{a}{a parameter}
#' \item{b}{b parameter}
#' }
"saa_pop_60"


#' Sablefish historical abundance data
#'
#' A dataset containing both fixed gear and trawl gear catch, cpue, rpn & rpw data by year
#'
#' @format A data frame with 121 observations and 5 variables:
#' \describe{
#' \item{year}{year}
#' \item{value}{weight of catch in 1,000 t, or relative abundance}
#' \item{variable}{rpw, rpn, cpue, or catch}
#' \item{fleet}{japan or domestic}
#' \item{gear}{lls = longline survey, llf = longline fishery, tf = trawl fishery}
#' }
"sabl_fixed_abundance"

#' Sablefish age and length comp data
#'
#' A dataset containing both age and length composition data for multiple fisheries and surveys
#'
#' @format A data frame with 2010 observations and 7 variables:
#' \describe{
#' \item{year}{year}
#' \item{fleet}{japan or domestic}
#' \item{type}{age or length}
#' \item{gear}{lls = longline survey, tf = trawl fishery, ts = trawl survey}
#' \item{age}{if relevant, otherwise NA}
#' \item{length}{length cm if relevant, otherwise NA}
#' \item{comp}{age or length}
#' \item{sex}{male, female or NA}
#' }
"sabl_fixed_comps"


#' Sablefish defining areas for age samples
#'
#' A dataset containing area definitions
#'
#' @format A data frame with 15 observations and 2 variables:
#' \describe{
#' \item{NMFS_AREA}{area number}
#' \item{AREA_NAME}{assignment for area number}
#' }
"sabl_new_areas"

#' vessel lengths
#'
#' Vessel lengths for calculating whale depredation rates in the sablefish fishery
#'
#' @format A data frame with 7558 observations and 2 variables:
#' \describe{
#' \item{VESSEL_CODE}{vessel id}
#' \item{LENGTH}{length of the vessel}
#' }
"sabl_vessel_lengths"

#' Sablefish fixed weights at age
#'
#' A dataset containing weights at age for two time blocks, by sex
#'
#' @format A data frame with 90 observations and 3 variables:
#' \describe{
#' \item{waa}{weight at age}
#' \item{sex}{male or female}
#' \item{timeblock}{current or old}
#' }
"sabl_waa"



#' GOA Pacific cod historical larval index data
#'
#' A dataset containing larval index collections year for 1977-2022
#'
#' @format A data frame with 92 observations and 5 variables:
#' \describe{
#' \item{year}{1977-2022}
#' \item{seas}{season?}
#' \item{index}{index id}
#' \item{obs}{observation}
#' \item{se_log}{log standard error of obs}
#' }
#' 
"goa_pcod_larval_indices"


#' GOA Pacific cod historical catch
#'
#' A dataset containing historical catch data by year for 1977-1990
#'
#' @format A data frame with 280 observations and 4 variables:
#' \describe{
#' \item{tons}{catch in tons}
#' \item{gear}{fishery gear type}
#' \item{year}{1977-1990}
#' \item{season}{quarterly fishery season}
#' } 
#' 
"goa_pcod_catch_1977_1990"


#' GOA flathead sole historical catch
#'
#' A dataset containing historical catch data by year for 1978-1990
#'
#' @format A data frame with 13 observations and 2 variables:
#' \describe{
#' \item{year}{1978-1990}
#' \item{catch}{catch in tons}
#' }
"goa_fhs_catch_1978_1990"