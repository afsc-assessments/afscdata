# fishery observer data query

currently setup for pulling haul level detail for age or length
compositions

## Usage

``` r
q_fish_obs(year, species, area, db, print_sql = FALSE, save = TRUE)
```

## Arguments

- year:

  max year to retrieve data from

- species:

  numeric agency values e.g. c("131", "132") - must be 3 digit codes
  (norpac species codes)

- area:

  fmp_area (GOA, BSAI) or fmp_subarea (BS, AI, WG, CG, WY, EY, SE) -
  also available (SEI, PWSI), can use all fmp_areas or all fmp_subareas,
  but don't mix the two

- db:

  data server to connect to (akfin)

- print_sql:

  outputs the sql query instead of calling the data - save must be false

- save:

  saves a file to the data/raw folder, otherwise sends output to global
  enviro (default: TRUE)

## Value

saves observer data as data/raw/fish_obs_data.csv or outputs to the
global environment, save also saves a copy of the SQL code used for the
query and stores it in the data/sql folder.

## Examples

``` r
if (FALSE) { # \dontrun{
db <- afscdata::connect()
q_fish_obs(year=2022, species=301, area="goa", db=db)
} # }
```
