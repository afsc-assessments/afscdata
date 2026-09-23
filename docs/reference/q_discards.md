# query fishery discard data from AKFIN server

query fishery discard data from AKFIN server

## Usage

``` r
q_discards(year, species, area, db, save = TRUE)
```

## Arguments

- year:

  assessment year

- species:

  species group code e.g., "DUSK" or numeric agency values e.g. c("131",
  "132") - must be either all 4 digit or 3 digit codes

- area:

  fmp_area (GOA, BSAI) or fmp_subarea (BS, AI, WG, CG, WY, EY, SE) -
  also available (SEI, PWSI), can use all fmp_areas or all fmp_subareas,
  but don't mix the two

- db:

  data server to connect to (akfin)

- save:

  saves a file to the data/output folder, otherwise sends output to
  global enviro (default: TRUE)

## Value

saves discard data as data/raw/fish_discard_data.csv or outputs to the
global environment

## Examples

``` r
if (FALSE) { # \dontrun{
db <- afscdata::connect()
q_discards(year=2026, species="NORK", area="goa", db=db)
afscdata::disconnect(db)
} # }
 
```
