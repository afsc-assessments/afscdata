# query fish ticket data

query fish ticket data

## Usage

``` r
q_fish_ticket(
  year,
  species,
  area,
  db,
  add_fields = NULL,
  print_sql = FALSE,
  save = TRUE
)
```

## Arguments

- year:

  max year to query through (and location to save results)

- species:

  species group code e.g., "DUSK"

- area:

  fmp_area (GOA, BSAI) or fmp_subarea (BS, AI, WG, CG, WY, EY, SE) -
  also available (SEI, PWSI), can use all fmp_areas or all fmp_subareas,
  but don't mix the two

- db:

  data server to connect to (akfin)

- add_fields:

  add other columns to the database (must currently exist on server).
  "\*" will return all table columns available

- print_sql:

  outputs the sql query instead of calling the data - save must be false

- save:

  saves a file to the data/raw folder, otherwise sends output to global
  enviro (default: TRUE)

## Value

saves fish ticket data as data/raw/fishticket_data.csv or outputs to the
global environment, save also saves a copy of the SQL code used for the
query and stores it in the data/sql folder.

## Examples

``` r
if (FALSE) { # \dontrun{
q_fish_ticket(year=2022, species="NORK", area="goa", db=db)
} # }
```
