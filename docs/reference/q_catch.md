# query fishery catch data from AKFIN server

Note that this function produces results that may be confidential. They
can be hidden from git/GitHub by adding fish_catch_data.csv to your
.gitignore file

## Usage

``` r
q_catch(
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

  max year to retrieve data from

- species:

  species group code e.g., "DUSK" or numeric agency values e.g. c("131",
  "132") - must be either all 4 digit or 3 digit codes

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

saves catch data as data/raw/fish_catch_data.csv or outputs to the
global environment, save also saves a copy of the SQL code used for the
query and stores it in the data/sql folder.

## Examples

``` r
if (FALSE) { # \dontrun{
db <- afscdata::connect()
q_catch(year=2022, species="NORK", area="goa", db=db)
} # }
 
```
