# query fishery specimen data from the AKFIN server

query fishery specimen data from the AKFIN server

## Usage

``` r
q_fsh_specimen(
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

  3 digit species codes (e.g., 201) - can place multiple in a vector
  c(201, 131)

- area:

  bs, ai, goa, or hwc, wc, hg, hbs - can do multiples c("bs","ai")

- db:

  data server to connect to (akfin)

- add_fields:

  add other columns to the database (must currently exist on server).
  "\*" will return all table columns available

- print_sql:

  outputs the sql query instead of calling the data (default: false)

- save:

  saves a file to the data/raw folder, otherwise sends output to global
  enviro (default: true)

## Value

saves fishery specimen data as data/raw/fish_specimen_data.csv or
outputs to the global environment, also saves a copy of the SQL code
used for the query and stores it in the data/sql folder.

## Examples

``` r
if (FALSE) { # \dontrun{
db <- afscdata::connect("akfin")
q_fsh_specimen(year=2022, species=301, area = "goa", db = db)
} # } 
```
