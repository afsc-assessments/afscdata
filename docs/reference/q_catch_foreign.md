# query foreign catch data from the AKFIN server

something about the species names and area codes, since they are so
different from everything else? pulls data from AKFIN
pre1991.foreign_blend table

## Usage

``` r
q_catch_foreign(year, species, area, db, print_sql = FALSE, save = TRUE)
```

## Arguments

- year:

  max year to retrieve data from

- species:

  common name (e.g., "sablefish" or "all flounders") - can call multiple
  species

- area:

  numeric area digit code - multiples is ok

- db:

  data server to connect to (akfin)

- print_sql:

  outputs the sql query instead of calling the data (default: false) -
  save must be false

- save:

  saves a file to the data/raw folder, otherwise sends output to global
  enviro (default: true)

## Value

saves catch data as data/raw/for_catch_data.csv or outputs to the global
environment, also saves a copy of the SQL code used for the query and
stores it in the data/sql folder.

## Examples

``` r
if (FALSE) { # \dontrun{
db <- afscdata::connect()
q_catch_foreign(year=2022, species="sablefish", area = 54:57, db = db)
} # }
```
