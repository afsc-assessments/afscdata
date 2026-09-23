# query bottom trawl survey length data from GAP on the AKFIN server

query bottom trawl survey length data from GAP on the AKFIN server

## Usage

``` r
q_bts_gap_length(year, species, area, db, print_sql = FALSE, save = TRUE)
```

## Arguments

- year:

  max year to retrieve data from

- species:

  5 digit species code (e.g., 10110) - can place multiple in a vector
  c(10110, 10130)

- area:

  ebs, nbs, ebs_slope, ebs_nbs, ai, goa - can do multiples c("bs","ai")

- db:

  data server to connect to (akfin)

- print_sql:

  outputs the sql query instead of calling the data (default: false) -
  save must be false

- save:

  saves a file to the data/raw folder, otherwise sends output to global
  enviro (default: true)

## Value

saves bts length data as data/raw/bts_length_data.csv or outputs to the
global environment, also saves a copy of the SQL code used for the query
and stores it in the data/sql folder.

## Examples

``` r
if (FALSE) { # \dontrun{
db <- afscdata::connect()
q_bts_gap_length(year=2022, species=10110, area = "goa", db = db)
} # }
```
