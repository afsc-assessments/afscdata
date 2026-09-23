# query GAP bottom trawl survey sizecomps

query GAP bottom trawl survey sizecomps

## Usage

``` r
q_bts_gap_sizecomp(year, species, area, db, print_sql = FALSE, save = TRUE)
```

## Arguments

- year:

  max year to retrieve data from

- species:

  5 digit afsc species code(s) e.g., 79210 or c(79210, 90210)

- area:

  options: ebs, ai, goa, nbs, ebs_slope, ebs_nbs, default: goa

- db:

  the database to query (akfin)

- print_sql:

  outputs the sql query instead of calling the data (default: false)

- save:

  save the file in designated folder, if FALSE outputs to global
  environment

## Value

saves bts sizecomp data as data/raw/area_bts_gap_sizecomp_data.csv or
outputs to the global environment, also saves a copy of the SQL code
used for the query and stores it in the data/sql folder.

## Examples

``` r
if (FALSE) { # \dontrun{
db <- afscdata::connect()
q_bts_gap_sizecomp(year=2022, species=21921, area = "goa", db = db)
} # } 
```
