# query bottom trawl survey agecomps

currently only available for goa and ai total age comps (not strata)

## Usage

``` r
q_bts_agecomp(year, species, area, db, print_sql = FALSE, save = TRUE)
```

## Arguments

- year:

  max year to retrieve data from

- species:

  5 digit afsc species code(s) e.g., 79210 or c(79210, 90210)

- area:

  options are 'ai' or 'goa' - can only call a single area

- db:

  the database to query (afsc)

- print_sql:

  outputs the sql query instead of calling the data (default: false)

- save:

  save the file in designated folder, if FALSE outputs to global
  environment

## Value

saves bts agecomp data as data/raw/area_bts_age_data.csv and
area_bts_age_specimen_data.csv or outputs to the global environment,
also saves a copy of the SQL code used for the query and stores it in
the data/sql folder.

## Details

for the goa and ai there is a "type" switch that queries by stratum, or
total - !!stratum should not be used at the moment !!. only one of these
can be used at a time.

## Examples

``` r
if (FALSE) { # \dontrun{
db <- afscdata::connect("afsc")
q_bts_agecomp(year=2022, species=21921, area = "goa", db = db)
} # } 
```
