# query nmfs longine survey specimen data (age, length, weight, sex, maturity). only available for sablefish!

longline database documentation available on
[akfin](https://akfinbi.psmfc.org/analyticsRes/Documentation/Database_Background_Instructions_AKFIN_20210915.pdf)

## Usage

``` r
q_lls_sable_specimen(
  year,
  area = c("goa", "bs", "ai"),
  use_historical = FALSE,
  db = akfin,
  print_sql = FALSE,
  save = TRUE
)
```

## Arguments

- year:

  max year to retrieve data from

- area:

  options are 'goa', 'bs', 'ai', or a combo. default=c('goa', 'bs',
  'ai')

- use_historical:

  T/F include historical Japanese survey data in the results (default:
  false)

- db:

  the database to query (akfin)

- print_sql:

  outputs the sql query instead of calling the data (default: false) -
  save must be false

- save:

  save the file in designated folder, if FALSE outputs to global
  environment

## Value

saves lls sablefish specimen data as data/raw/lls_specimen_data.csv or
outputs to the global environment. also saves a copy of the SQL code
used for the query and stores it in the data/sql folder.

## Details

source table on akfin: afsc.lls_age_view

## Examples

``` r
if (FALSE) { # \dontrun{
# sablefish specimen data in 1996 (first year ages were collected in the goa domestic survey) 
# in the gulf of alaska

q_lls_specimen(year=2000, area="goa", db=db)
} # } 
```
