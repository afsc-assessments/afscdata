# query nmfs longline survey raw length frequencies at the depth stratum and station level

longline database documentation available on
[akfin](https://akfinbi.psmfc.org/analyticsRes/Documentation/Database_Background_Instructions_AKFIN_20210915.pdf)

## Usage

``` r
q_lls_length(
  year,
  species,
  area = c("goa", "bs", "ai"),
  use_historical = FALSE,
  db,
  print_sql = FALSE,
  save = TRUE
)
```

## Arguments

- year:

  max year to retrieve data from

- species:

  5 digit afsc/race species code(s) e.g., 20510 for sablefish or
  c(30576, 30050) for both shortraker and rougheye/blackspotted

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

saves lls length data as data/raw/lls_length_data.csv or outputs to the
global environment. also saves a copy of the SQL code used for the query
and stores it in the data/sql folder.

## Details

primarily used in stock assessments for calculating sample size tables

sex-specific lengths are collected for sablefish, giant grenadier, spiny
dogfish, pacific cod, and greenland turbot (starting in 2021!). sex
codes: 1=male, 2=female, 3=unknown

source table on akfin: lls_length_summary_view

## Examples

``` r
if (FALSE) { # \dontrun{
# raw sablefish length frequencies in 1988 (year when the domestic survey started) 
# in the gulf of alaska

db <- connect("akfin")
q_lls_length(year=1988, species=20510, area="goa", db=db)
} # } 
```
