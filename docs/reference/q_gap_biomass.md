# query GAP products bottom trawl survey biomass note: pay attention to the 'design_year' column, particularly for region or depth queries as there may be data duplicates

six areas are available to query from bs = bering sea + northwest
1987-present (includes nw stations) - recommended bsslope = bering sea
slope nbs = northern bering sea ai = aleutian islands goa = gulf of
alaska old_bs = bering sea standard 1982-present (minus ~20 stations in
nw) - not recommended

## Usage

``` r
q_gap_biomass(
  year = 2024,
  species = 10110,
  area = "goa",
  type = "region",
  design_yr = 2025,
  db,
  print_sql = FALSE,
  save = TRUE
)
```

## Arguments

- year:

  max year to retrieve data from, and default folder location

- species:

  5 digit afsc species code(s) e.g., 79210 or c(79210, 90210)

- area:

  options are bs (the bs+nw), bsslope, nbs, ai, goa, old_bs (was called
  "standard") - can only call a single area

- type:

  the goa and ai have: region, reg_area, nmfs_stat_area, stratum, inpfc,
  inpfc_depth, depth; the bs has: region, subarea, stratum, depth; the
  bsslope has: region, subarea, stratum; the nbs has region, stratum -
  can only use a single type (default: "region")

- design_yr:

  the survey design year, using multiple will double (or more your
  values): default 2025

- db:

  the database to query (akfin)

- print_sql:

  outputs the sql query instead of calling the data (default: false)

- save:

  save the file in designated folder, if FALSE outputs to global
  environment

## Value

saves bts biomass data as data/raw/(area)\_(type)\_bts_biomass_data.csv
or outputs to the global environment, also saves a copy of the SQL code
used for the query and stores it in the data/sql folder.

## Examples

``` r
if (FALSE) { # \dontrun{
db <- afscdata::connect()
q_gap_biomass(year=2024, species=10110, area="bs", type="region", db=db)
} # } 
```
