# query bottom trawl survey biomass

probably need to beef up the documentation on the "by" switch

## Usage

``` r
q_bts_biomass(
  year,
  species,
  area,
  type = "total",
  db,
  print_sql = FALSE,
  save = TRUE
)
```

## Arguments

- year:

  max year to retrieve data from

- species:

  5 digit afsc species code(s) e.g., 79210 or c(79210, 90210)

- area:

  options are bs (the bs+nw), bsslope, nbs, ai, goa, old_bs (was called
  "standard") - can only call a single area

- type:

  "depth", "stratum", "area", "total", "inpfc", "inpfc_depth" - only
  available for goa/ai (default: "total") - can only use a single switch

- db:

  the database to query (akfin)

- print_sql:

  outputs the sql query instead of calling the data (default: false)

- save:

  save the file in designated folder, if FALSE outputs to global
  environment

## Value

saves bts biomass data as data/raw/area\_(type)\_bts_biomass_data.csv or
outputs to the global environment, also saves a copy of the SQL code
used for the query and stores it in the data/sql folder.

## Details

six areas are available to query from bs = bering sea + northwest
1987-present (includes nw stations) - recommended bsslope = bering sea
slope nbs = northern bering sea ai = aleutian islands goa = gulf of
alaska old_bs = bering sea standard 1982-present (minus ~20 stations in
nw) - not recommended

for the goa and ai there is a "type" switch that queries by depth,
stratum, area, total, or uses the inpfc table or inpfc by depth table
only one of these can be used at a time.

## Examples

``` r
if (FALSE) { # \dontrun{
db <- afscdata::connect("akfin")
q_bts_biomass(year=2022, species=21921, area = "goa", type = "depth", db = db)
} # } 
```
