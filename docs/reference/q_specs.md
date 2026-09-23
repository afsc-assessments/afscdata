# Query specs

Query specs

## Usage

``` r
q_specs(year, species, area, db, print_sql = FALSE, save = TRUE)
```

## Arguments

- year:

  for specifying the correct folder

- species:

  species group code e.g., "DUSK", "PCOD"

- area:

  "GOA" or "BSAI"

- db:

  data server to connect to (akfin)

- print_sql:

  outputs the sql query instead of calling the data - save must be false

- save:

  saves a file to the data/raw folder, otherwise sends output to global
  enviro (default TRUE)

## Examples

``` r
if (FALSE) { # \dontrun{
db = connect()
q_specs(year=2024, species='PCOD', area='BSAI', db=db, save=F)
} # }
```
