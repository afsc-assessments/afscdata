# compare GAP updated survey biomass to (now) retired design-based biomass estimates

compare GAP updated survey biomass to (now) retired design-based biomass
estimates

## Usage

``` r
gap_check_bio(year, species, area, type)
```

## Arguments

- year:

  current year

- species:

  afsc species codes e.g., 30420

- area:

  options = ai, goa, ebs, bss, nbs

- type:

  = region, subarea, area, stat_area, stratum, inpfc, inpfc_depth,
  depth, reg_area_depth

## Value

a list with orig values, gap values, and a basic report

## Examples

``` r
if (FALSE) { # \dontrun{
out <- gap_check_bio(year = 2024, species = 30420, type = 'total', area = 'AI' )
out$report
} # }
```
