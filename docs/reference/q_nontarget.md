# query non-target species catch estimate

non-target catch estimates by weight (or numbers)

## Usage

``` r
q_nontarget(year, target, area, db, save = TRUE)
```

## Arguments

- year:

  assessment year

- target:

  targeted species: 'p' = pollock-mid, 'b' = pollock-bottom, x' = rex,
  'h' = shallow flats, 'k' = rockfish, 'w' = arrowtooth, 'c' = pcod, 'i'
  = halibut

- area:

  fmp_area (GOA, BSAI) or fmp_subarea (BS, AI, WG, CG, WY, EY, SE) -
  also available (SEI, PWSI)

- db:

  data server to connect to (akfin)

- save:

  save the file in designated folder (default = T) or the global
  environment

## Value

a csv of non-target species catch by trip target group, saved in the
data/output folder

## Examples

``` r
if (FALSE) { # \dontrun{
akfin = afscdata::connect()
q_nontarget(year=2022, target="k", area="goa", db=akfin, save=FALSE)
disconnect(akfin)
} # }
```
