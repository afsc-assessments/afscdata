# query prohibited species catch (psc) estimate

prohibited species catch (PSC) estimates reported in tons for halibut
and herring, counts for salmon, crabs and other fish. Note that you can
combine trip target codes c("k", "x") and regions - though results will
be lumped together

## Usage

``` r
q_psc(year, target, area, db, save = TRUE)
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

a csv of prohibited species catch by trip target group, saved in the
data/output folder

## Examples

``` r
if (FALSE) { # \dontrun{
akfin = connect()
q_psc(year=2022, target="k", area="goa", db=akfin, save=FALSE)
disconnect(akfin)
} # }
```
