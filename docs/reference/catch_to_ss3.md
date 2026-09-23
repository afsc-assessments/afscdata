# Reformat Catch to SS3 structure currently only handles a single fleet, values in t

Reformat Catch to SS3 structure currently only handles a single fleet,
values in t

## Usage

``` r
catch_to_ss3(year, se = 0.01, season = 7, fleet = 1, yld_rat = NULL)
```

## Arguments

- year:

  assessment year

- se:

  standard error in log space, typically quite small

- season:

  numeric month for catches to be applied

- fleet:

  numeric id of the fishery fleet

- yld_rat:

  default NULL; else look for a vector with the projected-in year catch
  in output/ and replace

## Value

saves a csv in output/ with the correct format
