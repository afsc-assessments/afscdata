# raw data query for GOA shallow-water flatfish

raw data query for GOA shallow-water flatfish

## Usage

``` r
goa_shal_flats(year, off_yr = FALSE, catch_report = FALSE)
```

## Arguments

- year:

  assessment year

- off_yr:

  if this is an off-year assessment change to TRUE

- catch_report:

  is this a catch report year, default FALSE

## Value

a suite of raw data .csv files and a time stamp of when the query was
done

## Examples

``` r
if (FALSE) { # \dontrun{
goa_shal_flats(year = 2022, off_yr = FALSE)
} # }
```
