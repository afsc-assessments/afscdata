# Sablefish age and length comp data

A dataset containing both age and length composition data for multiple
fisheries and surveys

## Usage

``` r
sabl_fixed_comps
```

## Format

A data frame with 2010 observations and 7 variables:

- year:

  year

- fleet:

  japan or domestic

- type:

  age or length

- gear:

  lls = longline survey, tf = trawl fishery, ts = trawl survey

- age:

  if relevant, otherwise NA

- length:

  length cm if relevant, otherwise NA

- comp:

  age or length

- sex:

  male, female or NA
