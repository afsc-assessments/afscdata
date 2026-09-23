# Setup folder structure

Creates a common folder structure for assessment data

## Usage

``` r
setup_folders(
  year,
  dirs = c("raw", "user_input", "output", "sara", "sql"),
  tier = NULL
)
```

## Arguments

- year:

  assessment year

- dirs:

  directories to write

- tier:

  assessment tier to change the folders used - not currently implemented

## Value

creates a designated/named folder structure

## Examples

``` r
if (FALSE) { # \dontrun{
setup(2022)
} # }
```
