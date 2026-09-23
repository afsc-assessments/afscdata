# utility function to filter sql files

utility function to filter sql files

## Usage

``` r
sql_filter(sql_precode = "IN", x, sql_code, flag = "-- insert species")
```

## Arguments

- sql_precode:

  change input e.g., ("=")

- x:

  the variable to change (e.g., year)

- sql_code:

  the sql query code...

- flag:

  a flag in the sql code to place the precode and x in the appropriate
  location

## Examples

``` r
if (FALSE) { # \dontrun{
.d = sql_filter(sql_precode = "<=", 2011, sql_code = .d, flag = "-- insert year")
} # }
```
