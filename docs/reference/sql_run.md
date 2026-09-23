# utility function to run sql query

utility function to run sql query

## Usage

``` r
sql_run(database, query)
```

## Arguments

- database:

  which database to connect to 'akfin' or 'afsc'

- query:

  the sql query code

## Examples

``` r
if (FALSE) { # \dontrun{
.d = sql_read("fsh_catch.sql")
.d = sql_filter(sql_precode = "<=", 2011, sql_code = .d, flag = "-- insert year")
.d = sql_filter(x = area, sql_code = .d, flag = "-- insert region")
.d = sql_filter(sql_precode = "IN", x = c("PEL7", "PELS"), 
                   sql_code = .d, flag = "-- insert species")

afsc = DBI::dbConnect(odbc::odbc(), "afsc", UID = "afsc_user", PWD = "afsc_pwd") 
 
sql_run(afsc, query) %>%
         vroom::vroom_write(here::here(year, 'data', 'raw', 'fsh_catch_data.csv'))
DBI::dbDisconnect(afsc)
} # }
```
