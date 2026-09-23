# copy the previous SSC accepted model as the new "base" model

copy of all base model files, adds a "README.md" file that identifies
the model used (or appends the current README), predicated on using the
'afscdata::setup_folders()' function, though has an option for other
folder structures. Note, this function copies everything over (e.g.,
retrospectives) and may take a few minutes to complete

## Usage

``` r
accepted_model(base_year, base_model, year, folder = NULL)
```

## Arguments

- base_year:

  year of the base model

- base_model:

  name of the base model (folder name)

- year:

  current year

- folder:

  if the base model is in a different location than the afscdata
  structure is looking for

## Value

a 'base' model folder with all of the prior years inputs/outputs

## Examples

``` r
if (FALSE) { # \dontrun{
accepted_model(base_year = 2020, base_model = "m18.2b", year = 2021)
} # }
```
