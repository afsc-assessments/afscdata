# dev code for setting up r package
# load ----
library(devtools)
library(pkgdown)
library(keyring)

# dev code
# create_package()
use_description()
use_package("keyring")

use_readme_md()
use_mit_license()
devtools::document()
devtools::build()

use_r("queries")
use_r("tables")
use_test("queries")
pkgdown::build_site(examples = FALSE)

usethis::use_data(goa_pcod_larval_indices)

# check/test keyring
# keyring::key_set_with_value(service="afsc", username="WILLIAMSB", password = "pswd1")
# key_set_with_value(service="akfin", username="bwilliams", password = "pswd2")
rstudioapi::askForSecret("Test")
keyring::key_get("afsc", "WILLIAMSB")

keyring::key_get("akfin", "bwilliams")

keyring::backend_file$new()
db = "akfin"

keyring::key_list("afsc")$username
keyring::key_get(db, keyring::key_list(db)[1,2])
keyring::keyring_unlock()

keyring::key_list(db)$username

keyring::key_get(db, keyring::key_list(db)$username)


keyring::key_list(db)


keyring::key_get(db, keyring::key_list(db)$username)

use_r('gap_check_bio')
use_article('gap_check')
