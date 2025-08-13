library(googlesheets4)
library(googledrive)

# Run all this once to add token to project -------------------------------

# Also add these options to script accessing google -----------------------

options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = "./app_data/.secrets"
)

googledrive::drive_auth()
googlesheets4::gs4_auth()
