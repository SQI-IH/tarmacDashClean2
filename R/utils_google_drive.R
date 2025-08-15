#' Return Drive download URL for toolkit
#'
#' @export
#' @name google_drive
get_toolkit_download_url <- function() {
  folder_id <- "19te_XAcl6w6PmlWMhgb3ZyzfODsuCU9X"
  
  files <- googledrive::drive_ls(
    path = googledrive::as_id(folder_id),
    type = "application/pdf"
  )
  
  file_id <- trimws(files$id[grep("Toolkit", files$name, ignore.case = TRUE)][1])
  
  sprintf("https://drive.google.com/uc?export=download&id=%s", file_id)
}

init_google_auth <- function() {
  cache_dir <- "app_data/.secrets"  # relative to app root in deploy bundle
  
  # Tell gargle where the cache is and which account to pick automatically
  options(
    gargle_oauth_cache = cache_dir,
    gargle_oauth_email = TRUE  # or "you@yourdomain.com" to pin a specific account
  )
  
  # If client is provided via env vars, gargle will pick it up automatically.
  # If you bundled client.json, register it explicitly:
  client_json <- file.path(cache_dir, "client.json")
  if (file.exists(client_json)) {
    gargle::gargle_oauth_client_from_json(client_json) |> gargle::gargle_set_client()
  }
  
  # Now activate cached credentials for both APIs
  googlesheets4::gs4_auth(cache = cache_dir, email = TRUE)
  googledrive::drive_auth(cache = cache_dir, email = TRUE)
  
  # Optional: log which account is active
  try({
    msg <- paste0(
      "gs4 user: ", capture.output(googlesheets4::gs4_user())[1],
      " | drive user: ", capture.output(googledrive::drive_user())[1]
    )
    message(msg)
  }, silent = TRUE)
}
