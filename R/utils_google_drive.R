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
