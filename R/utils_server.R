#' Access files from within the app package
#'
#' @param ... character vectors specifying subdirectories and the file name
#'
#' @return A file path
#' @export
app_sys <- function(...) {
  system.file(..., package = "tarmacDash")
}

