#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
#' @export
golem_add_external_resources <- function(){
  golem::bundle_resources(
    path = app_sys("app/www"),
    app_title = "Tarmac Triage Project"
  )
  
  shiny::tags$head(
    # Load custom CSS
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom_desc.css")
  )
}
