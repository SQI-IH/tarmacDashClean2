# Module UI
 
#' @title mod_arrival_forecast_ui and mod_arrival_forecast_server
#' @description A shiny module.
 
mod_arrival_forecast_ui <- function(id) {
	ns <- NS(id)
	tabItem(
		tabName = "arrival_forecast",
		fluidRow(
			
		)
	)
}
 
# Module Server
 
mod_arrival_forecast_server <- function(input, output, session) {
	ns <- session$ns
}
 
## copy to body.R
# mod_arrival_forecast_ui("arrival_forecast_ui_1")
 
## copy to app_server.R
# callModule(mod_arrival_forecast_server, "arrival_forecast_ui_1")
 
## copy to sidebar.R
# menuItem("displayName",tabName = "arrival_forecast",icon = icon("user"))
 
