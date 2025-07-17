#' mod_ih_closures_ui
#'
#' @param id Shiny module id
#'
#' @export
mod_ih_closures_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shinydashboard::tabItem(
    tabName = "ih_closures",
    shinydashboard::box(
      width = 12,
      shiny::fluidRow(
        shiny::column(3,
                      shiny::selectizeInput(
                        ns("selectBoxContent"),
                        "Select Visualization",
                        choices = c("Closures by City (Individual)", "Closures by City (Cumulative)",
                                    "Closures by Type", "Closure Map", "All Reports")
                      )
        ),
        shiny::column(3, offset = 1,
                      shinyWidgets::checkboxGroupButtons(
                        ns("cities"),
                        "Cities to Include",
                        choices = character(0),
                        selected = character(0),
                        size = "xs",
                        status = "warning"
                      )
        ),
        shiny::column(2, offset = 1,
                      shiny::checkboxInput(ns("outage_type"), "Include Partial Closures?", value = FALSE)
        )
      )
    ),
    shinydashboard::box(
      title = "Requested Closures/Diversions in IH since July 1, 2024",
      closable = FALSE,
      width = 12,
      status = "primary",
      solidHeader = TRUE,
      collapsible = FALSE,
      shiny::conditionalPanel("input.selectBoxContent == 'Closures by City (Individual)'", ns = ns,
                              plotly::plotlyOutput(ns("outage_occurence"))
      ),
      shiny::conditionalPanel("input.selectBoxContent == 'Closures by City (Cumulative)'", ns = ns,
                              plotly::plotlyOutput(ns("outage_cumulative"))
      ),
      shiny::conditionalPanel("input.selectBoxContent == 'Closures by Type'", ns = ns,
                              plotly::plotlyOutput(ns("outage_type"))
      ),
      shiny::conditionalPanel("input.selectBoxContent == 'Closure Map'", ns = ns,
                              leaflet::leafletOutput(ns("closure_map"))
      ),
      shiny::conditionalPanel("input.selectBoxContent == 'All Reports'", ns = ns,
                              DT::dataTableOutput(ns("all_reports"), width = "500px")
      )
    )
  )
}


 
#' mod_outages_server
#'
#' @param id Shiny module id
#'
#' @export
mod_ih_closures_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    data_all <- shiny::reactive(outageDataLoad())
    
    data_filtered <- shiny::reactive({
      df <- data_all()
      df <- df[df$hospital_city %in% input$cities, ]
      if (!input$outage_type) {
        df <- df[df$diversion_type == "ED Full Diversion - ED fully closed", ]
      }
      df
    })
    
    output$outage_occurence <- plotly::renderPlotly({
      occurencePlot(data_filtered())
    })
    
    output$outage_cumulative <- plotly::renderPlotly({
      cumOccurencePlot(data_filtered())
    })
    
    output$outage_type <- plotly::renderPlotly({
      typePlot(data_filtered())
    })
    
    output$closure_map <- leaflet::renderLeaflet({
      df <- data_all()
      profile <- profileLoad()
      df |> dplyr::group_by(hospital_city) |> dplyr::count() |> 
        dplyr::left_join(profile, by = c("hospital_city" = "city")) |>
        create_incidence_map()
    })
    
    output$all_reports <- DT::renderDataTable({
      allReports(data_filtered())
    })
    
    shiny::observe({
      updateSideCtrls(data_all(), session)
    })
    
    shiny::observeEvent(input$reset_outage_filters, {
      updateSideCtrls(data_all(), session)
    })
  })
}

## copy to body.R
# mod_ih_closures_ui("ih_closures_ui_1")
 
## copy to app_server.R
# callModule(mod_ih_closures_server, "ih_closures_ui_1")
 
## copy to sidebar.R
# menuItem("displayName",tabName = "ih_closures",icon = icon("user"))
 
