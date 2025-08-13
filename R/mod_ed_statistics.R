#' mod_ed_statistics_ui
#'
#' @description Shiny UI for ED statistics visualization module.
#' @param id Shiny module ID
#'
#' @export
mod_ed_statistics_ui <- function(id) {
  ns <- shiny::NS(id)
  shinydashboard::tabItem(
    tabName = "ed_statistics",
    shiny::fluidRow(
      ed_stats_ui(ns)  # UI helper defined separately
    )
  )
}


#' mod_ed_statistics_server
#'
#' @description Shiny server logic for ED statistics.
#' @param id Shiny module ID
#'
#' @export
mod_ed_statistics_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    profData <- shiny::reactive(profileLoad())
    
    ed_data <- shiny::reactive({
      
      df <- edLoad(filenameCreate(profData(), input$site_select))
      alt <- unique(df$ctas_level)[!unique(df$ctas_level) %in% as.character(1:5)]
      ctas <- input$ctas_ed_visits
      if(sum(input$ctas_ed_visits %in% 'alt')>0){
        ctas <- c(ctas, alt)
      }
      df |>
        filter(ctas_level %in% ctas)

    })
    
    output$weekly_arrivals_by_year <- shiny::renderTable({
      ed_data() |>
        weekly_arrivals(input$select_time_range, input) |>
        stats::setNames(c("Year", "Ambulance", "Walk-In"))
    }, digits = 1, align = "c")
    
    output$arrival_time_range <- shiny::renderUI({
      shiny::HTML(
        glue::glue(
          "<span style='font-size:20px; text-align:center; display:block;'><br>Average ",
          "<span style='color:#DE5428;'>{input$arrival_freq}</span>",
          " Arrivals<br>Between<br>",
          "<span style='color:#005AA2;'>{input$select_time_range[1]}</span>",
          " and ",
          "<span style='color:#005AA2;'>{input$select_time_range[2]}</span></span>"
        )
      )
    })
    
    output$hourly_arrivals <- plotly::renderPlotly({
      tod_plot(
        df = ed_data(),
        yrs = input$select_time_year,
        inp = input$select_arrival_mode,
        times = input$select_time_range
      )
    })
  })
}


#' ed_stats_ui
#'
#' @description UI layout for ED statistics controls and visualizations.
#' @param ns Shiny namespace function
#'
#' @export
ed_stats_ui <- function(ns) {
  shiny::div(
    shinydashboard::box(
      title = "Frequency of ED Visits",
      width = 12,
      solidHeader = TRUE,
      shiny::fluidRow(
        shiny::column(2,
                      shiny::selectizeInput(ns("site_select"), "Project Site", choices = included.cities)
        ),
        shiny::column(3,
                      shinyWidgets::sliderTextInput(
                        inputId = ns("select_time_range"),
                        label = "Select Time Window for Average:",
                        choices = c(paste0(0:23, ":00"), paste0(0:12, ":00")),
                        selected = c("8:00", "20:00"),
                        grid = TRUE
                      )
        ),
        shiny::column(2,
                      shinyWidgets::checkboxGroupButtons(
                        inputId = ns("ctas_ed_visits"),
                        label = "CTAS to include",
                        choices = c(1:5, "alt"),
                        selected = c(1:5, "alt"),
                        size = "xs",
                        status = "primary"
                      )
        )
      )
    ),
    
    shiny::fluidRow(
      shiny::column(6,
                    shinyjqui::jqui_resizable(
                      options = list(maxHeight = "600"),
                      shinydashboard::box(
                        title = "Frequency of ED Visits",
                        width = 12,
                        solidHeader = TRUE,
                        shiny::fluidRow(
                          shiny::column(7,
                                        shinycssloaders::withSpinner(shiny::tableOutput(ns("weekly_arrivals_by_year")))
                          ),
                          shiny::column(5,
                                        shiny::selectizeInput(
                                          inputId = ns("arrival_freq"),
                                          label = "Averaging Period",
                                          choices = c("Weekly", "Daily")
                                        ),
                                        shiny::uiOutput(ns("arrival_time_range"))
                          )
                        )
                      )
                    )
      ),
      
      shiny::column(6,
                    shinyjqui::jqui_resizable(
                      options = list(maxHeight = "600"),
                      shinydashboard::box(
                        title = "Arrivals by Time of Day",
                        width = 12,
                        solidHeader = TRUE,
                        style = "height: 270px;",
                        dropdown(
                          width = "150px",
                          right = TRUE,
                          icon = shiny::icon("calendar"),
                          shinyWidgets::prettyCheckboxGroup(
                            inputId = ns("select_arrival_mode"),
                            label = "Select arrival mode:",
                            choices = c("Ambulance", "Walk-in"),
                            selected = c("Ambulance", "Walk-in"),
                            thick = TRUE,
                            icon = shiny::icon("square-check"),
                            animation = "jelly",
                            outline = TRUE,
                            status = "info"
                          )
                        ),
                        shinycssloaders::withSpinner(plotly::plotlyOutput(ns("hourly_arrivals"), height = "135px"))
                      )
                    )
      )
    )
  )
}
