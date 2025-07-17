#' mod_tarmac_ui
#'
#' @param id shiny id
#'
#' @export
#' @name mod_tarmac

mod_tarmac_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shinydashboard::tabItem(
    tabName = "tarmac",
    shiny::fluidRow(
      tarmac_description_box(ns)
    ),
    shiny::br(),
    shiny::fluidRow(
      shinydashboard::box(
        width = 8,
        title = "Monthly Tarmac Events",
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("tarmacBar"), height = "250px")
        )
      ),
      shiny::uiOutput(ns("tarmac_box"))
    )
  )
}

tarmac_description_box <- function(ns) {
  primary <- "#005AA2"
  darker_primary <- "#6185A2"  # Slightly darker than primary
  accent <- "#EDEDED"
  secondary <- "#DE5428"
  text_color <- "#EDEDED"
  shiny::tags$div(
    id = ns("tarmac_box"),
    style = sprintf(
      paste(
        "background-color:%s;",
        "color:%s;",
        "border-left:5px solid %s;",
        "border-radius:8px;",
        "padding:15px;",
        "width:98%%;",              # note the doubled %%
        "margin:20px auto;",
        "box-shadow:0 2px 4px rgba(0,0,0,.1);",
        "display:flex;",
        "justify-content:space-between;",
        "align-items:center;",
        "font-size:1.3em;",
        "font-weight:500;",
        "line-height:1.5;"
        , collapse = " "),
      darker_primary, text_color, accent
    ),
    
    class = "tarmac-desc-box",
    shiny::tags$div(style = "flex:1; font-weight:bold; font-size:1.5em; color:#EDEDED;", "Project Aim"),
    shiny::tags$div(style = "flex:3 1 0; text-align:left;", shiny::textOutput(ns("tarmac_text"))),
    shiny::tags$div(
      style = "flex:0 0 auto; display:flex; align-items:center; gap:16px; margin-left:auto;",
      shiny::tags$a(
        href = get_toolkit_download_url(),
        shiny::tags$img(
          src = "toolkit-icon.png",
          alt = "Tarmac Toolkit",
          width = "80px",
          title = "Download Toolkit")
      ),
      shiny::tags$a(
        href = "tarmac-qr.png",
        download = "tarmac-qr.png",
        shiny::tags$img(src = "tarmac-qr.png",
                        alt = "QR Code",
                        width = "80px",
                        title = "Download QR code")
      )
    )
  )
}

 
#' mod_tarmac_server
#'
#' @param id shiny id
#'
#' @export
mod_tarmac_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    edData <- shiny::reactive(allEd())
    tarmacData <- shiny::reactive(load_tarmac_data(edData()))
    
    output$tarmacs_per_year <- shinydashboard::renderValueBox({
      recent <- read.csv("app_data/homepage_summary.csv") 
      
      
      
      shinydashboard::valueBox(
        value = sum(recent$tarmac_12mo),
        subtitle = shiny::HTML("Tarmac Patients Registered in the past 12 months"),
        icon = shiny::icon("hospital-user"),
        color = "green"
      )
    })
    
    output$tarmac_text <- shiny::renderText({
      df <- dynamicText()
      # df$text[df$instrument == 'aim text']
    })
    
    output$tarmacBar <- plotly::renderPlotly({
      summaryBarPlot(tarmacData())
    })
    
    output$tarmac_box <- tarmac_boxUI(ns)
  })
}


## copy to body.R
# mod_tarmac_ui("tarmac_ui_1")

## copy to app_server.R
# callModule(mod_tarmac_server, "tarmac_ui_1")

## copy to sidebar.R
# menuItem("displayName",tabName = "tarmac",icon = icon("user"))
