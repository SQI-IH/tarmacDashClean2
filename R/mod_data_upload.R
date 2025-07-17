# Module UI

#' @title mod_data_upload_ui and mod_data_upload_server
#' @description A shiny module for uploading, previewing, and processing ED arrival data.

mod_data_upload_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "data_upload",
    shinymanager::auth_ui(
      id = ns("auth"),
      # add image on top ?
      tags_top =
        tags$div(
          tags$h4("Tarmac Project Coordinator Content", style = "align:center"),
          tags$img(
            src = "2023 - SQI-watermark-colour-print.jpg", width = 300
          )
        ),
      # add information on bottom ?
      tags_bottom = tags$div(
        tags$p(
          "For any question, please  contact ",
          tags$a(
            href = "mailto:Lindsey.Boulet@interiorhealth.ca?Subject=Impact%20Dashboard",
            target="_top", "administrator"
          )
        )
      ),
      background = "#F8F8F8"
    ),
    shinyjs::useShinyjs(),
    shinyWidgets::useSweetAlert(),
    
    fluidRow(
      box(
        title = "Upload & Process Data",
        width = 12,
        fileInput(ns("file"), "Upload CSV File", accept = ".csv"),
        checkboxInput(ns("upload_to_db"), "Upload to Supabase?", value = FALSE),
        actionButton(ns("process"), "Process & Ingest Data"),
        verbatimTextOutput(ns("status"))
      ),
      box(
        title = "Preview Uploaded Data",
        width = 12,
        DT::dataTableOutput(ns("preview"))
      )
    )
  )
}

# Module Server

mod_data_upload_server <- function(input, output, session, conn = NULL, output_dir = "app_data/ed") {
  ns <- session$ns
  
  
  auth <- callModule(
    module =shinymanager::auth_server,
    id = "auth",
    session = session,
    check_credentials = shinymanager::check_credentials(
      data.frame(
        user = c("SQI Team", "lindsey", "sherry.sopko", "Tarmac Guest"),
        password = c("colourandshape", "mess9044", "sparwood-tarmac-project-2025", 'guest-passcode-5150')
      )
      
    )
    
  )
  shinyjs::disable(ns("process"))
  
  source("R/fct_clean_ed_data.R")
  source("R/fct_write_site_csvs.R")
  source("R/fct_build_homepage_summary.R")
  
  raw_data <- reactiveVal(NULL)
  cleaned_data <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    req(input$file)
    df <- readr::read_csv(input$file$datapath, show_col_types = FALSE)
    raw_data(df)
    
    shinyjs::enable(ns("process"))
  })
  
  observeEvent(input$process, {
    print("button works")
    req(raw_data())
    
    withProgress(message = "Processing data...", value = 0, {
      incProgress(0.1, detail = "Loading functions")
      source("R/fct_clean_ed_data.R")
      source("R/fct_write_site_csvs.R")
      source("R/fct_build_homepage_summary.R")
      
      tryCatch({
        incProgress(0.2, detail = "Cleaning data")
        clean_df <- clean_ed_data(raw_data())
        cleaned_data(clean_df)
        
        progress <- shiny::Progress$new()
        progress$set(message = "Saving site files", value = 0)
        on.exit(progress$close(), add = TRUE)
        
        write_site_csvs(clean_df, output_dir = output_dir, progress = progress)
        print("Data Saved")
        
        if (isTRUE(input$upload_to_db) && !is.null(conn)) {
          source("R/fct_upload_to_supabase.R")
          incProgress(0.75, detail = "Uploading to Supabase")
          upload_to_supabase(clean_df, conn)
        }
        
        incProgress(0.85, detail = "Building summaries")
        build_homepage_summary()
        print("Data Summarized")
        
        incProgress(1, detail = "Done")
        output$status <- renderText("✅ Data successfully processed.")
      }, error = function(e) {
        print(paste("❌ Error during processing:", e$message))
        output$status <- renderText(paste("❌ Error:", e$message))
      })
    })
  })
  
  output$preview <- DT::renderDataTable({
    req(raw_data())
    DT::datatable(
      head(raw_data(), 10),
      options = list(scrollX = TRUE),
      rownames = FALSE
    )
  })
}

## copy to body.R
# mod_data_upload_ui("data_upload_ui_1")

## copy to app_server.R
# callModule(mod_data_upload_server, "data_upload_ui_1")

## copy to sidebar.R
# menuItem("displayName", tabName = "data_upload", icon = icon("user"))
