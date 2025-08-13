app_server <- function(input, output, session) {
  # 1) Always init the landing tab quickly
  mod_tarmac_server("tarmac_ui_1")
  
  # 2) Track which heavy modules have been initialized
  loaded <- shiny::reactiveValues(
    ih_closures      = FALSE,
    project_docs     = FALSE,
    ed_statistics    = FALSE,
    event_analysis   = FALSE,
    arrival_forecast = FALSE
  )
  
  # 3) Helper to init a tab once
  init_tab <- function(tab) {
    switch(tab,
           "ih_closures" = if (!loaded$ih_closures) {
             loaded$ih_closures <- TRUE
             mod_ih_closures_server("ih_closures_ui_1")
           },
           "project_docs" = if (!loaded$project_docs) {
             loaded$project_docs <- TRUE
             mod_project_docs_server("project_docs_ui_1")
           },
           "ed_statistics" = if (!loaded$ed_statistics) {
             loaded$ed_statistics <- TRUE
             mod_ed_statistics_server("ed_statistics_ui_1")
           },
           "event_analysis" = if (!loaded$event_analysis) {
             loaded$event_analysis <- TRUE
             mod_event_analysis_server("event_analysis_ui_1")
           },
           "arrival_forecast" = if (!loaded$arrival_forecast) {
             loaded$arrival_forecast <- TRUE
             # keep legacy callModule if this one still uses it
             callModule(mod_arrival_forecast_server, "arrival_forecast_ui_1")
           }
    )
  }
  
  # 4) Lazy-load on tab change.
  #    Use ignoreInit = FALSE so it fires once for the *current* tab too.
  #    If the current tab is the landing "tarmac", init_tab() simply does nothing.
  shiny::observeEvent(input$main_tabs, {
    shiny::req(input$main_tabs)
    init_tab(input$main_tabs)
  }, ignoreInit = FALSE)
}
