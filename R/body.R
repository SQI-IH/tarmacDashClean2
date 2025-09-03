body <- function() {
  dashboardBody(
    useShinyjs(),
    theme_dashboard(),
    # (optional) keep the datepicker fix
    tags$head(tags$style(HTML("
      .datepicker-dropdown { z-index: 3000 !important; }
      .daterangepicker     { z-index: 3000 !important; }
    "))),
    tabItems(
      tabItem(tabName = "tarmac",         mod_tarmac_ui("tarmac_ui_1"))
      ,tabItem(tabName = "ih_closures",    mod_ih_closures_ui("ih_closures_ui_1"))
      ,tabItem(tabName = "project_docs",   mod_project_docs_ui("project_docs_ui_1"))
      ,tabItem(tabName = "ed_statistics",  mod_ed_statistics_ui("ed_statistics_ui_1"))
      ,tabItem(tabName = "event_analysis", mod_event_analysis_ui("event_analysis_ui_1"))
      # ,tabItem(tabName = "ai_forecast",    mod_ai_forecast_ui("ai_forecast_ui_1"))
    )
  )
}
