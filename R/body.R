body <- function() {
  dashboardBody(
    useShinyjs(),
    theme_dashboard(),
    tabItems(
      mod_tarmac_ui("tarmac_ui_1")
      ,mod_ih_closures_ui("ih_closures_ui_1")
      ,mod_project_docs_ui("project_docs_ui_1")
      # ,mod_site_profile_ui("site_profile_ui_1")
      ,mod_ed_statistics_ui("ed_statistics_ui_1")
      ,mod_event_analysis_ui("event_analysis_ui_1")
      # ,mod_data_upload_ui("data_upload_ui_1")
      
    )
  )
}
