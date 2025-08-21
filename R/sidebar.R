sidebar <- function() {
  dashboardSidebar(
    sidebarMenu(
      id = "main_tabs",
      menuItem("Tarmac Overview", tabName = "tarmac", icon = icon("truck-medical"))
      ,menuItem("IH Closures", tabName = "ih_closures", icon = icon("plug-circle-xmark"))
      # # ,menuItem("ED Site Profiles",tabName = "site_profile",icon = icon("hospital"))
      ,menuItem("ED Statistics", tabName = "ed_statistics", icon = icon("chart-column"))
      ,menuItem("Project Documents", tabName = "project_docs", icon = icon("file"))
      ,menuItem("ED Event Viewer", tabName = "event_analysis", icon = icon("calendar"))
      # ,menuItem("Upload Data",tabName = "data_upload",icon = icon("upload"))
      ,menuItem("AI Forecast",tabName = "ai_forecast",icon = icon("user"))
    ),
    collapsed = FALSE,
    width = 200
  )
}
