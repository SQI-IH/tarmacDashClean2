app_ui <- function() {
  library(shinyjqui)
  # Add this to your UI
  tags$head(
    tags$style(HTML("
    .main-header .logo {
      width: 300px;  /* Match the width from above */
    }
    .main-header .navbar {
      margin-left: 300px;  /* Match the width from above */
    }
  "))
  )
  tagList(
    golem_add_external_resources()
  )
  dashboardPage(
    header(),
    sidebar(),
    body()
  )
}
