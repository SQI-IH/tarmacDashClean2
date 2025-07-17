header <- function() {
  dashboardHeader(
    title = tagList(
      span(class = "logo-lg", a("Tarmac Triage", style = "color:white !important"))
    ),
    controlbarIcon = "gear"
  )
}
