#' Load IH outage data from Google Sheets
#'
#' @return A cleaned data frame of outage events
#' @export
outageDataLoad <- function() {
  # options(
  #   gargle_oauth_email = TRUE,
  #   gargle_oauth_cache = app_sys("app_data/.secrets")  # golem-style project path
  # )
  # 
  # df <- googlesheets4::read_sheet(
  #   "https://docs.google.com/spreadsheets/d/1blKyewomhKKNxCJ_nnGt7MQFDH0RBnPKh4hrUKf-o_c/edit?gid=976485941"
  # )
  df <- readr::read_csv('app_data/outage/outage_summary_main.csv')
  
  dfOutage <- 
    df |>
    dplyr::filter(!is.na(submission_date)) |>
    dplyr::mutate(start_date = lubridate::ymd(start_date))
  
  dfPhsa <- readxl::read_excel('./app_data/outage/phsa_data.xlsx')
  
  dfPhsa <- 
    dfPhsa |>
    dplyr::mutate(start_date = lubridate::date(lubridate::ymd_hms(`Closure Start`)))
  
  dplyr::full_join(dfOutage, dfPhsa, by = c('hospital_name' = 'Diverting Hospital',
                                                      'start_date' = 'start_date',
                                                      'hospital_dept' = 'Department'))
}

#' Export filtered outage data to CSV
#'
#' @param df A filtered dataframe of outage records
#' @param path Output CSV path (default = './output/ED Closure Info.csv')
#' @export
outageDataExport <- function(df, path = "./output/ED Closure Info.csv") {
  df |>
    dplyr::filter(hospital_dept == "Emergency") |>
    dplyr::select(start_date:end_time, hospital_city, hospital_name) |>
    readr::write_csv(path)
}

#' Create sidebar UI controls for outages module
#'
#' @param ns Namespace function
#' @param tag Optional tag to differentiate inputs
#' @param city Logical, whether to include city selector
#' @return HTML UI tag
#' @export
sideCtrls <- function(ns, tag = "", city = TRUE) {
  cktag <- paste0("cities", tag)
  drtag <- paste0("outage_dates", tag)
  tptag <- paste0("outage_type", tag)
  
  shiny::div(
    shinyWidgets::checkboxGroupButtons(
      inputId = ns(cktag),
      label = shiny::tags$span(style = "color: #FBFAF5;", "City"),
      choices = character(0),
      selected = character(0),
      size = "xs",
      status = "warning"
    ),
    shiny::dateRangeInput(
      inputId = ns(drtag),
      label = shiny::tags$span(style = "color: #FBFAF5;", "Date Range:"),
      start = "",
      end = "",
      min = "",
      max = "",
      format = "mm/dd/yy",
      separator = " - "
    ),
    shiny::checkboxInput(
      inputId = ns(tptag),
      label = shiny::tags$span(style = "color: #FBFAF5;", "Include Partial Closures?"),
      value = FALSE
    ),
    shiny::actionButton(ns("reset_outage_filters"), "Reset Data")
  )
}

#' Update sidebar UI controls dynamically
#'
#' @param df Outage data
#' @param session The current shiny session
#' @param tag Optional suffix tag to target specific inputs
#' @export
updateSideCtrls <- function(df, session, tag = "") {
  cktag <- paste0("cities", tag)
  drtag <- paste0("outage_dates", tag)
  tptag <- paste0("outage_type", tag)
  
  cts <- unique(df$hospital_city)
  cts <- cts[!is.na(cts)]
  
  sel <- df |>
    filter(hospital_dept == "Emergency",
           diversion_type == "ED Full Diversion - ED fully closed",
      start_date >= today() - months(6)) |>
    pull(hospital_city) |>
    unique() 
  
  shinyWidgets::updateCheckboxGroupButtons(
    session = session,
    inputId = cktag,
    choices = cts,
    selected = sel,
    size = "xs"
  )
  
  shiny::updateDateRangeInput(
    session = session,
    inputId = drtag,
    start = min(df$start_date, na.rm = TRUE),
    end = max(df$start_date, na.rm = TRUE),
    min = min(df$start_date, na.rm = TRUE),
    max = max(df$start_date, na.rm = TRUE)
  )
  
  shiny::updateCheckboxInput(session, inputId = tptag, value = FALSE)
}
