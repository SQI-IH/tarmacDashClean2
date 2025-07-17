# Module UI

#' @title mod_event_analysis_ui and mod_event_analysis_server
#' @description A shiny module.

mod_event_analysis_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shinydashboard::tabItem(
    tabName = "event_analysis",
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
    shinycssloaders::withSpinner(shiny::uiOutput(ns("tarmac_occurence")))
  )
}

# Module Server

mod_event_analysis_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    auth <- callModule(
      module =shinymanager::auth_server,
      id = "auth",
      session = session,
      check_credentials = shinymanager::check_credentials(
        data.frame(
          user = c("SQI Team", "lindsey"),
          password = c("colourandshape", "mess9044")
        )
        
      )
      
    )
    
    edData <- shiny::reactive({
      allEd(input$calSiteSelect)
    })
    
    caldata <- shiny::reactive({
      df <- edData() 
      
      df <- df |>
        dplyr::mutate(
          year_mo = format(date, "%Y-%m"),
          year = lubridate::year(lubridate::ym(.data$year_mo)),
          ed_arrival_date_time = parse_ed_time(arrival_datetime),
          date = parse_ed_time(arrival_datetime),
          Hour = lubridate::hour(date)
        )
      
      if (input$tarmac_labeled == "Yes") {
        df <- df |>
          dplyr::filter(
            stringr::str_detect(stringr::str_to_lower(discharge_comment),
                                stringr::regex("t[ar]{1,2}m[ac]{1,2}", ignore_case = TRUE)),
            !stringr::str_detect(discharge_comment, "Tamara"),
            !stringr::str_detect(discharge_comment, "tramadol"),
            !stringr::str_detect(discharge_comment, "Tramacet")
          )
      }
      
      if (input$view_mode == "by Year") {
        df <- dplyr::filter(df, year == input$year_select)
      }
      
      rg <- as.numeric(lubridate::hm(input$event_time_range)) / 3600
      if (rg[2] < 23) rg[2] <- rg[2] + 1
      df <- dplyr::filter(df, Hour >= rg[1], Hour <= rg[2])
      
      last <- dplyr::case_when(
        input$view_mode == "Past 3 Months" ~ 3,
        TRUE ~ 12
      )
      
      if (nrow(df) > 0) {
        newest_dt <- max(df$ed_arrival_date_time, na.rm = TRUE)
        cutoff_dt <- parse_ed_time(newest_dt) - months(last)
        df <- dplyr::filter(df, ed_arrival_date_time >= cutoff_dt)
      }
      
      if ("other" %in% input$ctas) {
        x <- as.numeric(input$ctas[!(input$ctas == "other")])
        out <- as.character(setdiff(1:5, x))
        df <- dplyr::filter(df, !(ctas_level %in% out))
      } else {
        df <- dplyr::filter(df, ctas_level %in% input$ctas)
      }
      
      df
    })
    
    caldata_summary <- shiny::reactive({
      caldata() |>
        dplyr::group_by(date) |>
        dplyr::count()
    })
    
    output$calheat <- shiny::renderPlot({
      df <- caldata_summary()
      if (nrow(df) == 0) {
        df <- dplyr::bind_rows(df, list("date" = lubridate::ymd(paste0(input$year_select, "-01-01")), "n" = NA))
      }
      calendarHeatmap(df$date, df$n, legpos = "none", site = input$calSiteSelect)
    })
    
    real_cal_data <- shiny::reactive({
      create_calendar()
    })
    
    output$calreal <- shiny::renderPlot({
      df <- real_cal_data()
      df2 <- caldata_summary()
      df <- dplyr::left_join(df, df2, by = "date")
      real_calendar_plot(latest_date = lubridate::ymd(lubridate::today()), df)
    })
    
    shiny::observeEvent(input$calSiteSelect, {
      df <- profileLoad()
      if (input$calSiteSelect != "All Tarmac Sites") {
        new <- df |>
          dplyr::filter(city == input$calSiteSelect) |>
          dplyr::select(Open, Close) |>
          unlist()
        
        if (new[2] == "NA") {
          shinyWidgets::updateSliderTextInput(
            session = session,
            inputId = "event_time_range",
            selected = paste0(c(0, 23), ":00")
          )
        } else {
          shinyWidgets::updateSliderTextInput(
            session = session,
            inputId = "event_time_range",
            selected = paste0(c(new[2] - 2, new[2]), ":00")
          )
        }
      }
    })
    
    output$records <- DT::renderDT({
      df <- caldata()
      df2 <- profileLoad()
      df <- dplyr::left_join(df, df2, by = c("facility_name" = "Data Code"))
      
      df |>
        dplyr::select(
          arrival_mode, ctas_level, city, arrival_datetime,
          disposition, discharge_comment,account_number_hash
        ) |>
        dplyr::mutate(Datetime = format(lubridate::ymd_hms(arrival_datetime), "%Y-%m-%d %H:%M:%S")) |>
        dplyr::select(-arrival_datetime) |>
        dplyr::rename(
          "Arrival Mode" = arrival_mode,
          "CTAS" = ctas_level,
          "Comment" = discharge_comment,
          "Discharge Disp" = disposition
        ) |>
        dplyr::arrange(dplyr::desc(Datetime))
    }, options = list(dom = "tip", pageLength = 5))
    
    output$event_summary <- shiny::renderUI({
      df <- caldata_summary()
      time <- if (input$view_mode == "Past 3 Months") {
        "the past 3 months,"
      } else {
        paste0(input$year_select, ",")
      }
      site <- if (input$calSiteSelect == "All Tarmac Sites") {
        "at <span style='color:#DE5428;'> all tarmac sites </span>"
      } else {
        paste0("in <span style='color:#DE5428;'>", input$calSiteSelect, "</span>")
      }
      shiny::HTML(paste(
        "<span style='font-size:20px; text-align:center; display:block;'><br>In ",
        time, "<br>between",
        "<span style='color:#005AA2;'>", input$event_time_range[1], "</span>",
        "and",
        "<span style='color:#005AA2;'>", input$event_time_range[2], "</span>",
        "<br> there were",
        "<span style='color:#DE5428;'>", sum(df$n), "</span> ED visits<br>",
        site,
        "</span>"
      ))
    })
    
    output$plot_ui <- shiny::renderUI({
      if ("Past 3 Months" %in% input$view_mode) {
        shiny::plotOutput(ns("calreal"), height = "170px")
      } else {
        shiny::plotOutput(ns("calheat"), height = "170px")
      }
    })
    
    output$tarmac_occurence <- shiny::renderUI({
      dfProf <- profileLoad() |>
        dplyr::filter(`ED Data Avail`) |>
        dplyr::select(`Hospital Name`, city)
      
      shiny::div(
        shiny::fluidRow(
          shinydashboard::box(
            width = 12,
            title = "Select Event Parameters",
            collapsible = TRUE,
            shiny::fluidRow(
              shiny::column(1, shiny::selectizeInput(ns("view_mode"), "View Mode:", choices = c("3 Mos." = "Past 3 Months", "Year" = "by Year"), selected = "Past 3 Months")),
              shiny::column(2, shiny::selectizeInput(ns("calSiteSelect"), "Select Site:", choices = c("All Tarmac Sites", sort(dfProf$city)), selected = "All Tarmac Sites")),
              shiny::column(2, shinyWidgets::sliderTextInput(inputId = ns("event_time_range"), label = "Event time range:", choices = paste0(0:23, ":00"), selected = c("0:00", "23:00"))),
              shiny::column(1, shinyWidgets::radioGroupButtons(ns("tarmac_labeled"), "Tarmac Label", choices = c("Yes", "No"))),
              shiny::column(2, shinyWidgets::checkboxGroupButtons(ns("ctas"), "CTAS", choices = c(1:5, "other"), selected = c(1:5, "other"))),
              shiny::column(2, shinyWidgets::radioGroupButtons(ns("arrv_mode"), "Arrival Mode", choices = c("Amb.", "Walk-In", "Both"), selected = "Both")),
              shiny::column(1, shiny::conditionalPanel(condition = paste0("input['", ns("view_mode"), "'] == 'by Year'"), shiny::selectizeInput(ns("year_select"), "Select Year:", choices = 2025:2022, selected = 2025)))
            )
          ),
          # shinydashboard::box(width = 12, title = "Event Heatmap", collapsible = TRUE, shinycssloaders::withSpinner(shiny::uiOutput(ns("plot_ui"))))
        ),
        shiny::fluidRow(
          shinydashboard::box(width = 9, title = "Individual Records", collapsible = TRUE, shinycssloaders::withSpinner(DT::DTOutput(ns("records")))),
          shinydashboard::box(width = 3, title = "Record Summary", collapsible = TRUE, shiny::uiOutput(ns("event_summary")))
        )
      )
    })
  })
}




## copy to body.R
# mod_event_analysis_ui("event_analysis_ui_1")

## copy to app_server.R
# callModule(mod_event_analysis_server, "event_analysis_ui_1")

## copy to sidebar.R
# menuItem("displayName",tabName = "event_analysis",icon = icon("user"))
