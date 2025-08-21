mod_ai_forecast_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3,
             selectInput(ns("site"), "Site", choices = NULL),
             checkboxGroupInput(ns("ctas"), "CTAS levels", choices = c(2,3,4), selected = c(2,3,4), inline = TRUE),
             dateRangeInput(ns("dates"), "Date range", start = Sys.Date() - 30, end = Sys.Date() + 30),
             sliderInput(ns("tod"), "Time of day window", min = 0, max = 24, value = c(16,18), step = 1),
             actionButton(ns("go"), "Predict")
      ),
      # in mod_ai_forecast_ui(), replace the whole column(9, ...) block with:
      # replace your right column with this
      column(9,
             fluidRow(
               shinydashboard::valueBoxOutput(ns("kpi_total"), width = 4),
               shinydashboard::valueBoxOutput(ns("kpi_daily"), width = 4),
               shinydashboard::box(
                 width = 4, title = "Top 3 likely date/time slots",
                 # status = "success", 
                 solidHeader = TRUE,
                 div(style = "font-size:12px;margin-top:-8px;color:#000000;",
                     "Ranked by expected arrivals in the selected hours"),
                 DT::DTOutput(ns("top3_times"), height = "140px")
               )
             ),
             uiOutput(ns("narrative")),
             plotOutput(ns("hourly_heatmap"), height = 260)
             # (table removed)
      )
      
    )
  )
}
mod_ai_forecast_server <- function(id, hourly_df, fitted_model, xlev = NULL) {
  moduleServer(id, function(input, output, session) {
    requireNamespace("ggplot2"); requireNamespace("DT"); requireNamespace("dplyr")
    
    observe({
      sites <- sort(unique(hourly_df$site))
      updateSelectInput(session, "site", choices = sites, selected = sites[1])
    })
    
    forecast_reactive <- reactive({
      req(input$site, input$ctas, input$dates, input$tod)
      predict_counts(
        model      = fitted_model,
        hourly_df  = hourly_df,
        site       = input$site,
        ctas_vec   = as.integer(input$ctas),
        start_date = input$dates[1],
        end_date   = input$dates[2],
        from_hour  = input$tod[1],
        to_hour    = input$tod[2],
        xlev       = xlev
      )
    }) |>
      bindCache(
        input$site,
        paste(sort(as.integer(input$ctas)), collapse = ","),
        input$dates, input$tod
      ) |>
      bindEvent(input$go, ignoreInit = TRUE)
    
    # ----- KPIs -----
    output$kpi_total <- shinydashboard::renderValueBox({
      req(forecast_reactive())
      fr  <- forecast_reactive()
      mu_total <- sum(fr$daily_window$expected_count)
      
      # 80% PI for the TOTAL over selected window (simulate)
      hrw <- fr$hourly %>% dplyr::filter(hod >= input$tod[1], hod < input$tod[2])
      pi  <- nb_sim_pi(mu_vec = hrw$pred, theta = fitted_model$theta, sims = 2000L)
      
      shinydashboard::valueBox(
        value = fmt_count(mu_total),
        subtitle = sprintf("Total over range (80%% PI: %s–%s)",
                           fmt_count(pi["low"]), fmt_count(pi["high"])),
        icon = icon("truck-medical"), color = "purple"
      )
    })
    
    output$kpi_daily <- shinydashboard::renderValueBox({
      req(forecast_reactive())
      mu_daily <- forecast_reactive()$daily_window$expected_count
      # Clearer “typical day”: average per day and most-days range (10th–90th pct)
      avg_day <- mean(mu_daily)
      rng     <- quantile(mu_daily, c(0.10, 0.90), na.rm = TRUE)
      shinydashboard::valueBox(
        value = fmt_count(avg_day),
        subtitle = sprintf("Avg per day; most days %s–%s",
                           fmt_count(rng[[1]]), fmt_count(rng[[2]])),
        icon = icon("calendar-day"), color = "light-blue"
      )
    })
    
    # ----- Narrative -----
    output$narrative <- renderUI({
      req(forecast_reactive())
      fr    <- forecast_reactive()
      site  <- input$site
      mu_tot <- round(sum(fr$daily_window$expected_count))
      hrw   <- fr$hourly %>% dplyr::filter(hod >= input$tod[1], hod < input$tod[2])
      pi    <- nb_sim_pi(mu_vec = hrw$pred, theta = fitted_model$theta, sims = 2000L)
      days  <- paste(fmt_date(input$dates[1]), "to", fmt_date(input$dates[2]))
      hours <- paste0(sprintf("%02d:00", input$tod[1]), "–", sprintf("%02d:00", input$tod[2]))
      HTML(sprintf("
        <h4>What this means</h4>
        <p>At <b>%s</b>, between <b>%s</b>, we expect about <b>%d</b> ambulance arrivals with CTAS %s over <b>%s</b>
        (<b>%d–%d</b> with 80%% confidence). That’s about <b>%s per day</b> on average.</p>",
                   site, hours, mu_tot, paste(sort(as.integer(input$ctas)), collapse = "/"), days,
                   round(pi['low']), round(pi['high']), fmt_count(mean(fr$daily_window$expected_count))
      ))
    })
    
    # ----- Heatmap (kept) -----
    output$hourly_heatmap <- renderPlot({
      req(forecast_reactive())
      hr <- forecast_reactive()$hourly %>%
        dplyr::mutate(date = as.Date(hour_ts)) %>%
        dplyr::group_by(date, hod) %>%
        dplyr::summarise(pred = sum(pred), .groups = "drop")
      ggplot2::ggplot(hr, ggplot2::aes(x = hod, y = date, fill = pred)) +
        ggplot2::geom_tile() +
        ggplot2::scale_x_continuous(breaks = seq(0, 23, by = 3)) +
        ggplot2::labs(x = "Hour of day", y = "Date", fill = "Expected")
    })
    
    # ----- Top 3 date/time slots -----
    output$top3_times <- DT::renderDT({
      req(forecast_reactive())
      tz <- "America/Vancouver"
      hrw <- forecast_reactive()$hourly %>%
        dplyr::filter(hod >= input$tod[1], hod < input$tod[2]) %>%
        dplyr::group_by(hour_ts) %>%
        dplyr::summarise(expected = sum(pred), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(expected)) %>%
        dplyr::slice_head(n = 3) %>%
        dplyr::mutate(
          date = fmt_date(lubridate::with_tz(hour_ts, tz)),
          time = fmt_time(lubridate::with_tz(hour_ts, tz)),
          expected = fmt_count(expected)
        ) %>%
        dplyr::select(date, time, expected)
      
      DT::datatable(
        hrw,
        options = list(dom = "t", paging = FALSE),
        rownames = FALSE
      )
    })
    
    # ----- Daily table -----
    output$daily_table <- DT::renderDT({
      req(forecast_reactive())
      forecast_reactive()$daily_window %>%
        dplyr::mutate(
          date = fmt_date(date),
          expected_count = fmt_count(expected_count)
        ) %>%
        dplyr::rename(`expected_count` = expected_count) %>%
        DT::datatable(options = list(pageLength = 10), rownames = FALSE)
    })
  })
}

## copy to body.R
# mod_ai_forecast_ui("ai_forecast_ui_1")
 
## copy to app_server.R
# callModule(mod_ai_forecast_server, "ai_forecast_ui_1")
 
## copy to sidebar.R
# menuItem("displayName",tabName = "ai_forecast",icon = icon("user"))
 
