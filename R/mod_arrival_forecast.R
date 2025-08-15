# Module UI
 
#' @title mod_arrival_forecast_ui and mod_arrival_forecast_server
#' @description A shiny module.
 
mod_arrival_forecast_ui <- function(id) {
  ns <- NS(id)
  shinydashboard::tabItem(
    tabName = "arrival_forecast",
    fluidRow(
      box(
        title = div(
          class = "d-flex justify-content-between align-items-center",
          span("Arrival forecast (next 8 hours)"),
          div(
            id = ns("badge_wrap"),
            tags$span(id = ns("cal_badge"), class = "label label-default", "Calibration: —"),
            HTML("&nbsp;"),
            tags$span(id = ns("base_rate"), class = "label label-info", "This hour historically: —")
          )
        ),
        width = 12, status = "primary", solidHeader = TRUE,
        fluidRow(
          column(
            width = 8,
            plotly::plotlyOutput(ns("forecast_strip"), height = 150),
            div(
              class = "small text-muted mt-2",
              HTML(
                sprintf(
                "<strong>Legend:</strong> High ≥ <span id=\"%s\"></span>, Medium <span id=\"%s\"></span>–<span id=\"%s\"></span>, Low &lt; <span id=\"%s\"></span>",
                ns("th_high_val"), ns("th_med_lo_val"), ns("th_med_hi_val"), ns("th_low_val")
                )
                )
              )
            ),
          column(
            width = 4,
            valueBoxOutput(ns("vbox_next_hour"), width = 12),
            valueBoxOutput(ns("vbox_conf"), width = 12)
          )
        )
      )
    ),
    
    fluidRow(
      box(
        title = "If arrives: expected resource footprint",
        width = 12, status = "info", solidHeader = TRUE,
        fluidRow(
          column(4, infoBoxOutput(ns("ibox_bed"), width = 12)),
          column(4, infoBoxOutput(ns("ibox_imaging"), width = 12)),
          column(4, infoBoxOutput(ns("ibox_consult"), width = 12))
        ),
        div(
          class = "mt-2",
          actionButton(ns("act_ready"), "I prepped bed", class = "btn btn-primary"),
          HTML("&nbsp;"),
          actionButton(ns("act_notify"), "I notified team", class = "btn btn-default")
        )
      )
    ),
    
    fluidRow(
      box(
        title = "Why is risk up/down right now?",
        width = 12, status = "warning", solidHeader = TRUE,
        uiOutput(ns("explain_chips")),
        div(class = "mt-3", plotly::plotlyOutput(ns("last10_spark"), height = 130)),
        div(
          class = "mt-3",
          textAreaInput(
            ns("override_reason"),
            label = "Override / local knowledge (optional)",
            placeholder = "e.g., CT down for maintenance; wildfire affecting EMS travel times",
            rows = 2
          ),
          actionButton(ns("save_override"), "Record override", class = "btn btn-warning")
        )
      )
    )
  )
}
 
# Module Server
 
mod_arrival_forecast_server <- function(input, output, session) {
	ns <- session$ns
	# Validate deps
	if (!requireNamespace("plotly", quietly = TRUE)) stop("plotly is required")
	if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 is required")
	
	# Make thresholds visible in legend (requires useShinyjs() in the page)
	observe({
	  shinyjs::html("th_high_val", sprintf("%d%%", round(thresholds$high * 100)))
	  shinyjs::html("th_med_lo_val", sprintf("%d%%", round(thresholds$low * 100)))
	  shinyjs::html("th_med_hi_val", sprintf("%d%%", round(thresholds$high * 100 - 1)))
	  shinyjs::html("th_low_val", sprintf("%d%%", round(thresholds$low * 100)))
	})
	
	# Pull forecasts initially and on a timer
	get_fc <- reactivePoll(
	  intervalMillis = poll_secs * 1000,
	  session = session,
	  checkFunc = function() Sys.time(), # replace with a data-version endpoint if you have one
	  valueFunc = function() {
	    req(is.function(forecast_provider))
	    req(is.reactive(filters_reactive))
	    filters <- filters_reactive()
	    fc <- forecast_provider(filters)
	    validate(need(all(c(
	      "hour","p","p_low","p_high","top_factors","bed_type",
	      "imaging_likelihood","consult","calibration_brier","hist_base_rate"
	    ) %in% names(fc)), "forecast_provider() returned an unexpected schema"))
	    dplyr::arrange(fc, .data$hour)
	  }
	)
	
	# Headline metrics for the next hour
	next_row <- reactive({
	  fc <- get_fc()
	  now_hr <- lubridate::floor_date(Sys.time(), unit = "hour")
	  dplyr::filter(fc, .data$hour == now_hr) |> dplyr::slice_tail(n = 1)
	})
	
	output$vbox_next_hour <- renderValueBox({
	  n <- next_row(); req(n$p)
	  shinydashboard::valueBox(
	    value = sprintf("%d%%", round(100 * n$p)),
	    subtitle = "Next hour",
	    icon = icon("area-chart"),
	    color = if (n$p >= thresholds$high) "red" else if (n$p >= thresholds$low) "yellow" else "aqua"
	  )
	})
	
	output$vbox_conf <- renderValueBox({
	  n <- next_row(); req(n$p_low, n$p_high)
	  shinydashboard::valueBox(
	    value = sprintf("%d–%d%%", round(100 * n$p_low), round(100 * n$p_high)),
	    subtitle = "Confidence (range)",
	    icon = icon("shield"), color = "teal"
	  )
	})
	
	# Resource footprint infoBoxes
	output$ibox_bed <- renderInfoBox({
	  n <- next_row()
	  shinydashboard::infoBox("Bed type", n$bed_type %||% "—", icon = icon("bed"), color = "light-blue", fill = TRUE)
	})
	output$ibox_imaging <- renderInfoBox({
	  n <- next_row()
	  shinydashboard::infoBox("Imaging likelihood", n$imaging_likelihood %||% "—", icon = icon("camera"), color = "light-blue", fill = TRUE)
	})
	output$ibox_consult <- renderInfoBox({
	  n <- next_row()
	  shinydashboard::infoBox("Likely consult", n$consult %||% "—", icon = icon("user-md"), color = "light-blue", fill = TRUE)
	})
	
	# Calibration + base-rate badges (simple text updates)
	observe({
	  n <- next_row(); req(n$calibration_brier, n$hist_base_rate)
	  shinyjs::html("cal_badge", sprintf("Brier: %.3f", n$calibration_brier))
	  shinyjs::html("base_rate", sprintf("This hour historically: %d%%", round(100 * n$hist_base_rate)))
	})
	
	# Forecast strip plot
	output$forecast_strip <- plotly::renderPlotly({
	  fc <- get_fc(); req(nrow(fc) > 0)
	  df <- dplyr::mutate(fc, band = dplyr::case_when(
	    .data$p >= thresholds$high ~ "High",
	    .data$p >= thresholds$low  ~ "Medium",
	    TRUE ~ "Low"
	  ))
	  
	  g <- ggplot2::ggplot(df, ggplot2::aes(x = .data$hour, y = .data$p)) +
	    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$p_low, ymax = .data$p_high), alpha = 0.2) +
	    ggplot2::geom_col(ggplot2::aes(fill = .data$band), width = 0.035) +
	    ggplot2::geom_point() +
	    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
	    ggplot2::labs(x = NULL, y = NULL) +
	    ggplot2::theme_minimal(base_size = 12) +
	    ggplot2::theme(legend.position = "none", panel.grid.minor = ggplot2::element_blank())
	  
	  plotly::ggplotly(g, tooltip = c("x","y")) |> plotly::config(displayModeBar = FALSE)
	})
	
	# Explanation chips
	output$explain_chips <- renderUI({
	  n <- next_row(); req(n$top_factors)
	  factors <- tryCatch({
	    if (is.character(n$top_factors)) jsonlite::fromJSON(n$top_factors) else n$top_factors
	  }, error = function(e) list())
	  if (length(factors) == 0) return(tags$div(span(class = "label label-default", "No drivers available")))
	  div(
	    lapply(factors, function(x){
	      lab <- if (!is.null(x$label)) x$label else as.character(x)
	      span(class = "label label-info", style = "margin-right:6px; display:inline-block;", lab)
	    })
	  )
	})
	
	# Last 10 predictions vs truth sparkline (if truth available)
	output$last10_spark <- plotly::renderPlotly({
	  fc <- get_fc()
	  df <- dplyr::slice_tail(fc, n = 10) |> dplyr::mutate(obs = .data$observed %||% NA_real_)
	  
	  g <- ggplot2::ggplot(df, ggplot2::aes(x = .data$hour)) +
	    ggplot2::geom_line(ggplot2::aes(y = .data$p)) +
	    ggplot2::geom_point(ggplot2::aes(y = .data$p)) +
	    ggplot2::geom_point(ggplot2::aes(y = .data$obs), shape = 4, size = 2, na.rm = TRUE) +
	    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
	    ggplot2::labs(x = NULL, y = NULL) +
	    ggplot2::theme_minimal(base_size = 11) +
	    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
	  
	  plotly::ggplotly(g, tooltip = c("x","y")) |> plotly::config(displayModeBar = FALSE)
	})
	
	# Action click logging (stub: print; replace with DB/API write)
	observeEvent(input$act_ready,  { n <- next_row(); cat("ACTION: prepped bed @", Sys.time(), "p=", n$p, "
") })
	observeEvent(input$act_notify, { n <- next_row(); cat("ACTION: notified team @", Sys.time(), "p=", n$p, "
") })
	
	observeEvent(input$save_override, {
	  msg <- input$override_reason
	  if (nzchar(msg)) {
	    n <- next_row();
	    cat("OVERRIDE:", format(Sys.time()), "—", msg, "(next hour p=", round(n$p,3), ")
")
	    showNotification("Override recorded", type = "message")
	  } else {
	    showNotification("Please enter a reason before saving.", type = "warning")
	  }
	})
	
}
 
## copy to body.R
# mod_arrival_forecast_ui("arrival_forecast_ui_1")
 
## copy to app_server.R
# callModule(mod_arrival_forecast_server, "arrival_forecast_ui_1")
 
## copy to sidebar.R
# menuItem("displayName",tabName = "arrival_forecast",icon = icon("user"))
 
