# options(
#   # whenever there is one account token found, use the cached token
#   gargle_oauth_email = TRUE,
#   # specify auth tokens should be stored in a hidden directory ".secrets"
#   gargle_oauth_cache = "./app_data/.secrets"
# )

tarmacFilter <- function(df) {
  df %>%
    dplyr::filter(
      stringr::str_detect(
        stringr::str_to_lower(discharge_comment),
        stringr::regex("t[ar]{1,2}m[ac]{1,2}", ignore_case = TRUE)
      ),
      !stringr::str_detect(discharge_comment, stringr::regex("Tamara", ignore_case = TRUE)),
      !stringr::str_detect(discharge_comment, stringr::regex("tramadol", ignore_case = TRUE)),
      !stringr::str_detect(discharge_comment, stringr::regex("Tramacet", ignore_case = TRUE))
    )
}


tarmac_boxUI <- function(ns){
  renderUI({
    fluidRow(
      # box(
      #   title = "Value Card",
      #   width = 4,
      #   status = "primary",
      #   solidHeader = TRUE,
      #   collapsible = TRUE,
      #   closable = FALSE,
      #   label = tagList(icon("info-circle"), "Dynamic"),
      valueBoxOutput(ns("tarmacs_per_year"), width = 2)
      # )
    )
  })
  
}

summaryBarPlot <- function(dfT, color_palette = NULL, plot.type = 'bar') {
  # Convert the date column to Date type
  data <- dfT %>%
    mutate(
      arrival_datetime = ymd_hms(arrival_datetime),
      year_mo = format(date, "%Y-%m"),
      year_month = ym(year_mo) # Extract year-month
    )
  
  # Get the range for the last 12 months
  end_date <- floor_date(Sys.Date(), "month")
  start_date <- end_date - months(11)
  
  # Generate a complete year-month sequence
  all_months <- seq.Date(start_date, end_date, by = "month")
  
  # Create a summary table with counts
  summary_data <- data %>%
    dplyr::filter(year_month >= start_date & year_month <= end_date) %>%
    dplyr::group_by(city, year_month) %>%
    dplyr::summarise(count = n(), .groups = "drop") %>%
    tidyr::complete(
      city,
      year_month = all_months,
      fill = list(count = 0)
    )
  
  # Ensure year_month is treated as a factor ordered by date
  summary_data <- summary_data %>%
    mutate(year_month = factor(format(year_month, "%b %Y"), levels = format(all_months, "%b %Y")))
  
  # If no custom color palette is provided, use a default one
  if (is.null(color_palette)) {
    color_palette <- RColorBrewer::brewer.pal(length(unique(summary_data$city)), "Set3")
  }
  
  city_cols  <-
    c(
      "Castlegar"=  "#1f77b4", # blue
      "Sparwood" =  "#f5c647",
      "Chase" = "#17becf",  # cyan
      "Keremeos" = "#F15A25"  # cyan
    )
  
  x <- summary_data |> group_by(year_month) |> summarise(n = sum(count))
  # Plotly stacked bar chart with custom color palette
  p <- summary_data %>%
    plotly::plot_ly(
      x = ~year_month,
      y = ~count,
      color = ~city,
      colors = city_cols,
      type = "bar",
      hovertext = ~paste(year_month, "<br>Community:", city, "<br>Count:", count),  # ✅ Use hovertext
      hoverinfo = "text"
    ) %>%
    plotly::layout(
      barmode = "stack",  # Stack the bars instead of grouping
      xaxis = list(
        title = "",
        tickformat = "%b %Y",  # Month abbreviation and year
        tickangle = -45,       # Optional: Rotate labels for better visibility
        showticks = TRUE,      # Show tick marks
        ticks = "inside"       # Optional: Put the ticks inside the plot area
      ),
      yaxis = list(
        title = "Event Count",
        tickmode = "array",    # Ensure only integer breaks
        tickvals = seq(0, max(x$n), by = 1)  # Integer tick values
      ),
      title = "",
      legend = list(
        title = list(text = ""), # Optionally change legend title
        tracegroupgap = 5 # Optional: Adjust spacing between groups in the legend
      )
    )

  
  if(plot.type == 'control'){
  # --- your data prep (unchanged) ---
  df <- summary_data %>%
    group_by(year_month) |>
    summarise(count = sum(count)) |>
    mutate(year_month = factor(year_month, levels = unique(year_month)))
  
  df_stats <- df %>%
    mutate(
      mean_count = mean(count),
      sd_count = sd(count),
      UCL = mean_count + 3 * sd_count,
      LCL = pmax(mean_count - 3 * sd_count, 0)
    )
  
  # --- colors ---
  col_count <- "#1f77b4"  # blue
  col_mean  <- "black"  # green
  col_ucl   <- "#F15A25"  # red
  col_lcl   <- "#F15A25"  # amber
  
  # --- build plot ---
  p <- plotly::plot_ly(df_stats, x = ~year_month) %>%
    # counts
    plotly::add_trace(
      y = ~count,
      type = "scatter", mode = "lines+markers",
      line = list(width = 2, color = col_count),
      marker = list(size = 7),
      hovertemplate = "Month: %{x}<br><b>Count</b>: %{y}<extra></extra>",
      name = "Count", showlegend = FALSE
    ) %>%
    # mean
    plotly::add_lines(
      y = ~mean_count,
      line = list(dash = "dash", width = 2, color = col_mean),
      hovertemplate = "<b>Mean</b>: %{y:.2f}<extra></extra>",
      name = "Mean", showlegend = FALSE
    ) %>%
    # UCL
    plotly::add_lines(
      y = ~UCL,
      line = list(dash = "dot", width = 2, color = col_ucl),
      hovertemplate = "<b>UCL</b>: %{y:.2f}<extra></extra>",
      name = "UCL", showlegend = FALSE
    ) %>%
    # LCL
    plotly::add_lines(
      y = ~LCL,
      line = list(dash = "dot", width = 2, color = col_lcl),
      hovertemplate = "<b>LCL</b>: %{y:.2f}<extra></extra>",
      name = "LCL", showlegend = FALSE
    )
  
  # --- right-edge labels for mean/UCL/LCL ---
  last_x <- tail(df_stats$year_month, 1)
  last_vals <- df_stats %>% slice_tail(n = 1)
  
  p <- p %>%
    plotly::add_annotations(
      x = last_x, y = last_vals$mean_count,
      text = paste0("Mean: ", round(last_vals$mean_count, 2)),
      xanchor = "left", yanchor = "middle",
      ax = 15, ay = 0, showarrow = TRUE,
      arrowcolor = col_mean, font = list(color = col_mean)
    ) %>%
    plotly::add_annotations(
      x = last_x, y = last_vals$UCL,
      text = paste0("UCL: ", round(last_vals$UCL, 2)),
      xanchor = "left", yanchor = "middle",
      ax = 15, ay = 0, showarrow = TRUE,
      arrowcolor = col_ucl, font = list(color = col_ucl)
    ) %>%
    plotly::add_annotations(
      x = last_x, y = last_vals$LCL,
      text = paste0("LCL: ", round(last_vals$LCL, 2)),
      xanchor = "left", yanchor = "middle",
      ax = 15, ay = 0, showarrow = TRUE,
      arrowcolor = col_lcl, font = list(color = col_lcl)
    )
  
  # --- layout & hover style ---
  p <- plotly::layout(
    p,
    title = "Monthly Tarmac Events",
    xaxis = list(title = "Month", tickangle = -30),
    yaxis = list(title = "Count", rangemode = "tozero"),
    hovermode = "x unified",
    hoverlabel = list(bgcolor = "white", font = list(color = "black"))
  )
  }
  return(p)
}

allEd <- function(site = "All Tarmac Sites") {
  dfProf <- profileLoad()
  cities <- if (site == "All Tarmac Sites") {
    dfProf %>%
      filter(`Active Tarmac` == "Yes") %>%
      pull(city)
  } else {
    site
  }
  
  paths <- filenameCreate(dfProf, cities)
  purrr::map_dfr(paths, data.table::fread, colClasses = 'character') %>%
    mutate(datetime = parse_ed_time(arrival_datetime),
           date = as.Date(datetime),
           weekdate = weekdate(date),
           Hour = hour(datetime),
           time = format(datetime, "%H:%M:%S"),
           Year = year(date))
}

profileLoad_raw <- function() {
  # googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/19yf8Ty3si6XHjKRw6I6qyZ3CZl52ZY2q_7Q_dTJAR28/edit?gid=0#gid=0')
  readr::read_csv('app_data/site_profiles.csv')
}

# cache to memory or disk (disk persists across calls in a process)
cache <- cachem::cache_disk(dir = "shiny_cache")  # or cachem::cache_mem()
profileLoad <- memoise::memoise(profileLoad_raw, cache = cache)

# if you ever need to bust cache:
# memoise::forget(profileLoad)


filenameCreate <- function(dfProf, site, from.google = FALSE){
  z <- dfProf |>
    dplyr::filter(city %in% site) |>
    dplyr::select(`Data Code`) |> unlist()
  if(from.google){
    path <-  paste0(z, '.csv')
  }else{
    path <- paste0('./app_data/ed/', z, '.csv')
  }
  path
}

parse_ed_time <- function(date_vec) {
  # Try known formats in order of likelihood
  parsed <- parse_date_time(
    date_vec,
    orders = c("ymd HMS", "ymd HM", "ymd", "mdy HMS", "mdy HM", "mdy",
               "dmy HMS", "dmy HM", "dmy", "Ymd HMS", "Ymd"),
    exact = FALSE,
    tz = "UTC"
  )
  return(parsed)
}
weekdate <- function(date) {
  lubridate::floor_date(date, unit = "week", week_start = 1)  # 1 = Monday
}


load_tarmac_data <- function(df){
  dfP <- profileLoad()
  dfP <- dfP |>
    rename('facility_name' = "Data Code")
  dfT <- tarmacFilter(df)
  left_join(dfT, dfP)
}