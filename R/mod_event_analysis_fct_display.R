tarmac_occurenceUI <- function(ns, input_view_mode) {
  if (input_view_mode == "3 Months") {
    shiny::plotOutput(ns("calreal"), height = "170px")
  } else if (input_view_mode == "by Year") {
    shiny::plotOutput(ns("calheat"), height = "170px")
  }
}

calendarHeatmap <- function(dates, values, title = "", subtitle = "", legendtitle = "", legpos = "bottom", site){
  if (missing(dates)) {
    base::stop("Need to specify a dates vector.")
  }
  if (missing(values)) {
    base::stop("Need to specify a values vector.")
  }
  if (!lubridate::is.Date(dates)) {
    base::stop("dates vector need to be in Date format.")
  }
  if (base::length(dates) != base::length(values)) {
    base::stop("dates and values need to have the same length.")
  }
  
  my_theme <- function() {
    color.background <- "white"
    color.text <- "#22211d"
    otherSize <- 16
    
    ggplot2::theme_bw(base_size = 18) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = color.background, color = color.background)) +
      ggplot2::theme(plot.background  = ggplot2::element_rect(fill = color.background, color = color.background)) +
      ggplot2::theme(panel.border     = ggplot2::element_rect(color = color.background)) +
      ggplot2::theme(strip.background = ggplot2::element_rect(fill = color.background, color = color.background)) +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank()) +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
      ggplot2::theme(axis.ticks       = ggplot2::element_blank()) +
      ggplot2::theme(legend.position = legpos) +
      ggplot2::theme(legend.text = ggplot2::element_text(size = 8, color = color.text)) +
      ggplot2::theme(plot.title = ggplot2::element_blank(), plot.subtitle = ggplot2::element_blank()) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = otherSize, hjust = 0, color = color.text)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = otherSize, color = color.text)) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = otherSize + 4, color = "black", face = "bold")) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = otherSize + 4, color = "black", vjust = 1.25)) +
      ggplot2::theme(strip.text = ggplot2::element_text(face = "bold")) +
      ggplot2::theme(plot.margin = grid::unit(c(0.1, 0.2, 0.3, 0.35), "cm"))
  }
  
  min.date <- base::as.Date(base::paste(base::format(base::min(dates), "%Y"), "-01-01", sep = ""))
  max.date <- base::as.Date(base::paste(base::format(base::max(dates), "%Y"), "-12-31", sep = ""))
  df <- base::data.frame(date = base::seq(min.date, max.date, by = "days"), value = NA)
  df$value[base::match(dates, df$date)] <- values
  df$year <- base::as.factor(base::format(df$date, "%Y"))
  df$month <- base::as.numeric(base::format(df$date, "%m"))
  df$doy <- base::as.numeric(base::format(df$date, "%j"))
  df$dow <- base::as.numeric(base::format(df$date, "%w"))
  df$woy <- base::as.numeric(base::format(df$date, "%U")) + 1
  df$dowmapped <- base::ordered(df$dow, levels = 6:0)
  base::levels(df$dowmapped) <- base::rev(c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
  
  g <- ggplot2::ggplot(df, ggplot2::aes(woy, dowmapped, fill = value)) +
    ggplot2::geom_tile(colour = "darkgrey") +
    ggplot2::facet_wrap(~year, ncol = 1, labeller = ggplot2::labeller(year = function(x) base::paste(x, "-", site))) +
    ggplot2::coord_equal(xlim = c(2.5, 54)) +
    ggplot2::scale_x_continuous(breaks = 53 / 12 * (1:12) - 1.5, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
    my_theme() +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::scale_fill_gradientn(colours = c("white", "#107AB0"), na.value = "white", name = legendtitle,
                                  guide = ggplot2::guide_colorbar(direction = "horizontal",
                                                                  barheight = grid::unit(2, units = "mm"),
                                                                  barwidth = grid::unit(75, units = "mm"),
                                                                  title.position = "top",
                                                                  title.hjust = 0.5)) +
    ggplot2::labs(x = NULL, y = NULL, title = title, subtitle = subtitle)
  
  return(g)
}

create_calendar <- function(holidays = c()) {
  start_date <- base::as.Date("2021-01-01")
  end_date <- base::as.Date("2027-12-31")
  abb_order <- base::format(base::seq(start_date, end_date, by = "month"), format = "%Y-%B")
  
  calendar <- tibble::tibble(date = base::seq.Date(start_date, end_date, by = "day"))
  
  calendar <- calendar %>%
    dplyr::mutate(
      month = stringr::str_to_title(lubridate::month(date, label = TRUE, abbr = FALSE)),
      month = base::factor(month, levels = base::unique(month)),
      week = lubridate::isoweek(date) + dplyr::if_else(lubridate::wday(date) == 1, 1, 0),
      week = dplyr::if_else(week > 53, week - 53, week),
      day = lubridate::day(date),
      weekday = base::weekdays(date, abbreviate = TRUE),
      weekend_holydays = dplyr::if_else(lubridate::wday(date) %in% c(7, 1), "weekend", "workdays"),
      weekend_holydays = dplyr::if_else(date %in% holidays, "holidays", weekend_holydays),
      week_month = dplyr::dense_rank(week)
    ) %>%
    dplyr::group_by(month) %>%
    dplyr::mutate(week_month = dplyr::dense_rank(week)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      year_mo = base::format(date, format = "%Y-%m"),
      year_mo_abb = base::format(date, format = "%Y-%B"),
      year_mo_abb = base::factor(year_mo_abb, levels = abb_order)
    )
  
  return(calendar)
}

get_last_three_months <- function(date, mo_count = 3) {
  date <- base::as.Date(date)
  last_three_months <- base::seq(date %m-% months(mo_count - 1), by = "month", length.out = mo_count)
  year_month_strings <- base::format(last_three_months, "%Y-%m")
  return(year_month_strings)
}

real_calendar_plot <- function(latest_date = lubridate::today(), calendar, mo_count = 3, background = "#fffefa", weekday_color = "#fffefa", holiday_color = "#fffefa", weekend_color = "#e7e9ee", font_base = "Lato", font_title = "Lato Black") {
  extrafont::loadfonts(device = "win")
  
  abbreviated_weekdays <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
  
  calendar %>%
    dplyr::filter(year_mo %in% get_last_three_months(latest_date, mo_count)) %>%
    dplyr::mutate(
      weekday = base::factor(weekday, abbreviated_weekdays),
      event = !base::is.na(n)
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = weekday, y = week_month)) +
    ggplot2::geom_tile(alpha = 0.8, ggplot2::aes(fill = event), color = "grey30", size = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = day), family = font_base, size = 4) +
    ggplot2::facet_wrap(~year_mo_abb, scales = "free_x", ncol = 3) +
    ggplot2::scale_x_discrete() +
    ggplot2::scale_y_reverse(breaks = NULL) +
    ggplot2::labs(fill = NULL) +
    ggplot2::theme_void() +
    ggplot2::scale_fill_manual(values = c("#fffefa", "#107AB0")) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(1, 0.5, 1, 1, "cm"),
      axis.text.x = ggplot2::element_text(color = "black", family = font_base, size = 12, vjust = 2),
      plot.title = ggplot2::element_text(hjust = 0.5, vjust = 3, color = weekday_color, family = font_title, size = 45),
      strip.text.x = ggplot2::element_text(size = 20, face = "bold", color = "black", family = font_base),
      plot.caption = ggplot2::element_text(hjust = 1, color = weekday_color, family = font_base, size = 8),
      legend.position = "none",
      panel.spacing = grid::unit(0.5, "lines"),
      panel.background = ggplot2::element_rect(fill = background, color = background),
      plot.background = ggplot2::element_rect(fill = background, color = background)
    )
}
