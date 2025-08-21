# R/ai_forecast.R
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)
library(slider)
library(MASS)      # glm.nb
library(splines)   # optional if you want splines
# If you have a BC holiday helper, plug it in; otherwise skip the holiday flag.

# R/ai_loader.R
library(readr)
library(dplyr)
library(purrr)
library(tools)

read_all_sites <- function(dir_path) {
  files <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE)
  if (length(files) == 0) stop("No CSVs found in: ", dir_path)
  
  purrr::map_dfr(files, function(f) {
    site_from_file <- file_path_sans_ext(basename(f))
    readr::read_csv(
      f,
      col_types = readr::cols(.default = readr::col_character()), # <- force all char
      guess_max = 100000,
      show_col_types = FALSE
    ) %>%
      mutate(facility_name = dplyr::coalesce(facility_name, site_from_file))
  })
}

# Data prep & feature engineering -----------------------------------------
# ---- Put in R/ai_forecast.R ----
library(dplyr); library(tidyr); library(lubridate); library(stringr)
library(purrr); library(slider); library(MASS)

normalize_arrival_mode <- function(x) {
  x <- tolower(ifelse(is.na(x), "", x))
  grepl("ambul|bcehs|paramedic|ems", x)
}

prep_hourly_counts <- function(raw_df) {
  df <- raw_df %>%
    mutate(
      arrival_ts = coalesce(
        ymd_hms(arrival_datetime, quiet = TRUE),
        ymd_hms(ed_visit_start_date_time_for_beginning_time, quiet = TRUE),
        ymd_hms(pia_date_time, quiet = TRUE)
      ),
      site = facility_name,
      ctas = suppressWarnings(as.integer(ctas_level)),
      amb  = normalize_arrival_mode(arrival_mode)
    ) %>%
    filter(!is.na(arrival_ts), !is.na(ctas), ctas %in% 1:5, !is.na(site), site != "") %>%
    mutate(hour_ts = floor_date(with_tz(arrival_ts, "America/Vancouver"), "hour")) %>%
    distinct(account_number_hash, .keep_all = TRUE)
  
  hourly <- df %>%
    count(site, hour_ts, ctas, amb, name = "y") %>%
    group_by(site) %>%
    complete(
      hour_ts = seq(floor_date(min(hour_ts), "hour"),
                    ceiling_date(max(hour_ts), "hour") - hours(1),
                    by = "1 hour"),
      ctas = 1:5, amb = c(FALSE, TRUE),
      fill = list(y = 0L)
    ) %>% ungroup()
  
  hourly %>%
    mutate(
      dow = wday(hour_ts, week_start = 1),
      hod = hour(hour_ts),
      woy = isoweek(hour_ts),
      sin_hod = sin(2*pi*hod/24),  cos_hod = cos(2*pi*hod/24),
      sin_woy = sin(2*pi*woy/53),  cos_woy = cos(2*pi*woy/53)
    ) %>%
    group_by(site, ctas, amb) %>%
    arrange(hour_ts, .by_group = TRUE) %>%
    mutate(
      lag_24  = lag(y, 24),
      lag_168 = lag(y, 168),
      ma_7d   = slide_dbl(y, mean, .before = 24*7 - 1, .complete = TRUE)
    ) %>%
    ungroup()
}

fit_nb_model <- function(hourly_df) {
  dat <- hourly_df %>%
    filter(amb, ctas %in% 2:4) %>%
    mutate(ctas = factor(ctas), site = factor(site)) %>%
    tidyr::drop_na(lag_24, lag_168, ma_7d)
  fml <- y ~ site + ctas + factor(dow) + sin_hod + cos_hod + sin_woy + cos_woy +
    lag_24 + lag_168 + ma_7d
  MASS::glm.nb(fml, data = dat, link = log)
}


# ---- In app_global.R or at the top of app_server.R (before UI launches) ----
# scripts/train_ai_nb_model.R


# 
# # --- Load & prep ------------------------------------------------------------
# data_dir  <- "app_data/ed"
# raw_df    <- read_all_sites(data_dir)
# hourly_df <- prep_hourly_counts(raw_df)
# 
# dat_fit <- hourly_df %>%
#   filter(amb, ctas %in% 2:4) %>%
#   mutate(ctas = factor(ctas), site = factor(site)) %>%
#   tidyr::drop_na(lag_24, lag_168, ma_7d)
# 
# xlev <- list(
#   site = levels(dat_fit$site),
#   ctas = levels(dat_fit$ctas)
# )
# 
# # --- Fit light NB (no model/x/y) -------------------------------------------
# fml <- y ~ site + ctas + factor(dow) + sin_hod + cos_hod + sin_woy + cos_woy +
#   lag_24 + lag_168 + ma_7d
# 
# fit <- MASS::glm.nb(
#   formula = fml, data = dat_fit, link = log,
#   model = FALSE, x = FALSE, y = FALSE
# )
# 
# # --- Pack to an ultra-slim object ------------------------------------------
# # keep only what we need to predict: coef, terms (sans env), contrasts, theta
# terms_obj <- delete.response(terms(fit))
# attr(terms_obj, ".Environment") <- NULL
# 
# model_slim <- list(
#   coef       = stats::coef(fit),
#   terms      = terms_obj,
#   contrasts  = fit$contrasts,
#   theta      = unname(fit$theta),   # so code can still use model$theta
#   xlev       = xlev                 # convenience (site, ctas levels)
# )
# class(model_slim) <- "glm_nb_slim"
# 
# # --- Minimal history (last 8 days is enough for lags) -----------------------
# last_seen <- max(hourly_df$hour_ts, na.rm = TRUE)
# need_from <- last_seen - lubridate::hours(24*8)   # 8 days
# 
# hourly_df_light <- hourly_df %>%
#   filter(hour_ts >= need_from) %>%
#   dplyr::select(site, hour_ts, ctas, amb, y) %>%
#   mutate(
#     site = as.character(site),
#     ctas = as.integer(ctas),
#     amb  = as.logical(amb)
#   ) %>%
#   arrange(site, ctas, amb, hour_ts)
# 
# # --- Save (xz compression) --------------------------------------------------
# dir.create("inst/models", recursive = TRUE, showWarnings = FALSE)
# saveRDS(model_slim,        "inst/models/ai_nb_model.rds", compress = "xz")
# saveRDS(hourly_df_light,   "inst/models/hourly_df.rds",   compress = "xz")
# 
# cat("Saved ultra-slim artifacts:\n",
#     "  inst/models/ai_nb_model.rds (coeffs+terms+contrasts+theta)\n",
#     "  inst/models/hourly_df.rds   (last 8 days per series)\n")


# fast load
# ---- Fast load of pre-trained model + data ----

# Robust path finder for shipped model artifacts
find_model_file <- function(fname) {
  # 1) If your app defines app_sys(), try it
  if (exists("app_sys", mode = "function")) {
    p <- tryCatch(app_sys(file.path("models", fname)), error = function(e) NULL)
    if (!is.null(p) && nzchar(p) && file.exists(p)) return(p)
  }
  # 2) If the app is installed as a package, try system.file()
  pkg <- tryCatch(utils::packageName(), error = function(e) "")
  if (!is.null(pkg) && nzchar(pkg)) {
    p <- system.file("models", fname, package = pkg)
    if (nzchar(p) && file.exists(p)) return(p)
  }
  # 3) Running from source (e.g., shinyapps bundle) – use relative paths
  for (cand in c(file.path("inst","models",fname), file.path("models",fname))) {
    if (file.exists(cand)) return(cand)
  }
  NA_character_
}


model_path  <- find_model_file("ai_nb_model.rds")
hourly_path <- find_model_file("hourly_df.rds")

if (is.na(model_path) || is.na(hourly_path)) {
  stop("Model files not found. Ensure inst/models/ai_nb_model.rds and hourly_df.rds are deployed.")
}

`%||%` <- function(x, y) if (is.null(x)) y else x

loaded_obj <- readRDS(model_path)    # could be a bundle OR the model itself
hourly_df  <- readRDS(hourly_path)

if (is.list(loaded_obj) && !is.null(loaded_obj$model)) {
  # bundle shape: list(model=..., xlev=..., meta=...)
  fitted_model <- loaded_obj$model
  xlev         <- loaded_obj$xlev %||% NULL
} else {
  # model saved directly (slim or full)
  fitted_model <- loaded_obj
  # try to get xlev from the object; if full glm.nb, derive from model frame
  if (!is.null(loaded_obj$xlev)) {
    xlev <- loaded_obj$xlev
  } else if (inherits(loaded_obj, "negbin") && !is.null(loaded_obj$model)) {
    xlev <- list(
      site = levels(loaded_obj$model$site),
      ctas = levels(loaded_obj$model$ctas)
    )
  } else {
    xlev <- NULL
  }
}

# A little sanity print can help during development (optional)
message("Loaded model class: ", paste(class(fitted_model), collapse = "/"))


# R/ai_forecast.R
#' Predict expected arrivals for a site / CTAS / time window
#' @export
#' Predict expected arrivals for a site / CTAS / time window
#' @export
predict_counts <- function(model, hourly_df, site, ctas_vec = c(2,3,4),
                           start_date, end_date, from_hour = 16, to_hour = 18,
                           xlev = NULL) {
  tz <- "America/Vancouver"
  
  hrs <- seq(
    as.POSIXct(start_date, tz = tz),
    as.POSIXct(end_date,   tz = tz) + lubridate::hours(23),
    by = "hour"
  )
  
  # Build grid
  grid <- tidyr::expand_grid(
    site = site,
    hour_ts = hrs,
    ctas = ctas_vec,
    amb  = TRUE
  ) |>
    dplyr::mutate(
      dow = lubridate::wday(hour_ts, week_start = 1),
      hod = lubridate::hour(hour_ts),
      woy = lubridate::isoweek(hour_ts),
      sin_hod = sin(2*pi*hod/24),  cos_hod = cos(2*pi*hod/24),
      sin_woy = sin(2*pi*woy/53),  cos_woy = cos(2*pi*woy/53)
    )
  
  # Keep factor versions for the model, but create *plain* join keys
  if (!is.null(xlev)) {
    grid <- grid |>
      dplyr::mutate(
        site_factor = factor(site, levels = xlev$site),
        ctas_factor = factor(ctas, levels = xlev$ctas)
      )
  } else {
    grid <- grid |>
      dplyr::mutate(
        site_factor = factor(site, levels = levels(model$model$site)),
        ctas_factor = factor(ctas, levels = levels(model$model$ctas))
      )
  }
  
  # ---- Normalize join keys (avoid factor/int clashes) ----
  grid <- grid |>
    dplyr::mutate(
      site_key = as.character(site),
      ctas_key = as.integer(ctas)
    )
  
  hist <- hourly_df |>
    dplyr::filter(site == !!site, amb, ctas %in% ctas_vec) |>
    dplyr::select(site, hour_ts, ctas, amb, y) |>
    dplyr::mutate(
      site_key = as.character(site),
      ctas_key = as.integer(ctas)
    )
  
  # Lags via shifted joins (join by normalized keys)
  lag24  <- hist |>
    dplyr::transmute(site_key, hour_ts = hour_ts + lubridate::hours(24), ctas_key, amb, lag_24  = y)
  lag168 <- hist |>
    dplyr::transmute(site_key, hour_ts = hour_ts + lubridate::hours(168), ctas_key, amb, lag_168 = y)
  
  grid2 <- grid |>
    dplyr::left_join(lag24,  by = c("site_key","hour_ts","ctas_key","amb")) |>
    dplyr::left_join(lag168, by = c("site_key","hour_ts","ctas_key","amb"))
  
  # 7-day moving average from history
  ma_tbl <- hist |>
    dplyr::arrange(hour_ts) |>
    dplyr::group_by(site_key, ctas_key, amb) |>
    dplyr::mutate(ma_7d_hist = slider::slide_dbl(y, mean, .before = 24*7 - 1, .complete = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::transmute(site_key, hour_ts = hour_ts + lubridate::hours(1), ctas_key, amb, ma_7d_hist)
  
  grid3 <- grid2 |>
    dplyr::left_join(ma_tbl, by = c("site_key","hour_ts","ctas_key","amb")) |>
    dplyr::mutate(
      lag_24  = tidyr::replace_na(lag_24,  0),
      lag_168 = tidyr::replace_na(lag_168, 0),
      ma_7d   = tidyr::replace_na(ma_7d_hist, mean(hist$y, na.rm = TRUE))
    ) |>
    dplyr::select(-ma_7d_hist)
  
  # Predict using the factor columns we saved earlier
  newdata <- grid3 |>
    dplyr::mutate(
      site = site_factor,
      ctas = ctas_factor
    )
  
  mu <- if (inherits(model, "glm_nb_slim")) {
    predict_mu_nb(model, newdata)
  } else {
    stats::predict(model, newdata = newdata, type = "response")
  }
  
  preds <- dplyr::bind_cols(grid3, tibble::tibble(pred = mu)) |>
    dplyr::mutate(date = as.Date(hour_ts))
  
  daily_window <- preds |>
    dplyr::filter(hod >= from_hour, hod < to_hour) |>
    dplyr::group_by(date) |>
    dplyr::summarise(expected_count = sum(pred), .groups = "drop")
  
  list(
    hourly       = preds,
    daily_window = daily_window,
    total_window = sum(daily_window$expected_count)
  )
}

# --- helpers (put above server) ---
nb_sim_pi <- function(mu_vec, theta, sims = 2000L) {
  # return c(low80, high80) for the sum of independent NB draws with means=mu_vec
  draws <- replicate(sims, sum(rnbinom(length(mu_vec), size = theta, mu = mu_vec)))
  c(low = unname(quantile(draws, 0.10)), high = unname(quantile(draws, 0.90)))
}
fmt_date <- function(d) format(as.Date(d), "%b %d, %Y")

fmt_time <- function(t) sprintf("%02d:00", lubridate::hour(t))
fmt_count <- function(x) {
  ifelse(x >= 10, sprintf("%.0f", round(x)),
         ifelse(x >= 1, sprintf("%.1f", x), sprintf("%.2f", x)))
}

# Works with the ultra-slim bundle (class "glm_nb_slim")
predict_mu_nb <- function(model_slim, newdata) {
  stopifnot(inherits(model_slim, "glm_nb_slim"))
  X <- stats::model.matrix(model_slim$terms, newdata,
                           contrasts.arg = model_slim$contrasts)
  as.numeric(exp(drop(X %*% model_slim$coef)))
}

