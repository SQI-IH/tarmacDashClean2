# R/fct_clean_ed_data.R

clean_ed_data <- function(raw_df) {
  library(dplyr)
  library(digest)
  library(lubridate)
  library(janitor)
  
  df <- raw_df %>%
    janitor::clean_names() %>%
    rename(disposition = ed_departure_disposition_desc) %>%
    mutate(
      arrival_datetime = parse_date_time(ed_arrival_date_time, orders = c("ymd HMS", "mdy HMS")),
      account_number_hash = sapply(account_number, function(x) substr(digest(x, algo = "sha256"), 1, 16))
    )
  
  df %>%
    distinct(account_number_hash, .keep_all = TRUE) %>%
    select(-account_number, -ed_arrival_date_time)
}
