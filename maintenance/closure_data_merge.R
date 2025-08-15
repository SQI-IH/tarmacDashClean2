
# Diversion Report Outage Data  -------------------------------------------


df <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1blKyewomhKKNxCJ_nnGt7MQFDH0RBnPKh4hrUKf-o_c/edit?gid=976485941"
)

dfOutage <- 
  df |>
  dplyr::filter(!is.na(submission_date)) |>
  dplyr::mutate(start_date = lubridate::ymd(start_date))


# PHSA Data ---------------------------------------------------------------

dfPhsa <- readxl::read_excel('./app_data/outage/phsa_data.xlsx')

dfPhsa <- 
  dfPhsa |>
  dplyr::mutate(start_date = lubridate::date(lubridate::ymd_hms(`Closure Start`)))

dfJoin <- dplyr::full_join(dfOutage, dfPhsa, by = c('hospital_name' = 'Diverting Hospital',
                                                    'start_date' = 'start_date',
                                                    'hospital_dept' = 'Department'))
