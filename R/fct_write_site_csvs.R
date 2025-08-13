write_site_csvs <- function(df, output_dir = "app_data/ed", progress = NULL) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  sites <- unique(df$facility_name)
  total_sites <- length(sites)
  
  split(df, df$facility_name) %>%
    purrr::iwalk(function(site_df, facility_name) {
      outfile <- file.path(output_dir, paste0(facility_name, ".csv"))
      print(paste0('Processing ', facility_name, "."))
      site_df <- site_df %>% dplyr::mutate(across(everything(), as.character))
      
      if (file.exists(outfile)) {
        existing <- readr::read_csv(outfile, show_col_types = FALSE) %>%
          dplyr::mutate(across(everything(), as.character))
        
        combined <- dplyr::bind_rows(existing, site_df) %>%
          dplyr::distinct(account_number_hash, .keep_all = TRUE)
      } else {
        combined <- site_df
      }
      
      readr::write_csv(combined, outfile)
      
      # Update progress if available
      if (!is.null(progress)) {
        progress$inc(1 / total_sites, detail = paste("Processed site:", facility_name))
      }
    })
}
