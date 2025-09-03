source("R/fct_clean_ed_data.R")
source("R/fct_write_site_csvs.R")
source("R/fct_build_homepage_summary.R")
source("R/mod_tarmac_fct_display.R")


# new ed data -------------------------------------------------------------

datapath <- file.choose()
df <- readr::read_csv(datapath, show_col_types = FALSE)
clean_df <- clean_ed_data(df)
write_site_csvs(clean_df, output_dir = "app_data/ed")
build_homepage_summary()



# pull the project documents ----------------------------------------------
# Letter to BCEHS Unit Chiefs ---------------------------------------------

docpath <- "C:/Users/boul24/OneDrive - HealthBC/SQI - Spread Quality Improvement [IH]/02 - Projects/02 - Projects/Tarmac Triage/8 - Original Project Site - CDH (Castlegar)/_Toolkit/Draft Content/"
filename <- "Letter to BCEHS Unit Chiefs.pdf"
destpath <- "app_data/tarmac dashboard documents/"
file.copy(paste0(docpath, filename), paste0(destpath, filename), overwrite = TRUE)

# Letter to ED Transfer Sites ---------------------------------------------

filename <- "Letter to ED Transfer Sites.pdf"
file.copy(paste0(docpath, filename), paste0(destpath, filename), overwrite = TRUE)

# Toolkit -----------------------------------------------------------------
docpath <- "C:/Users/boul24/OneDrive - HealthBC/SQI - Spread Quality Improvement [IH]/02 - Projects/02 - Projects/Tarmac Triage/8 - Original Project Site - CDH (Castlegar)/_Toolkit/Draft Versions/"
filename <- "Tarmac Triage Change Toolkit.pdf"
file.copy(paste0(docpath, filename), paste0(destpath, filename), overwrite = TRUE)

# Protocols ---------------------------------------------------------------
destpath <- "app_data/tarmac dashboard documents/Tarmac Protocol/"

# Castlegar ---------------------------------------------------------------
docpath <- "C:/Users/boul24/OneDrive - HealthBC/SQI - Spread Quality Improvement [IH]/02 - Projects/02 - Projects/Tarmac Triage/8 - Original Project Site - CDH (Castlegar)/Protocol/"
filename <- "Tarmac Triage - Castlegar - Planned Closure.pdf"
file.copy(paste0(docpath, filename), paste0(destpath, filename), overwrite = TRUE)

# Chase -------------------------------------------------------------------
docpath <- "C:/Users/boul24/OneDrive - HealthBC/SQI - Spread Quality Improvement [IH]/02 - Projects/02 - Projects/Tarmac Triage/8.3 - Spread Site - CDF (Chase)/Protocol/"
filename <- "Tarmac Triage - Chase - Planned Closure.pdf"
file.copy(paste0(docpath, filename), paste0(destpath, filename), overwrite = TRUE)

# Clearwater --------------------------------------------------------------
docpath <- "C:/Users/boul24/OneDrive - HealthBC/SQI - Spread Quality Improvement [IH]/02 - Projects/02 - Projects/Tarmac Triage/8.4 - Spread Site - DHH (Clearwater)/Protocol/"
filename <- "Tarmac Triage - Clearwater - Unplanned Closure.pdf"
file.copy(paste0(docpath, filename), paste0(destpath, filename), overwrite = TRUE)

# Keremeos ----------------------------------------------------------------
docpath <- "C:/Users/boul24/OneDrive - HealthBC/SQI - Spread Quality Improvement [IH]/02 - Projects/02 - Projects/Tarmac Triage/8.6 - Spread Site - SSHC (Keremeos)/Protocol/"
filename <- "Tarmac Triage - Keremeos - Planned Closure.pdf"
file.copy(paste0(docpath, filename), paste0(destpath, filename), overwrite = TRUE)

# Merritt -----------------------------------------------------------------
docpath <- "C:/Users/boul24/OneDrive - HealthBC/SQI - Spread Quality Improvement [IH]/02 - Projects/02 - Projects/Tarmac Triage/8.1 Spread Site - NVH (Merritt)/Protocol/"
filename <- "Tarmac Triage - Merritt - Unplanned Closure.pdf"
file.copy(paste0(docpath, filename), paste0(destpath, filename), overwrite = TRUE)

# Oliver ------------------------------------------------------------------
docpath <- "C:/Users/boul24/OneDrive - HealthBC/SQI - Spread Quality Improvement [IH]/02 - Projects/02 - Projects/Tarmac Triage/8.7 - Spread Site - SOGH (Oliver)/"
filename <- "Tarmac Triage - Oliver - Unplanned Closure.pdf"
file.copy(paste0(docpath, filename), paste0(destpath, filename), overwrite = TRUE)

# Sparwood ----------------------------------------------------------------
docpath <- "C:/Users/boul24/OneDrive - HealthBC/SQI - Spread Quality Improvement [IH]/02 - Projects/02 - Projects/Tarmac Triage/8.5 - Spread Site - SPE (Sparwood)/Protocol/"
filename <- "Tarmac Triage - Sparwood - Planned Closure.pdf"
file.copy(paste0(docpath, filename), paste0(destpath, filename), overwrite = TRUE)

# Blank Template ----------------------------------------------------------
docpath <- "C:/Users/boul24/OneDrive - HealthBC/SQI - Spread Quality Improvement [IH]/02 - Projects/02 - Projects/Tarmac Triage/10 - Graphics/Tarmac Protocol (HR WEB - Emails, document) 17 06 25/"
filename <- "Tarmac Triage Unplanned Blank Template.pdf"
file.copy(paste0(docpath, filename), paste0(destpath, filename), overwrite = TRUE)




