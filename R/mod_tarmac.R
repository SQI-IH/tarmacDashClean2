# Helpers (kept; improved file matching + static path)
# --------------------------------------------------
# Locate served /www dir in dev & deploy
.find_www_dir <- function() {
  if (dir.exists("inst/app/www")) return(normalizePath("inst/app/www", winslash = "/", mustWork = FALSE))
  if (dir.exists("www"))         return(normalizePath("www", winslash = "/", mustWork = FALSE))
  if (exists("app_sys", mode = "function", inherits = TRUE)) {
    p <- try(app_sys("app/www"), silent = TRUE)
    if (!inherits(p, "try-error") && dir.exists(p)) return(normalizePath(p, winslash = "/", mustWork = FALSE))
  }
  normalizePath(file.path(getwd(), "www"), winslash = "/", mustWork = FALSE)
}

# Find a Toolkit/Literature Review file on disk (.pdf or .docx)
find_toolkit_file <- function() {
  docs_dir <- file.path(.find_www_dir(), "tarmac-docs")
  if (!dir.exists(docs_dir)) return(NULL)
  
  files <- list.files(docs_dir, pattern = "\\.(pdf|docx)$", full.names = TRUE, ignore.case = TRUE)
  if (!length(files)) return(NULL)
  
  # Prefer names with "Toolkit"; else TARMAC/Literature/Triage; else newest
  hit <- files[grepl("Toolkit", basename(files), ignore.case = TRUE)]
  if (!length(hit)) hit <- files[grepl("TARMAC|Triage|Literature", basename(files), ignore.case = TRUE)]
  if (!length(hit)) hit <- files[order(file.info(files)$mtime, decreasing = TRUE)][1]
  
  normalizePath(hit[1], winslash = "/", mustWork = FALSE)
}

# MIME helper so Shiny sets the right Content-Type (if using downloadHandler fallback)
guess_mime <- function(path) {
  ext <- tolower(tools::file_ext(path %||% ""))
  switch(ext,
         "pdf"  = "application/pdf",
         "docx" = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
         # sensible default
         "application/octet-stream"
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Map URL prefix /tarmac-docs to the real folder so static links work
.expose_tarmac_docs <- function() {
  docs_dir <- file.path(.find_www_dir(), "tarmac-docs")
  if (dir.exists(docs_dir)) {
    shiny::addResourcePath("tarmac-docs", docs_dir)
  }
}


# =========================
# Module
# =========================

#' mod_tarmac_ui
#'
#' @param id shiny id
#' @export
#' @name mod_tarmac
mod_tarmac_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tags$head(shiny::tags$style(shiny::HTML(sprintf("\n  /* Only affect this specific box */\n  #%s .box-header { position: relative; }\n  #%s .tarmac-box-tool {\n    position: absolute;\n    top: 6px;\n    right: 10px;\n    z-index: 2;\n  }\n  #%s .tarmac-box-tool .btn { padding: 6px 8px; }\n", ns("tarmac_boxwrap"), ns("tarmac_boxwrap"), ns("tarmac_boxwrap")))))
  
  shinydashboard::tabItem(
    tabName = "tarmac",
    shiny::fluidRow(
      tarmac_description_box(ns)   # renders the icon + dynamic download link
    ),
    shiny::br(),
    shiny::fluidRow(
      shiny::div(
        id = ns("tarmac_boxwrap"),
        shinydashboard::box(
          width = 8,
          title = shiny::tagList(
            "Monthly Tarmac Events",
            shiny::div(
              class = "tarmac-box-tool",
              shinyWidgets::dropdown(
                width = "180px",
                right = TRUE,
                icon  = shiny::icon("chart-simple"),
                shiny::selectInput(
                  inputId = ns("chart_type"),
                  label   = NULL,
                  choices = c("Bar Chart" = "bar", "Control Chart" = "control"),
                  selected = "bar",
                  width = "160px"
                )
              )
            )
          ),
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("tarmacBar"), height = "250px")
          )
        )
      ),
      shiny::uiOutput(ns("tarmac_box"))
    )
  )
}


# Description box with right-side icons (Toolkit link is injected by server)
tarmac_description_box <- function(ns) {
  primary <- "#005AA2"
  darker_primary <- "#6185A2"
  accent <- "#EDEDED"
  secondary <- "#DE5428"
  text_color <- "#EDEDED"
  
  shiny::tags$div(
    id = ns("tarmac_box"),
    style = sprintf(
      paste(
        "background-color:%s;",
        "color:%s;",
        "border-left:5px solid %s;",
        "border-radius:8px;",
        "padding:15px;",
        "width:98%%;",
        "margin:20px auto;",
        "box-shadow:0 2px 4px rgba(0,0,0,.1);",
        "display:flex;",
        "justify-content:space-between;",
        "align-items:center;",
        "font-size:1.3em;",
        "font-weight:500;",
        "line-height:1.5;"
        , collapse = " "),
      darker_primary, text_color, accent
    ),
    
    class = "tarmac-desc-box",
    shiny::tags$div(style = "flex:1; font-weight:bold; font-size:1.5em; color:#EDEDED;", "Project Aim"),
    shiny::tags$div(style = "flex:3 1 0; text-align:left;", shiny::textOutput(ns("tarmac_text"))),
    
    # Right-side icons
    shiny::tags$div(
      style = "flex:0 0 auto; display:flex; align-items:center; gap:16px; margin-left:auto;",
      
      # Dynamic toolkit link rendered by server (exact filename + download attr)
      shiny::uiOutput(ns("toolkit_icon_link")),
      
      # QR (raw static asset under /www)
      shiny::tags$a(
        href = "tarmac-qr.png",
        download = "tarmac-qr.png",
        shiny::tags$img(
          src = "tarmac-qr.png",
          alt = "QR Code",
          width = "80px",
          title = "Download QR code"
        )
      )
    )
  )
}


#' mod_tarmac_server
#'
#' @param id shiny id
#' @export
mod_tarmac_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Expose /tarmac-docs so the static link works both locally & on shinyapps.io
    .expose_tarmac_docs()
    
    # Emit a direct <a href="/tarmac-docs/<filename>" download> link for the icon
    output$toolkit_icon_link <- shiny::renderUI({
      f <- find_toolkit_file()
      message("toolkit_icon_link -> find_toolkit_file(): ", f %||% "NULL")
      
      if (is.null(f) || !file.exists(f)) {
        return(
          shiny::tags$div(
            title = "Toolkit not found in www/tarmac-docs (or inst/app/www/tarmac-docs).",
            shiny::tags$img(src = "toolkit-icon.png", alt = "Tarmac Toolkit", width = "80px",
                            style = "opacity:0.5; cursor:not-allowed;")
          )
        )
      }
      fname <- basename(f)
      shiny::tags$a(
        href = file.path("tarmac-docs", fname),   # static resource URL (maps via addResourcePath)
        download = fname,                           # ask browser to download
        shiny::tags$img(
          src   = "toolkit-icon.png",
          alt   = "Tarmac Toolkit",
          width = "80px",
          title = paste("Download", fname)
        )
      )
    })
    
    # Optional: keep a downloadHandler fallback (not used by the icon)
    output$download_toolkit <- shiny::downloadHandler(
      filename = function() {
        f <- find_toolkit_file()
        if (is.null(f)) "TARMAC_Triage_Document.pdf" else basename(f)
      },
      content = function(file) {
        f <- find_toolkit_file()
        message("download_toolkit -> find_toolkit_file(): ", f %||% "NULL")
        if (is.null(f) || !file.exists(f)) {
          shiny::showNotification(
            "Toolkit file not found in www/tarmac-docs (or inst/app/www/tarmac-docs when deployed).",
            type = "error", duration = 7
          )
          stop("Toolkit file not found at: ", f %||% "<NULL>")
        }
        ok <- file.copy(f, file, overwrite = TRUE)
        if (!ok) stop("Failed to copy Toolkit from: ", f)
      },
      contentType = {
        f <- find_toolkit_file()
        guess_mime(f)
      }
    )
    
    # ---- Your existing logic ----
    edData     <- shiny::reactive(allEd())
    tarmacData <- shiny::reactive(load_tarmac_data(edData()))
    
    output$tarmacs_per_year <- shinydashboard::renderValueBox({
      recent <- read.csv("app_data/homepage_summary.csv")
      shinydashboard::valueBox(
        value = sum(recent$tarmac_12mo),
        subtitle = shiny::HTML("Tarmac Patients Registered in the past 12 months"),
        icon = shiny::icon("hospital-user"),
        color = "green"
      )
    })
    
    output$tarmac_text <- shiny::renderText({
      df <- dynamicText()
      # df$text[df$instrument == 'aim text']
    })
    
    output$tarmacBar <- plotly::renderPlotly({
      summaryBarPlot(tarmacData(), plot.type = input$chart_type)
    })
    
    output$tarmac_box <- tarmac_boxUI(ns)
  })
}
