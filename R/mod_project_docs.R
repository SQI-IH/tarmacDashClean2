# =========================
# Project Docs (Local) Module
# =========================
# Usage:
# mod_project_docs_ui("docs")
# mod_project_docs_server("docs", base_dir = "app_data/tarmac dashboard documents")

#' mod_project_docs_ui
#'
#' @description UI for displaying downloadable local documents.
#' @param id Shiny module ID
#'
#' @export
mod_project_docs_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shinydashboard::tabItem(
    tabName = "project_docs",
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        title = "Project Documents",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        
        # Parent-level docs (root PDFs)
        shiny::uiOutput(ns("parent_docs_ui")),
        
        shiny::tags$hr(),
        shiny::h4("Tarmac Site Protocols"),
        shiny::uiOutput(ns("tarmac_protocol_docs")),
        
        shiny::tags$hr(),
        shiny::h4("Paramedic Workflow"),
        shiny::uiOutput(ns("paramedic_workflow_docs"))
      )
    )
  )
}

#' mod_project_docs_server
#'
#' @description Server logic to list and download local PDFs.
#' @param id Shiny module ID
#' @param base_dir Character path to the root docs folder (default:
#'                 "app_data/tarmac dashboard documents")
#'
#' @export
mod_project_docs_server <- function(id,
                                    base_dir = "app_data/tarmac dashboard documents") {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ==============
    # Config
    # ==============
    PINNED_DOC <- "Tarmac Triage Change Toolkit"  # case/space-insensitive
    
    # ==============
    # Helpers
    # ==============
    `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
    .norm  <- function(x) trimws(tolower(x))
    
    safe_list <- function(path, pattern = NULL, recursive = FALSE) {
      if (!dir.exists(path)) return(character(0))
      list.files(path, pattern = pattern, full.names = TRUE, recursive = recursive)
    }
    
    # Create a stable, unique key from full file path
    safe_key <- function(path) {
      if (requireNamespace("digest", quietly = TRUE)) {
        return(paste0("k_", digest::digest(path, algo = "xxhash64")))
      }
      raw <- gsub("[^A-Za-z0-9]+", "_", path)
      paste0("k_", make.names(raw, unique = TRUE))
    }
    
    make_df <- function(paths) {
      if (length(paths) == 0) {
        return(data.frame(
          display_name = character(),
          name         = character(),
          path         = character(),
          key          = character(),
          stringsAsFactors = FALSE
        ))
      }
      fn <- basename(paths)
      data.frame(
        display_name = tools::file_path_sans_ext(fn),
        name         = fn,
        path         = paths,
        key          = vapply(paths, safe_key, character(1)),
        stringsAsFactors = FALSE
      )
    }
    
    render_links <- function(df, ns, section, highlight = NULL) {
      if (is.null(df) || nrow(df) == 0) return(NULL)
      
      shiny::tagList(
        lapply(seq_len(nrow(df)), function(i) {
          label_text <- df$display_name[i]
          label_node <- if (!is.null(highlight) && .norm(label_text) == .norm(highlight)) {
            shiny::tags$strong(label_text)
          } else {
            label_text
          }
          
          shiny::downloadLink(
            outputId = ns(paste0("download_", section, "_", df$key[i])),
            label    = label_node,
            style    = "display:block; margin: 6px 0;"
          )
        })
      )
    }
    
    register_downloads <- function(df, section) {
      if (is.null(df) || nrow(df) == 0) return(invisible(NULL))
      
      for (i in seq_len(nrow(df))) {
        local({
          ii   <- i
          key  <- df$key[ii]
          name <- df$name[ii]
          path <- df$path[ii]
          
          output[[paste0("download_", section, "_", key)]] <- shiny::downloadHandler(
            filename = function() name,
            content  = function(file) {
              ok <- file.copy(path, file, overwrite = TRUE)
              if (!ok) stop("Failed to copy file: ", path)
            }
          )
        })
      }
      invisible(NULL)
    }
    
    # ==============
    # Reactive state
    # ==============
    documents <- shiny::reactiveValues(
      parent = NULL,
      tarmac_protocol = NULL,
      paramedic_workflow = NULL
    )
    
    # Populate docs on load / when base_dir changes
    shiny::observe({
      root <- base_dir
      
      # Root PDFs
      parent_paths <- safe_list(root, pattern = "\\.pdf$", recursive = FALSE)
      parent_df    <- make_df(parent_paths)
      
      if (nrow(parent_df)) {
        parent_df$is_pinned <- .norm(parent_df$display_name) == .norm(PINNED_DOC)
        parent_df <- parent_df[order(-parent_df$is_pinned, parent_df$display_name), ]
        parent_df$is_pinned <- NULL
      }
      
      # Subfolders
      tarmac_dir  <- file.path(root, "Tarmac Protocol")
      paramed_dir <- file.path(root, "Paramedic Workflow")
      
      tarmac_df  <- make_df(safe_list(tarmac_dir,  pattern = "\\.pdf$", recursive = FALSE))
      paramed_df <- make_df(safe_list(paramed_dir, pattern = "\\.pdf$", recursive = FALSE))
      
      # Sort subfolder lists alphabetically
      if (nrow(tarmac_df))  tarmac_df  <- tarmac_df[order(tarmac_df$display_name), ]
      if (nrow(paramed_df)) paramed_df <- paramed_df[order(paramed_df$display_name), ]
      
      documents$parent             <- parent_df
      documents$tarmac_protocol    <- tarmac_df
      documents$paramedic_workflow <- paramed_df
    })
    
    # ==============
    # UI bindings
    # ==============
    output$parent_docs_ui <- shiny::renderUI({
      render_links(documents$parent, ns, section = "parent", highlight = PINNED_DOC)
    })
    output$tarmac_protocol_docs <- shiny::renderUI({
      render_links(documents$tarmac_protocol, ns, section = "tarmac_protocol")
    })
    output$paramedic_workflow_docs <- shiny::renderUI({
      render_links(documents$paramedic_workflow, ns, section = "paramedic_workflow")
    })
    
    # ==============
    # Download handlers
    # ==============
    shiny::observe({ register_downloads(documents$parent,             "parent") })
    shiny::observe({ register_downloads(documents$tarmac_protocol,    "tarmac_protocol") })
    shiny::observe({ register_downloads(documents$paramedic_workflow, "paramedic_workflow") })
  })
}
