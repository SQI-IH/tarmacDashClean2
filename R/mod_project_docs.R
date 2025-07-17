#' mod_project_docs_ui
#'
#' @description UI for displaying downloadable Google Drive documents.
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
        shiny::uiOutput(ns("parent_docs_ui")),
        shiny::tags$hr(),
        shiny::h4("Tarmac Site Protocols"),
        shiny::tags$hr(),
        shiny::uiOutput(ns("tarmac_protocol_docs")),
        shiny::tags$hr()
      )
    )
  )
}


#' mod_project_docs_server
#'
#' @description Server logic to manage document lists and downloads from Google Drive.
#' @param id Shiny module ID
#'
#' @export
mod_project_docs_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    PINNED_DOC <- "Tarmac Triage Change Toolkit"
    
    documents <- shiny::reactiveValues(
      parent = NULL,
      tarmac_protocol = NULL,
      paramedic_workflow = NULL
    )
    
    shiny::observe({
      
      folder_id <- googledrive::as_id(DRIVE_FOLDER_ID)
      
      parent_pdfs <- googledrive::drive_ls(path = folder_id, type = "application/pdf")
      
      documents$parent <- parent_pdfs |>
        dplyr::mutate(
          display_name = tools::file_path_sans_ext(name),
          is_pinned = trimws(tolower(display_name)) == trimws(tolower(PINNED_DOC))
        ) |>
        dplyr::arrange(dplyr::desc(is_pinned), display_name) |>
        dplyr::select(display_name, name, id)
      
      subfolders <- googledrive::drive_ls(path = folder_id, type = "application/vnd.google-apps.folder")
      
      for (i in seq_len(nrow(subfolders))) {
        pdfs <- googledrive::drive_ls(path = googledrive::as_id(subfolders$id[i]), type = "application/pdf")
        doc_df <- data.frame(
          display_name = tools::file_path_sans_ext(pdfs$name),
          name = pdfs$name,
          id = pdfs$id,
          stringsAsFactors = FALSE
        )
        
        folder_name <- subfolders$name[i]
        if (folder_name == "Tarmac Protocol") documents$tarmac_protocol <- doc_df
        if (folder_name == "Paramedic Workflow") documents$paramedic_workflow <- doc_df
      }
    })
    
    render_links <- function(df, ns, highlight = NULL) {
      if (is.null(df) || nrow(df) == 0) return(NULL)
      shiny::tagList(
        lapply(seq_len(nrow(df)), function(i) {
          label_text <- df$display_name[i]
          label <- if (!is.null(highlight) && trimws(tolower(label_text)) == trimws(tolower(highlight))) {
            shiny::tags$strong(label_text)
          } else label_text
          
          shiny::downloadLink(
            ns(paste0("download_", df$id[i])),
            label = label_text,
            style = "display: block; margin-bottom: 10px;"
          )
        })
      )
    }
    
    output$parent_docs_ui <- shiny::renderUI({ render_links(documents$parent, ns, highlight = PINNED_DOC) })
    output$tarmac_protocol_docs <- shiny::renderUI({ render_links(documents$tarmac_protocol, ns) })
    output$paramedic_workflow_docs <- shiny::renderUI({ render_links(documents$paramedic_workflow, ns) })
    
    register_downloads <- function(df, prefix) {
      if (is.null(df) || nrow(df) == 0) return()
      for (i in seq_len(nrow(df))) {
        local({
          local_i <- i
          local_id <- df$id[local_i]
          output[[paste0("download_", local_id)]] <- shiny::downloadHandler(
            filename = function() df$name[local_i],
            content = function(file) {
              googledrive::drive_download(
                googledrive::as_id(local_id),
                path = file,
                overwrite = TRUE
              )
            }
          )
        })
      }
    }
    
    shiny::observe({ register_downloads(documents$parent, "parent") })
    shiny::observe({ register_downloads(documents$tarmac_protocol, "tarmac_protocol") })
    shiny::observe({ register_downloads(documents$paramedic_workflow, "paramedic_workflow") })
  })
}
