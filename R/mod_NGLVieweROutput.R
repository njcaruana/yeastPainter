#' NGLVieweROutput UI Function
#'
#' @description The main server module for the NGLVieweR object.
#'
#' @param id,input,output,session Internal parameters for {shiny}. DO NOT REMOVE.
#' @param r A reactive values object from the main app server.
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_NGLVieweROutput_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "position: relative;",
      NGLVieweROutput(ns("structure")),
      uiOutput(ns("logfc_legend"))
    ),
    #NGLVieweROutput(ns("structure")),
    #loader for structure rendering
    div(
      id = ns("render-loader"),
      style = "position: absolute; z-index: 999; top: 91vh; padding: 25px; color: grey;",
      HTML('<div class="fa-1x"><i class="fas fa-spinner fa-spin"></i> loading...</div>')
    ),
    hidden(
      div(
        id = ns("paint-loader"),
        style = "position: absolute; z-index: 999; top: 91vh; padding: 50px; color: grey;",
        HTML('<div class="fa-1x"><i class="fas fa-spinner fa-spin"></i> currently painting your structure, this may take a minute or two...</div>')
      ))
  )
}


#' NGLVieweROutput Server Function
#'
#' @noRd
mod_NGLVieweROutput_server <- function(input, output, session, r) {
  ns <- session$ns

  # The main viewer output. This will only render once or on a major event.
  output$structure <- renderNGLVieweR({
    # Load example by default if no file is uploaded
    if (is.null(r$pdbViewer$PDB)) {
      r$pdbViewer$PDB <- app_sys("app/www/7cid.ngl")
      r$pdbViewer$fileExt <- "pdb"
      r$pdbViewer$name <- "5xtd"
    }

    viewerOutput <- NGLVieweR(r$pdbViewer$PDB, format = r$pdbViewer$fileExt) %>%
      loadStage(r$pdbViewer$stage) %>%
      setQuality("low") %>%
      setFocus(0) %>%
      setSpin(FALSE) %>%
      addRepresentation("cartoon", param = list(
        name = "aa_clicked", visible = TRUE,
        sele = "none", color = "element", colorValue = "#33FF19"
      )) %>%
      # Load from .ngl file
      #loadLabels(r$fileInput$labels) %>%
      loadSelections(r$pdbViewer$selections) %>%
      #loadContacts(r$fileInput$contacts) %>%
      loadStructure(r$pdbViewer$structure, format = r$pdbViewer$fileExt) %>%
     loadSurface(r$pdbViewer$surface)

    return(viewerOutput)
  })


  output$logfc_legend <- renderUI({
    req(r$range)
    req(r$painted_once)

    min_val <- r$range[1]
    max_val <- r$range[2]
    mid_val <- if (min_val < 0 && max_val > 0) 0 else (min_val + max_val) / 2

    tags$div(
      class = "logfc-legend",

      # Tick labels ABOVE the bar
      tags$div(
        class = "logfc-ticks",
        tags$span(sprintf("%.2f", min_val)),
        tags$span(sprintf("%.2f", mid_val)),
        tags$span(sprintf("%.2f", max_val))
      ),

      # Gradient bar
      tags$div(class = "logfc-gradient")
    )
  })


  isolate({
    r$selection$loaded <- FALSE
    r$label$loaded <- FALSE
    #r$contact$loaded <- FALSE
    r$structure$loaded <- FALSE
    r$surface$loaded <- FALSE
    #r$ligand$loaded <- FALSE
    r$stage$loaded <- FALSE
    r$structure$structure <- NULL
    r$surface$surface <- NULL
    #r$ligand$ligand <- NULL
    r$stage$stage <- NULL
    #r$contact$contacts <- NULL

  })



  # Loader
  observeEvent(r$painting, priority = 100, {
    if(isTRUE(r$painting)){
      shinyjs::show("paint-loader")
    } else {
      shinyjs::hide("paint-loader")
    }
  })


  observeEvent(r$rendering, {
    if(isTRUE(r$rendering) || is.null(r$rendering)) {
      shinyjs::show("render-loader")
    } else {
      shinyjs::hide("render-loader")
    }
  })


}
## To be copied in the UI
#

## To be copied in the server
#

