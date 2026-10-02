#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  r <- reactiveValues()

  observe({
    r$sequence <- input$`NGLVieweROutput_ui_1-structure_sequence`
    r$resno <- input$`NGLVieweROutput_ui_1-structure_resno`
    r$chainname <- input$`NGLVieweROutput_ui_1-structure_chainname`
    r$sequence_df <- sequence_df(r$sequence, r$resno, r$chainname, selchain = r$sequenceOutput$selectedChain)
    r$aa_clicked <- input$aa_clicked
    r$PDB <- input$`NGLVieweROutput_ui_1-structure_PDB`
  })

  observe({
    r$rendering <- input$`NGLVieweROutput_ui_1-structure_rendering`
  })

  observe({
    r$range = input$`pdbPainter_ui_1-range`
  })

  observeEvent(r$tabswitch, {
    req(r$tabswitch)
    updateTabsetPanel(session, "tabset1", selected = r$tabswitch)
    r$tabswitch <- NULL
  })

  # --- Workflow rail ---------------------------------------------------
  # Sits above the tab content in place of the tab strip. The three steps all
  # act on the same view, so the rail reads as progress rather than as
  # navigation, but clicking one still brings the viewer forward.
  output$stageRail <- renderUI({
    stageRail(stageStatus(r), active = input$tabset1)
  })

  lapply(appStages(), function(s) {
    observeEvent(input[[paste0("goto_", s$key)]], {
      updateTabsetPanel(session, "tabset1", selected = s$tab)
    }, ignoreInit = TRUE)
  })

  observeEvent(input$goto_About, {
    updateTabsetPanel(session, "tabset1", selected = "About")
  }, ignoreInit = TRUE)

  observe({
    r$sidebarItemExpanded <- input$sidebarItemExpanded #loading of UI_components from .ngl file
  })

  #Component handlers
  # Relayed as list(id, nonce) rather than the bare id: reactiveValues ignores
  # a write identical to the value already stored, so clicking the same entry
  # twice in a row never reached the modules. The nonce makes every click a
  # distinct value. Handlers defined in handlers.js.
  clickRelay <- local({
    n <- 0
    function(id) {
      n <<- n + 1
      list(id = id, nonce = n)
    }
  })

  observeEvent(input$selectionRemove_id, {
    r$selection$selectionRemove_id <- clickRelay(input$selectionRemove_id)
  })
  observeEvent(input$selectionLink_id, {
    r$selection$selectionLink_id <- clickRelay(input$selectionLink_id)
  })
  observeEvent(input$labelRemove_id, {
    r$label$labelRemove_id <- clickRelay(input$labelRemove_id)
  })
  observeEvent(input$labelLink_id, {
    r$label$labelLink_id <- clickRelay(input$labelLink_id)
  })

  observeEvent(input[["selection_ui_1-closeSelectionPanel"]], {
    removeUI(selector = "#selection_ui_1-floating_panel")
    r$floatingPanel_inserted <- FALSE  # Reset the flag here
  })

  observe({
    r$examples$example_link_id <- input$example_link_id
  })

  mod_about_server("about_ui_1", r = r)
  callModule(mod_pdbViewer_server, "pdbViewer_ui_1", globalSession = session, r = r)
  callModule(mod_selection_server, "selection_ui_1", globalSession = session, r = r)
  callModule(mod_stage_server, "stage_ui_1", globalSession = session, r = r)
  callModule(mod_snapshot_server, "snapshot_ui_1", globalSession = session, r = r)
  callModule(mod_pdbPainter_server, "pdbPainter_ui_1", globalSession = session, r = r)
  callModule(mod_NGLVieweROutput_server, "NGLVieweROutput_ui_1", r = r)

}
