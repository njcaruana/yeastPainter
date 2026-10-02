#' RCA Graph UI Function
#'
#' @description The main server module for the the Complex Table object.
#'
#' @param id,input,output,session Internal parameters for {shiny}. DO NOT REMOVE.
#' @param r A reactive values object from the main app server.
#' @noRd
#'
#' @importFrom shiny NS tagList

# Subunit selections are always drawn as a uniformly coloured surface, so
# these are constants rather than user-facing controls.
SELECTION_TYPE <- "surface"
SELECTION_COLOR_SCHEME <- "uniform"

mod_selection_ui <- function(id) {
  ns <- NS(id)

  tags$div(
    id = ns("floating_panel"),
    style = paste0(
      "position: fixed;", "top: 0; right: 0;", "width: 300px; height: 100vh;",
      "background-color: #222d33;", "border-left: 1px solid #ccc;",
      "box-shadow: -3px 0 8px rgba(0,0,0,0.1);", "padding: 20px;",
      "z-index: 1050;", "overflow-y: auto;", "color: #FFFFFF"
    ),
    h4("Control Panel"),
    bs_textInput(ns("selection"), "Select", placeholder = "e.g. 20-30 OR <NG>", id_modal = "select_modal"),
    # Type and scheme are fixed for subunit selections, so they are shown for
    # reference rather than offered as a choice.
    tags$div(
      style = "margin-bottom: 15px; font-size: 13px;",
      tags$div(
        style = "display: flex; justify-content: space-between; padding: 2px 0;",
        tags$span("Type", style = "opacity: 0.7;"),
        tags$strong(SELECTION_TYPE)
      ),
      tags$div(
        style = "display: flex; justify-content: space-between; padding: 2px 0;",
        tags$span("Scheme", style = "opacity: 0.7;"),
        tags$strong(SELECTION_COLOR_SCHEME)
      )
    ),
    colourpicker::colourInput(ns("selectionColor"), label = "Color", "#00FF00", palette = "limited", closeOnClick = TRUE),
    sliderInput(ns("selectionOpacity"), "Opacity", min = 0, ticks = FALSE, max = 1, value = 1),
    textInput(ns("selName"), label = "Name"),
    actionButton(ns("addSelection"), "Update Selection"),
    actionButton(ns("closeSelectionPanel"), "Close Proteins"),
    br(),
    br(),
    tags$div(id = "selectionPlaceholder", style = "padding-bottom: 15px; overflow-y: auto; max-height:200px;")

    # Insert the controls here
    #mod_selectionControls_ui(ns("controls")),

    #br(), br(),
    #actionButton(ns("close_panel"), "Close Panel")
  )
}


#'Server Function
#'
#' @noRd
mod_selection_server <- function(input, output, session, globalSession, r) {

  ns <- session$ns

  Viewer_proxy <- NGLVieweR_proxy("NGLVieweROutput_ui_1-structure", session = globalSession)

 # Inputs for sequenceOutput module
  observeEvent(input$selection, {
    r$selection$selectionInput <- input$selection
  })
  observeEvent(input$selectionColor, {
    r$selection$selectionColor <- input$selectionColor
  })
#
  # Load UI component from .ngl file and bind to component data.frame
  observeEvent(r$sidebarItemExpanded, {
    if (r$selection$loaded == FALSE && (r$sidebarItemExpanded == "selection")) { # reset in mod_NGLVieweROutput

      # Reset input values
      updateTextInput(session, "selection", value = "")
      colourpicker::updateColourInput(session, "selectionColor", value = "#00FF00")
      updateSliderInput(session, "selectionOpacity", value = 1)
      updateTextInput(session, "selName", value = "")

      # loadUI components
      loadUI_component(r$pdbViewer$selections, "selection")

      # load data
      r$selection$selections <- r$pdbViewer$selections
      r$selection$loaded <- TRUE
    }
  })

#
  r$selection <- reactiveValues(counter = 0, editing_uu_id = NULL)
  isolate(r$selection$selectionColorScheme <- SELECTION_COLOR_SCHEME)

  observeEvent(input$addSelection, {
    selection <- insertUI_selection(paste0(r$sequence_df$AA, collapse = ""), isolate(input$selection), isolate(r$gene_map))
    if (selection == "none") return()

    # Clicking a saved selection loads it for editing. Saving then updates
    # that entry rather than adding another one under a fresh id.
    editing <- isolate(r$selection$editing_uu_id)
    existing <- !is.null(editing) &&
      !is.null(r$selection$selections) &&
      sprintf("selection-%s", editing) %in% r$selection$selections$id

    uu_id <- if (existing) editing else uuid::UUIDgenerate()
    sel_id <- sprintf("selection-%s", uu_id)

    if (existing) {
      row_i <- match(sel_id, r$selection$selections$id)
      name <- insertUI_name("selection", isolate(input$selName),
                            counter = r$selection$selections$row[row_i])
    } else {
      r$selection$counter <- r$selection$counter + 1
      name <- insertUI_name("selection", isolate(input$selName),
                            counter = r$selection$counter)
    }

    # save selection data
    new_sel <- data.frame(
      row = if (existing) r$selection$selections$row[row_i] else r$selection$counter,
      type = "selection",
      id = sel_id,
      name = name,
      selection = selection,
      colorValue = isolate(input$selectionColor),
      colorScheme = SELECTION_COLOR_SCHEME,
      opacity = isolate(input$selectionOpacity),
      structureType = SELECTION_TYPE,
      stringsAsFactors = FALSE
    )

    # Drop the old representation first: adding one under a name that already
    # exists leaves the previous version drawn underneath.
    if (existing) {
      Viewer_proxy %>% removeSelection(name = sel_id)
    }

    Viewer_proxy %>% addSelection(SELECTION_TYPE,
                                  param = list(
                                    name = sel_id,
                                    sele = selection,
                                    colorScheme = SELECTION_COLOR_SCHEME,
                                    colorValue = isolate(input$selectionColor),
                                    opacity = isolate(input$selectionOpacity)
                                  )
    )

    if (existing) {
      r$selection$selections[row_i, ] <- new_sel
      # Keep the entry's label in step when it was renamed.
      shinyjs::html(selector = sprintf("#selectionLink-%s", uu_id), html = name)
    } else {
      insertUI_component("selection", name, uu_id = uu_id)
      r$selection$selections <- rbind(r$selection$selections, new_sel)
    }

    # reset inputs
    reset("selection")
    reset("selName")
    r$selection$editing_uu_id <- NULL
  })


  # # Remove UI_component
observeEvent(r$selection$selectionRemove_id, {
  removed <- r$selection$selectionRemove_id
  removed_id <- if (is.list(removed)) removed$id else removed
  req(!is.null(removed_id))

  r$selection$selections <- removeUI_component(Viewer_proxy, r$selection$selections, "selection", removed_id)
  reset("selection")
  reset("selName")
  # Deleting the entry that was open for editing must not leave the next save
  # pointing at a row that no longer exists.
  r$selection$editing_uu_id <- NULL
})

  # update selection values when selection link is clicked
  observeEvent(r$selection$selectionLink_id, {
    # Relayed as list(id, nonce) so a repeat click on the same entry still
    # registers; older saved state may still hold a bare string.
    clicked <- r$selection$selectionLink_id
    clicked_id <- if (is.list(clicked)) clicked$id else clicked
    req(!is.null(clicked_id))

    uu_id <- str_replace(clicked_id, "selectionLink-", "")
    id <- sprintf("selection-%s", uu_id)
    data <- r$selection$selections[r$selection$selections$id == id, ]
    req(nrow(data) == 1)

    # Remember which entry is open so the next save updates it in place.
    r$selection$editing_uu_id <- uu_id

    updateTextInput(session, "selection", value = data$selection)
    colourpicker::updateColourInput(session, "selectionColor", value = data$colorValue)
    updateTextInput(session, "selName", value = data$name)
    updateSliderInput(session, "selectionOpacity", value = data$opacity)
  })


  observeEvent(r$selection$panel_open_trigger, {
    req(r$selection$selections)
    loadUI_component(data = r$selection$selections, type = "selection")
  })



}

## To be copied in the UI
#

## To be copied in the server
#

