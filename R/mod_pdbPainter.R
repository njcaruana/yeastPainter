#' Fold changes from a file, for a PDB entered by code.
#'
#' @description
#' The only route into the painter in this app: there is no complex database to
#' pick from, so the structure and the fold changes are both supplied directly.
#'
#' @noRd
mod_pdbPainter_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("filefc"), "Upload logFC data",
              accept = c(".csv", ".tsv", ".txt", ".xlsx")),
    div(
      style = "padding: 0 15px 10px 15px;",
      uiOutput(ns("logfcInfo"))
    )
  )
}

#' Painting controls.
#'
#' @noRd
mod_pdbPainter_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sliderInput(ns("range"), "Select logFC Range", min = -5, max = 5, value = c(-2, 2), step = 0.5),
    div(
      actionButton(ns("runpdbpainter"), "Paint", style="display: inline-block; color: #fff; background-color: #243d63; border-color: #fff;padding: 5px 14px 5px 14px; margin: 15px 5px 15px 15px; "),
      actionButton(ns("clearpaint"), "Clear Paint", style="display: inline-block; color: #fff; background-color: #344154; border-color: #fff;padding: 5px 14px 5px 14px; margin: 15px 15px 15px 10px; ")
    ),
    actionButton(ns("openSelectionPanel"), "Select Individual Subunits", style="color: #fff; background-color: #2b607a; border-color: #fff;padding: 5px 14px 5px 14px;margin: 1px 15px 15px 15px; ")
  )
}


#' PDBPainter Server Module
#'
#' @description A server module for handling custom coloring of PDB structures
#'   based on user-uploaded logFC data.
#'
#' @param input,output,session Internal parameters for {shiny}. DO NOT REMOVE.
#' @param r A reactive values object from the main app server.
#'
#' @noRd
mod_pdbPainter_server <- function(input, output, session, globalSession, r) {
  ns <- session$ns
  Viewer_proxy <- NGLVieweR_proxy("NGLVieweROutput_ui_1-structure", session = globalSession)

  #### Code for PDB Generation ###

  # The painting controls stay visible but disabled until a structure has been
  # loaded, because the Structure menu is also where structures are loaded.
  observe({
    has_structure <- isTRUE(r$structureLoaded)
    for (ctl in c("runpdbpainter", "clearpaint", "openSelectionPanel")) {
      if (has_structure) shinyjs::enable(ctl) else shinyjs::disable(ctl)
    }
  })

  observeEvent(input$runpdbpainter, {
    r$painting <- TRUE
    Viewer_proxy %>% updateRepresentation("structure", param = list(visible = FALSE)) # Hide the underlying cartoon
  })

  # --- The uploaded fold changes ---

  # Read once per upload rather than on every render, and report a bad file
  # here instead of failing later inside the paint.
  logfcValues <- reactive({
    req(input$filefc)
    tryCatch(
      fct_read_logfc_file(input$filefc$datapath, input$filefc$name),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 12)
        NULL
      }
    )
  })

  # How many identifiers actually resolve is the thing worth knowing before
  # painting: an unmatched protein leaves its chain grey.
  matchedAccessions <- reactive({
    vals <- logfcValues()
    if (is.null(vals) || nrow(vals) == 0) return(NULL)

    if ("uniprot" %in% names(vals)) {
      ifelse(!is.na(vals$uniprot) & nzchar(vals$uniprot),
             sub("-\\d+$", "", trimws(vals$uniprot)),
             yeastToUniprot(vals$genename))
    } else {
      yeastToUniprot(vals$genename)
    }
  })

  # Ticks the Fold changes stage in the rail once a readable file is in hand.
  observe({
    vals <- logfcValues()
    r$logfcLoaded <- !is.null(vals) && nrow(vals) > 0
  })

  output$logfcInfo <- renderUI({
    if (is.null(input$filefc)) {
      return(tags$span(
        style = "font-size: 11px; color: #9aa7b4;",
        paste0("Needs a fold-change column (logFC) and an identifier column: ",
               "gene name, ORF name or UniProt accession.")
      ))
    }

    vals <- logfcValues()
    if (is.null(vals) || nrow(vals) == 0) {
      return(tags$span(style = "font-size: 11px; color: #e8a33d;",
                       "No usable fold changes in that file."))
    }

    acc <- matchedAccessions()
    matched <- sum(!is.na(acc))

    tagList(
      tags$span(
        style = "font-size: 11px; color: #8bc34a;",
        sprintf("%d fold changes, %d matched to yeast proteins.",
                nrow(vals), matched)
      ),
      if (matched < nrow(vals)) {
        tags$div(
          style = "font-size: 11px; color: #e8a33d; padding-top: 3px;",
          sprintf("%d unmatched - those chains stay grey.", nrow(vals) - matched)
        )
      }
    )
  })

  # --- Painting ---

  paintingdata <- eventReactive(input$runpdbpainter, {
    req(r$pdbViewer$name)

    # observeEvent ignores NULL, so the loader below would never be switched
    # off on a bail-out. Clear it here on every path that returns nothing.
    abort <- function(msg) {
      r$painting <- FALSE
      showNotification(msg, type = "warning", duration = 10)
      NULL
    }

    if (is.null(input$filefc)) {
      return(abort("Nothing to paint with: upload a logFC file first."))
    }

    df <- logfcValues()
    if (is.null(df) || nrow(df) == 0) {
      return(abort("No usable logFC values to paint with."))
    }

    #create the hex colors
    range_vals <- input$range
    hexCols <- fct_generate_hex_colors(df, range_vals)

    #get the protein chain data
    chainDf <- tryCatch(
      fct_get_chain_df(r$pdbViewer$name),
      error = function(e) NULL
    )
    if (is.null(chainDf) || nrow(chainDf) == 0) {
      return(abort(paste0(
        "Could not fetch chain mappings for ", r$pdbViewer$name,
        " from PDBe. Check the code and your connection."
      )))
    }

    # Join logFC + chains via UniProt
    joinedDf <- fct_join_logfc_chains(hexCols, chainDf)

    return(joinedDf)
  })

  observe({
    req(input$range)
    r$range <- input$range
  })

  observeEvent(paintingdata(), {
    r$painting <- FALSE
    r$painted_once <- TRUE
  })

  # This is the reactive expression that prepares the final coloring data
  hex_colorset <- reactive({
    req(paintingdata())
    df <- paintingdata()
    df$mappings.chain_id <- paste0(":", trimws(df$mappings.chain_id))
    df %>%
      dplyr::select(sele = mappings.chain_id, colorValue = hex_from_scales)
  })

  hex_colorset_values <- reactive({
    req(hex_colorset())
    hex_colorset_values <- apply(hex_colorset(), 1, as.list)
    hex_colorset_values <- unname(hex_colorset_values)
    return(hex_colorset_values)
  })

  # Compute gene_map once, when paintingdata() finishes
  observeEvent(paintingdata(), {
    req(paintingdata())
    full_map <- dplyr::ungroup(paintingdata())

    gene_map <- data.frame(
      genename = as.character(full_map$genename),
      uniprot = as.character(full_map$uniprot),
      mappings.chain_id = paste0(":", trimws(full_map$mappings.chain_id)),
      stringsAsFactors = FALSE
    )

    # genename only comes from the uploaded logFC file, so every chain absent
    # from that file had none and could not be selected by name. Fall back to
    # the yeast reference name for the chain's own accession, which names all
    # of them - including the uncharacterised ORFs that have no gene name.
    missing <- is.na(gene_map$genename) | !nzchar(gene_map$genename)
    if (any(missing)) {
      gene_map$genename[missing] <- uniprotToGene(gene_map$uniprot[missing])
    }

    r$gene_map <- unique(gene_map[!is.na(gene_map$genename), , drop = FALSE])
  })


  # Only do the coloring when the button is pressed
  observe({
    req(hex_colorset_values())

    for (i in seq_along(hex_colorset_values())) {
      Viewer_proxy %>% addSelection(
        type = "surface",
        param = list(
          name = "PDBPainter",
          sele = hex_colorset_values()[[i]][["sele"]],
          color = hex_colorset_values()[[i]][["colorValue"]]
        )
      )
    }
  })

  observeEvent(input$clearpaint, {
    Viewer_proxy %>%
      removeSelection(
        name = c("PDBPainter")
      )
    Viewer_proxy %>% updateRepresentation("structure", param = list(visible = TRUE))
  })


  observeEvent(input$openSelectionPanel, {
    r$selection$panel_open_trigger <- runif(1)
    insertUI_floating_panel(
      panel_id = "selection_ui_1",
      r = r)
  })

}
