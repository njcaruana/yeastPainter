#' pdbViewer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pdbViewer_ui <- function(id) {
  ns <- NS(id)
  # The only route into a structure: a PDB code typed in directly. The
  # structure comes from RCSB and its chain-to-accession mapping from PDBe.
  tagList(
    textInput(ns("pdbID_New"), "Enter a PDB code", placeholder = "1KB9"),
    actionButton(ns("loadPDB"), "Load PDB", style="color: #fff; background-color: #243d63; border-color: #fff;padding: 5px 14px 5px 14px;margin: 1px 15px 15px 15px; ")
  )
}

#' pdbViewer appearance UI
#'
#' @description How the loaded structure is drawn.
#'
#' @noRd
mod_pdbViewer_appearance_ui <- function(id) {
  ns <- NS(id)
  tagList(
      selectInput(ns("structureType"), "Structure Type:", c(
        "cartoon","hide","ball+stick","surface","ribbon","backbone",
        "licorice","spacefill","line","contact","helixorient",
        "hyperball","rocket"
      ), selected = "cartoon"),

      selectInput(ns("structureColorScheme"), "Colour Scheme:", c(
        "residueindex","uniform","element","hydrophobicity",
        "bfactor","sstruc","random","resname","chainname",
        "entityindex","entitytype","modelindex","occupancy"
      ), selected = "residueindex"),

      colourpicker::colourInput(ns("structureColor"),
                                label = NULL, "blue", palette = "limited", closeOnClick = TRUE
      )
  )
}

#' fileInput Server Function
#'
#' @noRd
mod_pdbViewer_server <- function(input, output, session, globalSession, r) {
  ns <- session$ns

  Viewer_proxy <- NGLVieweR_proxy("NGLVieweROutput_ui_1-structure", session = globalSession)

  ## ---- PDB LOADING LOGIC ----

  observeEvent(input$loadPDB, {

    r$tabswitch <- "PDB"

    manual <- trimws(input$pdbID_New)

    if (!nzchar(manual)) {
      showNotification(
        "Enter a PDB code to load a structure.",
        type = "warning"
      )
      return()
    }

    pdb_to_load <- toupper(manual)

    loaded <- tryCatch(readFile(pdb_to_load), error = function(e) NULL)
    if (is.null(loaded)) {
      showNotification(
        paste0("Could not download ", pdb_to_load,
               " from RCSB. Check the code and your connection."),
        type = "error", duration = 10
      )
      return()
    }

    r$pdbViewer <- loaded
    r$pdbViewer$name <- pdb_to_load
    r$structureLoaded <- TRUE   # ticks the Structure stage in the rail
    r$stage$fileColor <- "black"
    r$rendering <- TRUE
  })

  ## ---- STRUCTURE CONTROL LOGIC ----

  # initialise once after first PDB load
  observeEvent(r$pdbViewer, {
    r$structure$loaded <- FALSE
  }, once = TRUE)

  observeEvent(r$pdbViewer, {
    if (!r$structure$loaded) {
      if (!is.null(r$pdbViewer$structure)) {
        data <- r$pdbViewer$structure
        type <- if (isFALSE(data$visible)) "hide" else data$type

        updateSelectInput(session, "structureType", selected = type)
        updateSelectInput(session, "structureColorScheme", selected = data$colorScheme)
        colourpicker::updateColourInput(session, "structureColor", value = data$colorValue)
      } else {
        representation <- if (isolate(r$pdbViewer$fileExt) %in% c("cif", "ngl", NULL)) {
          "cartoon"
        } else {
          "ball+stick"
        }

        updateSelectInput(session, "structureType", selected = representation)
        updateSelectInput(session, "structureColorScheme", selected = "residueindex")
        colourpicker::updateColourInput(session, "structureColor", value = "#0000FF")
      }

      r$structure$loaded <- TRUE
    }
  })

  observe({
    req(r$structure$loaded)

    input$structureType
    input$structureColorScheme
    input$structureColor

    Viewer_proxy %>% removeSelection("structure")

    Viewer_proxy %>% addSelection(
      input$structureType,
      param = list(
        name = "structure",
        sele = selection_to_ngl(paste0(r$sequence_df$AA, collapse = ""), input$structureSelection),
        colorScheme = input$structureColorScheme,
        colorValue = input$structureColor
      )
    ) %>% stageParameters(cameraFov = 18)

    if (input$structureType == "hide") {
      visible <- FALSE
      type <- "cartoon"
    } else {
      visible <- TRUE
      type <- input$structureType
    }

    r$structure$structure <- data.frame(
      type = type,
      colorScheme = input$structureColorScheme,
      colorValue = input$structureColor,
      visible = visible
    )
  })

  observeEvent(input$structureColorScheme, {
    if (input$structureColorScheme %in% c("uniform", "element")) {
      shinyjs::show("structureColor")
    } else {
      shinyjs::hide("structureColor")
    }
  })
}
