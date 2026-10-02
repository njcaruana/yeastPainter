#' snapshot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_snapshot_ui <- function(id) {
  ns <- NS(id)
  # Content only: a section of the "Structure" sidebar menu. No filename field
  # - the browser's save dialog names the file.
  tagList(
    materialSwitch(inputId = ns("antialias"), label = "antialias", right = TRUE, value = TRUE, status = "primary"),
    materialSwitch(inputId = ns("trim"), label = "trim", right = TRUE, value = TRUE, status = "primary"),
    materialSwitch(inputId = ns("transparent"), label = "transparent", right = TRUE, value = TRUE, status = "primary"),
    actionButton(ns("snapshot"), "Snapshot", style = "display: inline-block; color: #fff; background-color: #344154; border-color: #fff;padding: 5px 14px 5px 14px; margin: 15px 15px 15px 10px; ",)
  )
}

#' snapshot Server Function
#'
#' @noRd
mod_snapshot_server <- function(input, output, session, globalSession, r){
  ns <- session$ns


  observeEvent(input$snapshot, {
    # Named after the structure where there is one; the save dialog is where
    # the user renames it.
    fileName <- if (!is.null(r$pdbViewer$name) && nzchar(r$pdbViewer$name)) {
      paste0(r$pdbViewer$name, "_snapshot")
    } else {
      "structure_snapshot"
    }

    # The legend is only drawn once a structure has actually been painted,
    # matching the condition in mod_NGLVieweROutput's logfc_legend output.
    legend <- NULL
    if (isTRUE(r$painted_once) && !is.null(r$range)) {
      min_val <- r$range[1]
      max_val <- r$range[2]
      mid_val <- if (min_val < 0 && max_val > 0) 0 else (min_val + max_val) / 2
      legend <- list(
        min = sprintf("%.2f", min_val),
        mid = sprintf("%.2f", mid_val),
        max = sprintf("%.2f", max_val)
      )
    }

    session$sendCustomMessage("composite-snapshot", list(
      nglId = "NGLVieweROutput_ui_1-structure", # Ensure this matches your UI ID
      fileName = fileName,
      factor = 1,
      antialias = isTRUE(input$antialias),
      trim = isTRUE(input$trim),
      transparent = isTRUE(input$transparent),
      legend = legend,
      statusInput = session$ns("snapshotStatus")
    ))
  })

  # The browser reports back so a failed snapshot is not silent.
  observeEvent(input$snapshotStatus, {
    status <- input$snapshotStatus
    if (identical(status, "ok")) {
      showNotification("Snapshot saved.", type = "message", duration = 4)
    } else {
      showNotification(
        switch(status,
          "no-structure"    = "Load a structure before taking a snapshot.",
          "ngl-unavailable" = "The 3D viewer has not finished loading yet - try again in a moment.",
          paste0("Snapshot failed (", status, ").")
        ),
        type = "warning",
        duration = 8
      )
    }
  })


#Viewer_proxy <- NGLVieweR_proxy("NGLVieweROutput_ui_1-structure", session = globalSession)


# observeEvent(input$snapshot, {
#   req(r$range)
#   fileName <- if (nchar(input$snapshotName) != 0) input$snapshotName else "structure_snapshot"
#
#     Viewer_proxy %>% snapShot(
#       fileName = fileName,
#       param = list(
#         factor = 1,
#         antialias = isolate(input$antialias),
#         trim = isolate(input$trim),
#         transparent = isolate(input$transparent)
#       )
#     )
#
# })


}

# observeEvent(input$snapshot, {
#   if (nchar(input$snapshotName) != 0) {
#     fileName <- input$snapshotName
#   } else {
#     fileName <- "structure_snapshot"
#   }
#   Viewer_proxy %>% snapShot(
#     fileName = fileName,
#     param = list(
#       factor = 1,
#       antialias = isolate(input$antialias),
#       trim = isolate(input$trim),
#       transparent = isolate(input$transparent)
#     )
#   )
# })


## To be copied in the UI
# mod_snapshot_ui("snapshot_ui_1")

## To be copied in the server
# callModule(mod_snapshot_server, "snapshot_ui_1")

