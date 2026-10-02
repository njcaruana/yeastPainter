#' The workflow stage rail.
#'
#' @description
#' A horizontal stepper across the top of the body. This app has a single
#' working view, so the rail is a progress strip rather than navigation: it
#' shows the three steps in order and ticks each once it has produced what the
#' next one consumes.
#'
#' The sidebar holds the controls.
#'
#' @name stageRail
#' @import shiny
NULL

#' A labelled divider inside a sidebar menu.
#'
#' @description
#' The Structure menu gathers every control acting on the one 3D viewport, so
#' its sections need marking off.
#'
#' @param label Section heading.
#' @return A tag.
#' @export
sidebarSection <- function(label) {
  tags$div(class = "sidebar-section", label)
}

#' @rdname sidebarSection
#' @export
sidebarDivider <- function() {
  tags$hr(class = "sidebar-divider")
}

#' Stage definitions, in workflow order.
#'
#' @description
#' `tab` is the `value` of the corresponding tabPanel, so a step can drive
#' `updateTabsetPanel()` directly. All three steps act on the same view here,
#' so they all point at it.
#'
#' @return A list of stage definitions.
#' @export
appStages <- function() {
  list(
    list(key = "Structure", tab = "PDB",
         label = "Structure",    hint = "Load a PDB"),
    list(key = "Data",      tab = "PDB",
         label = "Fold changes", hint = "Upload logFC"),
    list(key = "Paint",     tab = "PDB",
         label = "Paint",        hint = "Colour the structure")
  )
}

#' Which stages are complete.
#'
#' @description
#' A stage counts as done once it has produced the thing later stages consume,
#' not merely when it has been visited.
#'
#' @param r The app's shared reactive values.
#' @return A named logical vector over `appStages()` keys.
#' @export
stageStatus <- function(r) {
  c(
    # An explicit flag rather than pdbViewer$name: the viewer assigns the
    # bundled example a name on first render, which would tick this stage
    # merely for opening the app.
    Structure = isTRUE(r$structureLoaded),
    Data      = isTRUE(r$logfcLoaded),
    Paint     = isTRUE(r$painted_once)
  )
}

#' Render the rail.
#'
#' @param status Output of `stageStatus()`.
#' @param active `value` of the tab currently shown, or `NULL`.
#' @return A `tagList`.
#' @export
stageRail <- function(status, active = NULL) {
  stages <- appStages()

  steps <- lapply(seq_along(stages), function(i) {
    s <- stages[[i]]
    done <- isTRUE(status[[s$key]])
    is_active <- identical(active, s$tab)

    tags$li(
      class = paste(
        "wf-step",
        if (done) "wf-done" else "",
        if (is_active) "wf-active" else ""
      ),
      actionLink(
        inputId = paste0("goto_", s$key),
        label = tagList(
          tags$span(
            class = "wf-mark",
            if (done) icon("check") else tags$span(i)
          ),
          tags$span(
            class = "wf-text",
            tags$span(class = "wf-label", s$label),
            tags$span(class = "wf-hint", s$hint)
          )
        )
      )
    )
  })

  tags$div(
    class = "wf-rail",
    tags$ul(class = "wf-steps", steps),
    # About is not a workflow stage, but it still needs a way in now that the
    # tab strip is hidden.
    tags$div(
      class = "wf-aside",
      actionLink("goto_About", label = tagList(icon("circle-info"), " About")),
      tags$span(
        class = "wf-count",
        sprintf("%d/%d", sum(status, na.rm = TRUE), length(stages))
      )
    )
  )
}
