#' About UI Function
#'
#' @param id Internal parameters for shiny
#'
#' @noRd
mod_about_ui <- function(id) {
  ns <- NS(id)

  # Wrapper so the panel headings can take the header's typeface without
  # restyling every box in the app.
  div(
    class = "about-page",
    fluidRow(
      box(
        width = 12, status = "primary", solidHeader = FALSE,
        title = "Yeast Painter",
        div(
          class = "about-intro",
          tags$p(
            "Yeast Painter colours a 3D structure by fold change. Give it a ",
            tags$strong("PDB code"), " and a ", tags$strong("logFC table"),
            ", and each chain is shaded on a blue-white-red scale by how far ",
            "its protein moved, producing a topographical heatmap of the ",
            "complex that can be exported as a transparent PNG."
          ),
          tags$p(
            "Identifiers are resolved against the reviewed ",
            tags$em("Saccharomyces cerevisiae"), " S288C proteome, so standard ",
            "gene names (", tags$code("COX4"), "), systematic ORF names (",
            tags$code("YGL187C"), ") and UniProt accessions all work without ",
            "relabelling your data."
          ),
          tags$p(
            "Work through the three steps in the bar above. Each one ticks once ",
            "it has produced what the next needs, and all the controls live in ",
            "the Structure menu on the left."
          ),
          actionButton(
            ns("start"), "Start with your data",
            icon = icon("arrow-right"),
            class = "about-cta"
          )
        )
      )
    ),

    fluidRow(
      box(
        width = 12, status = "primary", solidHeader = FALSE,
        title = "How it works",
        # The grid class goes on the rendered content, not around the
        # uiOutput: its wrapper div would otherwise be the grid's only child.
        uiOutput(ns("stageCards"))
      )
    ),

    fluidRow(
      box(
        width = 12, status = "primary", solidHeader = FALSE,
        title = "Your logFC file",
        collapsible = TRUE,
        tags$p(
          class = "about-note",
          "Columns are found by name, not by position, so extra columns are ",
          "ignored and the order does not matter. Two are needed:"
        ),
        uiOutput(ns("columns")),
        tags$p(
          class = "about-note",
          "csv, tsv, txt and xlsx are all read. Proteins that cannot be ",
          "matched are counted in the sidebar and their chains are left grey, ",
          "as are chains for proteins that were not measured."
        )
      )
    ),

    fluidRow(
      box(
        width = 12, status = "primary", solidHeader = FALSE,
        title = "Data sources",
        collapsible = TRUE,
        tags$p(
          class = "about-note",
          "Yeast Painter is an interface to work already done by others. ",
          "Structures, chain mappings and gene identifiers all come from the ",
          "resources below."
        ),
        uiOutput(ns("sources"))
      )
    ),

    uiOutput(ns("colophon"))
  )
}

#' Stage cards shown on the About page.
#'
#' @description
#' Labels come from `appStages()` so the page cannot drift from the rail above
#' it; only the longer copy lives here.
#'
#' @noRd
about_stage_copy <- function() {
  list(
    Structure = list(
      icon = "cube",
      text = "Enter a PDB code. The structure is fetched from RCSB and its chains are mapped to UniProt accessions through PDBe."
    ),
    Data = list(
      icon = "file-arrow-up",
      text = "Upload a table of fold changes keyed by gene name, ORF name or accession. The sidebar reports how many of them matched."
    ),
    Paint = list(
      icon = "palette",
      text = "Choose the logFC range the colour scale spans, then paint. Individual subunits can be selected and recoloured by name, and the result exported with its legend."
    )
  )
}

#' Columns read from an uploaded fold-change file.
#'
#' @noRd
about_columns <- function() {
  list(
    list(name = "Fold change",
         accepted = "logFC, log2FC, log2FoldChange, foldChange",
         note = "Numeric. Rows without a number here are dropped."),
    list(name = "Identifier",
         accepted = "gene, geneName, symbol, ORF, systematicName",
         note = "Standard gene names or systematic ORF names."),
    list(name = "Identifier (alternative)",
         accepted = "uniprot, uniprotID, accession",
         note = "Used directly when present; isoform suffixes are dropped.")
  )
}

#' The reference resources the app is built on.
#'
#' @noRd
about_sources <- function() {
  list(
    list(name = "RCSB PDB", url = "https://www.rcsb.org/",
         use = "Structure files"),
    list(name = "PDBe", url = "https://www.ebi.ac.uk/pdbe/",
         use = "UniProt to chain mappings used for painting"),
    list(name = "UniProt", url = "https://www.uniprot.org/",
         use = "Reviewed S. cerevisiae S288C proteome: gene, ORF and accession resolution"),
    list(name = "SGD", url = "https://www.yeastgenome.org/",
         use = "Systematic gene nomenclature the ORF names follow"),
    list(name = "NGL Viewer", url = "https://nglviewer.org/",
         use = "The 3D molecular graphics engine")
  )
}

#' About Server Function
#'
#' @param id Internal parameters for shiny
#' @param r The app's shared reactive values, used to move to a stage.
#'
#' @noRd
mod_about_server <- function(id, r = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$stageCards <- renderUI({
      copy <- about_stage_copy()
      cards <- lapply(seq_along(appStages()), function(i) {
        s <- appStages()[[i]]
        info <- copy[[s$key]]
        # "action-button" is what Shiny's binding looks for, so the card
        # itself becomes the input rather than needing a link inside it.
        tags$a(
          class = "action-button about-card",
          id = ns(paste0("card_", s$key)),
          href = "#",
          tags$div(
            class = "about-card-head",
            tags$span(class = "about-card-num", i),
            tags$span(class = "about-card-icon", icon(info$icon)),
            tags$span(class = "about-card-title", s$label)
          ),
          tags$p(class = "about-card-text", info$text)
        )
      })
      div(class = "about-stages", cards)
    })

    # Cards are shortcuts into their stage.
    lapply(appStages(), function(s) {
      observeEvent(input[[paste0("card_", s$key)]], {
        if (!is.null(r)) r$tabswitch <- s$tab
      }, ignoreInit = TRUE)
    })

    observeEvent(input$start, {
      if (!is.null(r)) r$tabswitch <- appStages()[[1]]$tab
    }, ignoreInit = TRUE)

    output$columns <- renderUI({
      div(class = "about-sources", lapply(about_columns(), function(c) {
        tags$div(
          class = "about-source",
          tags$span(c$name),
          tags$span(class = "about-source-use",
                    tags$code(c$accepted), " — ", c$note)
        )
      }))
    })

    output$sources <- renderUI({
      div(class = "about-sources", lapply(about_sources(), function(s) {
        tags$div(
          class = "about-source",
          tags$a(href = s$url, target = "_blank", rel = "noopener", s$name),
          tags$span(class = "about-source-use", s$use)
        )
      }))
    })

    output$colophon <- renderUI({
      version <- tryCatch(
        as.character(utils::packageVersion("yeastPainterApp")),
        error = function(e) NULL
      )
      tags$div(
        class = "about-colophon",
        tags$span(
          "Yeast Painter",
          if (!is.null(version)) paste0(" v", version)
        ),
        tags$span(class = "about-dot", "·"),
        tags$a(href = "https://rdms.app", target = "_blank", rel = "noopener",
               "rdms.app"),
        tags$span(class = "about-dot", "·"),
        tags$a(href = "https://github.com/njcaruana", target = "_blank",
               rel = "noopener", "source")
      )
    })
  })
}
