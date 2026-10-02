#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinydashboardPlus
#' @import colourpicker
#' @import bsplus
#' @import shinyWidgets
#' @import shinyjs
#' @import NGLVieweR
#' @import shinyjqui
#' @import uuid
#' @import readr
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Your application UI logic
    dashboardPage(
      title = "Yeast Painter",
      skin = "black",
      dashboardHeader(
        title = tags$img(
          src = "www/rdmslogo_long.png",
          height = "40px",
          style = "height: 60%; vertical-align: middle;"),
        tags$li(
          class = "dropdown header-link",
          id = "header-link-home",
          actionLink(
            "home",
            label = "home",
            onclick = "window.open('https://rdms.app', '_blank')"
          )
        ),
        tags$li(
          class = "dropdown header-link",
          id = "header-link-complexity",
          actionLink(
            "complexity",
            label = "complexity",
            onclick = "window.open('https://rdmassspec.shinyapps.io/rangeapp/', '_blank')"
          )
        ),
        tags$li(
          class = "dropdown header-link",
          id = "header-link-rangeplot",
          actionLink(
            "rangeplot",
            label = "rangefinder",
            onclick = "window.open('https://rdmassspec.shinyapps.io/rangeapp/', '_blank')"
          )
        ),
        tags$li(
          class = "dropdown",
          id = "github_logo",
          actionLink("github",
                     label = NULL, icon = icon("github"),
                     onclick = "window.open('https://github.com/njcaruana', '_blank')"
          )
        )
      ),
      dashboardSidebar(
        minified = FALSE,
        # One menu: every control here acts on the single 3D viewport. Ordered
        # the way the work runs - load a structure, bring fold changes to it,
        # settle how it is drawn, then paint.
        sidebarMenu(
          menuItem("Structure",
            tabName = "structureTab", icon = icon("cube"), startExpanded = TRUE,
            sidebarSection("Load a structure"),
            mod_pdbViewer_ui("pdbViewer_ui_1"),
            sidebarDivider(),
            sidebarSection("Fold changes"),
            mod_pdbPainter_upload_ui("pdbPainter_ui_1"),
            sidebarDivider(),
            # Appearance before Paint: how the structure is drawn is settled
            # before it is coloured, not after.
            sidebarSection("Appearance"),
            mod_pdbViewer_appearance_ui("pdbViewer_ui_1"),
            sidebarDivider(),
            sidebarSection("Paint"),
            mod_pdbPainter_ui("pdbPainter_ui_1"),
            sidebarDivider(),
            sidebarSection("Snapshot"),
            mod_snapshot_ui("snapshot_ui_1"),
            sidebarDivider(),
            sidebarSection("View"),
            mod_stage_ui("stage_ui_1")
          )
        )
      ),
      dashboardBody(

        # Replaces the tab strip, which is hidden in styles.css.
        uiOutput("stageRail"),

        tabsetPanel(
          id = "tabset1",
          tabPanel(
            title = "About",
            value = "About",
            mod_about_ui("about_ui_1")
          ),
          tabPanel(
            title = "PDB Painter",
            value = "PDB",
            fluidRow(mod_NGLVieweROutput_ui("NGLVieweROutput_ui_1"))
          )
        )

      )
    )
  )
}

#' Version string for the bundled www resources.
#'
#' @description
#' Derived from the most recent modification time under `app/www`, so any edit
#' to a bundled script or stylesheet produces a new URL and browsers stop
#' serving a cached copy. For an installed package the mtimes are set at
#' install time, so the value is stable between restarts and changes on
#' reinstall.
#'
#' @noRd
www_resource_version <- function() {
  files <- list.files(app_sys("app/www"), recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) return("0.0.1")
  paste0("0.0.", as.integer(max(file.mtime(files))))
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(
      ext = 'ico'
    ),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "yeastPainterApp",
      # Version the bundle by the newest file in www/. golem's default is a
      # fixed "0.0.1", which keeps the URL identical forever, so browsers serve
      # stale JS and CSS after an edit. Deriving it from mtimes means editing a
      # file changes the URL and the browser refetches.
      version = www_resource_version()
    ),
    # bundle_resources() versions the scripts but links the stylesheet as a
    # bare "www/styles.css", which browsers then cache indefinitely - an edited
    # stylesheet never reaches the page. Re-link it with the same version so a
    # CSS change actually shows up.
    tags$link(
      rel = "stylesheet", type = "text/css",
      href = paste0("www/styles.css?v=", www_resource_version())
    ),
    useShinyjs(),
    extendShinyjs(text = jsboxCollapse, functions = c("collapse")), #Collapse box when clicking on title
    # Input modals
    bsplus::use_bs_tooltip(),
    bsplus::use_bs_popover(),
    bs_input_modal("select_modal", "Selection Language", htmlTemplate(app_sys("app/www/selectionModal.html")), "medium"),
    bs_input_modal("contact_modal", "Contact Selection", htmlTemplate(app_sys("app/www/contactModal.html")), "medium")
  )
}
