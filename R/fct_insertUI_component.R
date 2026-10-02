#' Insert components to UI
#'
#' @description
#' Function to add UI components on submission
#'
#' @param type type of components. E.g. label, selection, contact.
#' @param name name of the components. Use \code{insertUI_name()} to generate a unique name.
#' @param uu_id uniqueID used to name the UI components.
#'
#' @examples
#' \dontrun{
#' insertUI_component('selection', insertUI_name('ligand'),
#'                    counter = 3, uu_id = "575ceb43-9ff5-40fb-85a4-9a8e9e06ceff")
#' }
#' @export
insertUI_component <- function(type, name, uu_id){
  insertUI(
    selector = sprintf('#%sPlaceholder', type),
    ui = tags$div(
      style = 'display:flex;',
      id = sprintf('%s-%s', type, uu_id),
      class = sprintf("%sholder", type),
      style = 'font-size: 100%; padding: 0;',
      actionLink(
        sprintf('%sLink-%s', type, uu_id),
        label = name,
        class = sprintf("%sLink btn-link", type),
      ),
      actionLink(
        sprintf('%sRemove-%s', type, uu_id),
        label = NULL,
        icon = icon('trash'),
        class = sprintf("%sRemove", type),
        style = 'color: red;'
      )
    )
  )
}

#' component UI name
#'
#' @description
#' Function to add a unique name
#'
#' @param name name of the selection. Defaults to selection-\code{counter} if input is \code{NULL} or \code{""}
#' @param counter value to create unique \code{name} if none is provided
#' @param type type of selection. E.g. label, selection, contact.
#'
#' @examples
#' \dontrun{
#' insertUI_name('selection', '', 3)
#' "selection-3"
#' }
#' @export
insertUI_name <- function(type, name = NULL, counter = 0) {

  if (is.null(name)) {
    name <- type
  } else if (nchar(name) < 1) {
    name <- type
  } else {
    name <- name
  }

  if (name == type && counter != 0) {
    name <- sprintf("%s-%s", name, counter)
  }
  return(name)
}

#' components UI selection
#'
#' @description
#' Function to transform ngl.js selection for UI component
#'
#' @param selection ngl.js query to be transformed. Uses \code{selection_to_ngl()}
#' @param sequence protein sequence in string format.
#' @param gene_map gene map selection.
#'
#' @examples
#' \dontrun{
#' insertUI_selection("1-3 OR <GHK>", "NGLSDFGHK")
#' }
#' @export
insertUI_selection <- function(sequence, selection, gene_map) {

  if (nchar(isolate(selection)) > 0) {
    selection <- selection_to_ngl(sequence, selection, gene_map)
  } else {
    selection <- 'none'
  }
  return(selection)
}


#' Insert Floating Panel
#'
#' @description
#' Helper to inject the floating selection panel into the document body.
#'
#' @param panel_id The module id for the floating panel.
#' @param r The global reactiveValues object containing \code{r$floatingPanel$inserted}.
#'
#' @export
insertUI_floating_panel <- function(panel_id, r) {
  # Opening a panel that is already open used to insert a second copy of it.
  # Every input inside then existed twice under the same id, which breaks the
  # bindings. Insert once and do nothing on later opens.
  if (isTRUE(r$floatingPanel_inserted)) {
    return(invisible(FALSE))
  }

  # mod_selection_ui() already wraps itself in a div carrying
  # "<panel_id>-floating_panel", so it is inserted directly. Wrapping it in
  # another div with the same id produced duplicate ids in the document.
  insertUI(
    selector = "body",
    where = "beforeEnd",
    ui = mod_selection_ui(panel_id)
  )

  r$floatingPanel_inserted <- TRUE
  invisible(TRUE)
}

# insertUI_floating_panel <- function(panel_id) {
#   # Only insert if the DOM does not exist yet
#   if (!exists(paste0(panel_id, "-floating_panel"), envir = shiny::getDefaultReactiveDomain()$input)) {
#     insertUI(
#       selector = "body",
#       where = "beforeEnd",
#       ui = tags$div(
#         id = paste0(panel_id, "-floating_panel"),
#         mod_selection_ui(panel_id)
#       )
#     )
#     shinyjs::hide(paste0(panel_id, "-floating_panel"))
#   }
# }
