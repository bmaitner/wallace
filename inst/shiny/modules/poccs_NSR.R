poccs_NSR_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
        actionButton(ns("NSROccs"), "Run NSR")
  )
}
