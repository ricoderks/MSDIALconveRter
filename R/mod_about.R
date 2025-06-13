#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList p a h3 verbatimTextOutput renderPrint
#' @importFrom bslib card
#' @importFrom sessioninfo session_info
#'
mod_about_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      shiny::h3("Issues"),
      shiny::p("If you have any ideas to extend this shiny app please send me an email. If you have any issue please send me an email or go to the ",
               shiny::a("issue tracker.", href = "http://github.com/ricoderks/MSDIALconveRter/issues", target = "_blank"),
               "Cheers, Rico"),
      shiny::h3("Session info"),
      shiny::verbatimTextOutput(ns("about_session"))
    )
  )
}

#' about Server Functions
#'
#' @noRd
mod_about_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$about_session <- shiny::renderPrint({
      sessioninfo::session_info()
    })
  })
}

## To be copied in the UI
# mod_about_ui("about_1")

## To be copied in the server
# mod_about_server("about_1")
