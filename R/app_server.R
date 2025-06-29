#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#'
#' @noRd
#'
app_server <- function(input, output, session) {
  # increase upload limit
  options(shiny.maxRequestSize = 30 * 1024^2)

  # for communication between the modules
  r <- shiny::reactiveValues(
    omics = NULL,
    files = list(
      meta_file = NULL,
      pos_file = NULL,
      neg_file = NULL
    ),
    tables = list(
      meta_data = NULL,
      pos_data = NULL,
      neg_data = NULL,
      raw_data = NULL,
      convert_data = NULL,
      feature_data = NULL
    ),
    meta = list(
      filename_col = NULL
    )
  )

  # load the modules
  mod_files_server(id = "files_1",
                   r = r)

  mod_convert_server(id = "convert_1",
                     r = r)

  mod_about_server(id = "about_1")
}
