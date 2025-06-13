#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#' @importFrom utils packageVersion
#' @importFrom bslib page_navbar nav_panel nav_spacer nav_menu
#'
#' @noRd
#'
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinyjs::useShinyjs(),

    bslib::page_navbar(
      title = paste0("CPM - MSDIAL converter | v", utils::packageVersion("MSDIALconveRter")),
      bslib::nav_panel(
        title = "Files",
        mod_files_ui(id = "files_1")
      ),
      bslib::nav_panel(
        title = "About",
        mod_about_ui(id = "about_1")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "MSDIALconveRter"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
