#' convert UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList HTML showNotification downloadButton downloadHandler
#' @importFrom bslib card page_sidebar sidebar card_body tooltip layout_column_wrap
#' @importFrom bsicons bs_icon
#' @importFrom shinyWidgets progressBar updateProgressBar
#' @importFrom utils write.csv
#'
mod_convert_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      bslib::page_sidebar(
        sidebar = bslib::sidebar(
          shiny::div(
            shiny::div(
              shiny::actionButton(
                inputId = ns("convert_btn"),
                label = "Convert"
              ),
              style = "display:flex;justify-content:center"
            ),
            shiny::hr(),
            shiny::uiOutput(
              outputId = ns("convert_ui")
            ),
            shiny::hr(),
            shiny::div(
              shiny::downloadButton(
                outputId = ns("convert_download_expdata"),
                label = "Download data"
              ),
              style = "display:flex;justify-content:center"
            ),
            shiny::p(),
            shiny::div(
              shiny::downloadButton(
                outputId = ns("convert_download_featuredata"),
                label = "Download feature data"
              ),
              style = "display:flex;justify-content:center"
            ),
            style = "font-size:85%"
          ) # end div
        ), # end side bar
        bslib::card_body(
          bslib::layout_column_wrap(
            width = 1 / 2,
            height = "25%",
            shinyWidgets::progressBar(
              id = ns("sample_count_bar"),
              title = "Sample count",
              value = 0,
              total = 1,
              unit_mark = "%"
            ),
            shinyWidgets::progressBar(
              id = ns("feature_count_bar"),
              title = "Feature count",
              value = 0,
              total = 1,
              unit_mark = "%"
            )
          )
        ),
        bslib::card_body(
          shiny::div(
            DT::dataTableOutput(
              outputId = ns("convertdata_preview_table")
            ),
            style = "font-size:75%;"
          )
        )
      )
    )
  )
}

#' convert Server Functions
#'
#' @param r reactive values for communication between modules.
#'
#' @noRd
#'
mod_convert_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    output$convert_ui <- shiny::renderUI({
      shiny::req(r$omics)

      switch(
        r$omics,
        "met" = shiny::tagList(
          shiny::h4("Output options"),
          shiny::radioButtons(
            inputId = ns("convert_select_option"),
            label = "Column names:",
            choices = list("Feature names" = "names",
                           "Feature ID's" = "ids")
          ),
          shiny::checkboxGroupInput(
            inputId = ns("convert_remove"),
            label = bslib::tooltip(
              trigger = list(
                "Remove features:",
                bsicons::bs_icon(name = "info-circle")
              ),
              "Which features do you want to remove?"
            ),
            choices = c(
              "Unknown" = "unknown",
              "Low score" = "low score",
              "no MS2" = "no MS2"
            ),
            selected = c("unknown", "low score", "no MS2")
          ),
          shiny::radioButtons(
            inputId = ns("convert_duplicates"),
            label = "Duplicates:",
            choices = list(
              "Sum" = "sum",
              "Average" = "average",
              "Rename" = "rename"
            ),
            selected = "rename"
          )
        ),
        "lip" = shiny::tagList(
          shiny::checkboxGroupInput(
            inputId = ns("convert_remove"),
            label = bslib::tooltip(
              trigger = list(
                "Remove features:",
                bsicons::bs_icon(name = "info-circle")
              ),
              "Which features do you want to remove?"
            ),
            choices = c(
              "Unknown" = "unknown",
              "Low score" = "low score",
              "no MS2" = "no MS2"
            ),
            selected = c("unknown", "low score", "no MS2")
          ),
          shiny::radioButtons(
            inputId = ns("convert_duplicates"),
            label = "Duplicates:",
            choices = list(
              "Sum" = "sum",
              "Average" = "average",
              "Rename" = "rename"
            ),
            selected = "rename"
          )
        )
      )

    })


    observeEvent(input$convert_btn, {
      shiny::req(r$tables$raw_data,
                 input$convert_duplicates,
                 r$omics)
      if(r$omics == "lip") {
        print("Convert data")
        res <- extract_data(
          data = r$tables$raw_data,
          selected_cols = r$tables$meta_data[, r$meta$filename_col],
          clean = input$convert_remove,
          duplicates = input$convert_duplicates,
          omics = r$omics
        )

        r$tables$convert_data <- res$exp_data
        r$tables$feature_data <- res$feature_data
      } else {
        if(!(is.null(input$convert_remove) & input$convert_select_option == "names")) {
          print("Convert data")
          res <- extract_data(
            data = r$tables$raw_data,
            selected_cols = r$tables$meta_data[, r$meta$filename_col],
            clean = input$convert_remove,
            duplicates = input$convert_duplicates,
            option = input$convert_select_option,
            omics = r$omics
          )

          r$tables$convert_data <- res$exp_data
          r$tables$feature_data <- res$feature_data
        } else {
          r$tables$convert_data <- NULL
          shiny::showNotification(
            ui = "ERROR: Can not create table! When 'Feature names' are selected as column names, the 'unknown' features need to be removed!",
            duration = NULL,
            type = "error"
          )
        }
      }
    })


    output$convertdata_preview_table <- DT::renderDataTable({
      shiny::req(r$tables$convert_data)

      print("Show table")
      data_table <- r$tables$convert_data

      shinyWidgets::updateProgressBar(
        session = session,
        id = "sample_count_bar",
        title = "Sample count",
        value = nrow(data_table),
        total = length(r$tables$meta_data[, r$meta$filename_col])
      )

      shinyWidgets::updateProgressBar(
        session = session,
        id = "feature_count_bar",
        title = "Feature count",
        value = ncol(data_table) - 1,
        total = nrow(r$tables$raw_data)
      )

      if(ncol(data_table) > 100) {
        data_table <- data_table[, 1:100]
        shiny::showNotification(
          ui = "INFO: Only the first 100 columns are shown!",
          type = "message"
        )
      }

      output_table <- DT::datatable(
        data = data_table,
        rownames = FALSE,
        options = list(dom = "t",
                       pageLength = -1)
      )

      return(output_table)
    })


    output$convert_download_expdata <- shiny::downloadHandler(
      filename = "experiment_data.csv",
      content = function(file) {
        utils::write.csv(x = r$tables$convert_data,
                         file = file,
                         row.names = FALSE)
      }
    )


    output$convert_download_featuredata <- shiny::downloadHandler(
      filename = "feature_data.csv",
      content = function(file) {
        utils::write.csv(x = r$tables$feature_data,
                         file = file,
                         row.names = FALSE)
      }
    )

  })
}

## To be copied in the UI
# mod_convert_ui("convert_1")

## To be copied in the server
# mod_convert_server("convert_1")
