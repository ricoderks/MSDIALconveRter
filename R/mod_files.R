#' files UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib navset_card_tab nav_panel card page_sidebar sidebar tooltip
#' @importFrom bsicons bs_icon
#' @importFrom DT dataTableOutput
#'
mod_files_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::navset_card_tab(
      #---------------------------------------------------------- meta data ----
      bslib::nav_panel(
        title = "Meta data",
        bslib::card(
          bslib::page_sidebar(
            sidebar = bslib::sidebar(
              shiny::div(
                shiny::h4("Column selection"),
                shiny::selectInput(
                  inputId = ns("metadata_select_filename"),
                  label = "File names",
                  choices = NULL
                ),

                style = "font-size:85%"
              )

            ),
            shiny::fileInput(
              inputId = ns("metadata_file"),
              label = "Data file:",
              multiple = FALSE,
              accept = c(".csv", ".tsv", ".txt", ".xlsx")
            ),
            bslib::card_body(
              shiny::div(
                DT::dataTableOutput(
                  outputId = ns("metadata_preview_table")
                ),
                style = "font-size:75%;"
              )
            )
          )
        )
      ), # end navpanel meta data
      #----------------------------------------------------------- raw data ----
      bslib::nav_panel(
        title = "Raw data",
        bslib::card(
          bslib::page_sidebar(
            sidebar = bslib::sidebar(
              title = "Raw data",
              open = TRUE,
              shiny::radioButtons(
                inputId = ns("raw_select_omics"),
                label = "Select omics :",
                choices = c("Lipidomics" = "lip",
                            "Metabolomics" = "met"),
                selected = "lip"
              ),
              shiny::checkboxGroupInput(
                inputId = ns("raw_which_files"),
                label = bslib::tooltip(
                  trigger = list(
                    "Upload files",
                    bsicons::bs_icon(name = "info-circle")
                  ),
                  "Which result files from MS-DIAL to upload!"
                ),
                choices = c("Positive" = "pos", "Negative" = "neg"),
                selected = c("pos", "neg")
              ),
              shiny::selectInput(
                inputId = ns("raw_select_table"),
                label = "Select table",
                choices = c("Raw table" = "raw_data",
                            "Filtered table" = "clean_data_wide"),
                selected = "clean_data"
              )
            ),
            bslib::card_body(
              height = "15%",
              bslib::layout_column_wrap(
                width = 1 / 2,
                shiny::fileInput(
                  inputId = ns("rawdata_pos_file"),
                  label = "Data file positive:",
                  multiple = FALSE,
                  accept = c(".txt")
                ),
                shiny::fileInput(
                  inputId = ns("rawdata_neg_file"),
                  label = "Data file negative:",
                  multiple = FALSE,
                  accept = c(".txt")
                ),
                shiny::textOutput(
                  outputId = ns("rawdata_status")
                )
              )
            ),
            bslib::card_body(
              height = "60%",
              shiny::div(
                DT::dataTableOutput(
                  outputId = ns("rawdata_preview_table")
                ),
                style = "font-size:75%;"
              )
            ),
            bslib::card_body(
              bslib::layout_column_wrap(
                width = 1 / 2,
                height = "15%",
                shinyWidgets::progressBar(
                  id = ns("sample_count_bar"),
                  title = "Sample count",
                  value = 0,
                  total = 100,
                  unit_mark = "%"
                ),
                shinyWidgets::progressBar(
                  id = ns("feature_count_bar"),
                  title = "Feature count",
                  value = 0,
                  total = 100,
                  unit_mark = "%"
                )
              )
            )
          )
        )
      ) # end navpanel raw data
    ) # end card
  )
}

#' files Server Functions
#'
#' @param r reactive values for communication between modules.
#'
#' @noRd
mod_files_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #------------------------------------------------------------ meta data ----
    shiny::observeEvent(
      input$metadata_file, {
        shiny::req(input$metadata_file)

        r$files$meta_file <- input$metadata_file$name
        file_path <- input$metadata_file$datapath
        data_table <- read_data(file_path = file_path)

        r$tables$meta_data <- data_table
        print("Meta data read")

        # update column names
        column_names <- colnames(r$tables$meta_data)
        shiny::updateSelectInput(
          inputId = "metadata_select_filename",
          choices = sort(column_names),
          selected = ifelse(any(grepl(x = column_names,
                                      pattern = ".*filename*",
                                      ignore.case = TRUE)),
                            grep(x = column_names,
                                 pattern = ".*filename.*",
                                 ignore.case = TRUE,
                                 value = TRUE)[1],
                            column_names[1])
        )
      })

    output$metadata_preview_table <- DT::renderDataTable({
      shiny::req(r$tables$meta_data)

      data_table <- r$tables$meta_data

      DT::datatable(data = data_table,
                    rownames = FALSE,
                    options = list(dom = "t",
                                   pageLength = -1))
    })

  })
}

## To be copied in the UI
# mod_files_ui("files_1")

## To be copied in the server
# mod_files_server("files_1")
