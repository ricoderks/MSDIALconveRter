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
#' @importFrom DT dataTableOutput formatStyle
#' @importFrom utils head
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
                selected = "met"
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
      shiny::req(r$tables$meta_data,
                 r$meta$filename_col)

      data_table <- r$tables$meta_data

      output_table <- DT::datatable(
        data = data_table,
        rownames = FALSE,
        options = list(dom = "t",
                       pageLength = -1)
      )

      if(!is.null(r$meta$filename_col)) {
        output_table <- output_table |>
          DT::formatStyle(
            columns = r$meta$filename_col,
            backgroundColor = "lightblue"
          )
      }

      return(output_table)
    })


    shiny::observeEvent(
      c(input$metadata_select_filename), {
        r$meta$filename_col <- input$metadata_select_filename
      }
    )

    #------------------------------------------------------------- raw data ----
    shiny::observeEvent(input$raw_select_omics, {
      r$omics <- input$raw_select_omics
      print(r$omics)
    })


    shiny::observeEvent(
      input$raw_which_files,
      {
        if("pos" %in% input$raw_which_files) {
          shinyjs::enable(id = "rawdata_pos_file")
        } else {
          shinyjs::disable(id = "rawdata_pos_file")
        }

        if("neg" %in% input$raw_which_files) {
          shinyjs::enable(id = "rawdata_neg_file")
        } else {
          shinyjs::disable(id = "rawdata_neg_file")
        }
      },
      ignoreNULL = FALSE
    )


    shiny::observeEvent(
      input$rawdata_pos_file,
      {
        shiny::req(input$rawdata_pos_file)

        print("Raw data read - pos file")
        output$rawdata_status <- shiny::renderText({
          ""
        })
        r$files$data_file_pos <- input$rawdata_pos_file
        tmp <- read_msdial(filename = input$rawdata_pos_file$datapath)
        if(any(tmp$`Adduct type` == "[M+H]+")) {
          tmp$`Alignment ID` <- paste("pos", tmp$`Alignment ID`, sep = "_")
          r$tables$pos_data <- tmp
        } else {
          shiny::showNotification(
            ui = "Error: This data doesn't appear to contain any positive mode data!",
            type = "error"
          )
          r$tables$pos_data <- NULL
        }

      })


    shiny::observeEvent(
      input$rawdata_neg_file,
      {
        shiny::req(input$rawdata_neg_file)

        print("Raw data read - neg file")
        output$rawdata_status <- shiny::renderText({
          ""
        })
        r$files$data_file_neg <- input$rawdata_neg_file
        tmp <- read_msdial(filename = input$rawdata_neg_file$datapath)
        if(any(tmp$`Adduct type` == "[M-H]-")) {
          tmp$`Alignment ID` <- paste("neg", tmp$`Alignment ID`, sep = "_")
          r$tables$neg_data <- tmp
        } else {
          shiny::showNotification(
            ui = "Error: This data doesn't appear to contain any negative mode data!",
            type = "error"
          )
          r$tables$neg_data <- NULL
        }

      })


    shiny::observeEvent(
      c(input$rawdata_pos_file,
        input$rawdata_neg_file),
      {
        shiny::req(r$meta$filename_col)

        # is it neg or pos or both data
        if(!is.null(r$tables$pos_data) & !is.null(r$tables$neg_data)) {
          print("Combine data")
          r$tables$raw_data <- rbind.data.frame(r$tables$pos_data,
                                                r$tables$neg_data)
        }
        if(length(input$raw_which_files) == 1) {
          if(!is.null(r$tables$pos_data)) {
            r$tables$raw_data <- r$tables$pos_data
          }
          if(!is.null(r$tables$neg_data)) {
            r$tables$raw_data <- r$tables$neg_data
          }
        }

      })


    output$rawdata_preview_table <- DT::renderDataTable({
      shiny::req(r$tables$raw_data,
                 r$tables$meta_data,
                 r$meta$filename_col)

      data_table <- r$tables$raw_data

      output_table <- DT::datatable(
        data = utils::head(data_table, n = 10),
        rownames = FALSE,
        options = list(dom = "t",
                       pageLength = -1)
      )

      if(any(colnames(r$tables$raw_dat) %in% r$tables$meta_data[, r$meta$filename_col])) {
        output_table <- output_table |>
          DT::formatStyle(
            columns = colnames(r$tables$raw_dat) %in% r$tables$meta_data[, r$meta$filename_col],
            backgroundColor = "lightblue"
          )
      }

      return(output_table)
    })
  })
}

## To be copied in the UI
# mod_files_ui("files_1")

## To be copied in the server
# mod_files_server("files_1")
