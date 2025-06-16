#' convert
#'
#' @description Convert the data.frame to a format suited for iSODA
#'
#' @param data data.frame.
#' @param selected_cols character() containing the column names.
#' @param clean character() what to clean.
#' @param option integer(1) which convert step to do.
#' @param omics character(1) which omics are we using.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @author Rico Derks
#'
convert_data <- function(data = NULL,
                         selected_cols = NULL,
                         clean = NULL,
                         option = c("names", "ids"),
                         omics = c("lip", "met")) {
  omics <- match.arg(arg = omics,
                     choices = c("lip", "met"))
  option <- match.arg(arg = option,
                      choices = c("names", "ids"))

  if(!is.null(clean)) {
    clean_data <- clean_features(data = data,
                                 clean = clean)
  } else {
    clean_data <- data
  }

  data_long <- clean_data |>
    tidyr::pivot_longer(
      cols = selected_cols,
      names_to = "sampleId",
      values_to = "peakArea"
    )

  names_from <- switch(
    option,
    "ids" = "Alignment ID",
    "names" = "Metabolite name"
  )

  data_wide <- data_long |>
    tidyr::pivot_wider(
      id_cols = "sampleId",
      names_from = names_from,
      values_from = "peakArea"
    )

  return(data_wide)
}


#' @title Clean up the data.frame
#'
#' @description
#' Clean up the data.frame, remove the 'Unknowns'.
#'
#' @param data data.frame in long format.
#' @param clean character() what to clean.
#'
#' @return 'cleaned' data.frame.
#'
#' @noRd
#'
#' @author Rico Derks
#'
clean_features <- function(data = NULL,
                           clean = c("unknown", "low score", "no MS2")) {
  clean <- match.arg(arg = clean,
                     choices = c("unknown", "low score", "no MS2"),
                     several.ok = TRUE)

  clean_data <- data[!grepl(x = data$`Metabolite name`,
                            pattern = paste0("^(", paste(clean, collapse = "|"), ").*$"),
                            ignore.case = TRUE), ]

  return(clean_data)
}




