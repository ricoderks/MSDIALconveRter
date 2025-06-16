#' convert
#'
#' @description Convert the data.frame to a format suited for iSODA
#'
#' @param data data.frame.
#' @param selected_cols character() containing the column names.
#' @param clean character() what to clean.
#' @param duplicates character(1), what to do with duplicates.
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
                         duplicates = c("sum", "average", "rename"),
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

  names_from <- switch(
    option,
    "ids" = "Alignment ID",
    "names" = "Metabolite name"
  )

  clean_data <- process_duplicates(data = clean_data[, c(names_from, selected_cols)],
                                   duplicates = duplicates)

  data_long <- clean_data |>
    tidyr::pivot_longer(
      cols = selected_cols,
      names_to = "sampleId",
      values_to = "peakArea"
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


#' @title Process duplicate columns
#'
#' @description
#' Process duplicate columns in different ways.
#'
#' @param data data.frame.
#' @param duplicates character(1), what to do with duplicates (i.e. sum, average or rename the columns).
#'
#' @returns data.frame with no duplicate columns anymore.
#'
#' @noRd
#'
#' @author Rico Derks
#'
process_duplicates <- function(data = NULL,
                               duplicates = c("sum", "average", "rename")) {
  duplicates <- match.arg(arg = duplicates,
                          choices = c("sum", "average", "rename"))

  res <- switch(
    duplicates,
    "sum" = sums.process_duplicates(data = data),
    "average" = average.process_duplicates(data = data),
    "rename" = rename.process_duplicates(data = data)
  )

  return(res)
}


#' @title Sum duplicates
#'
#' @description
#' Sum duplicates.
#'
#' @param data data.frame.
#'
#' @returns data.frame.
#'
#' @importFrom stats aggregate
#'
#' @noRd
#'
#' @author Rico Derks
#'
sums.process_duplicates <- function(data = NULL) {
  res <- stats::aggregate(x = data[, -1],
                          by = list(data[, 1]),
                          FUN = function(x) {
                            sum(x, na.rm = TRUE)
                          })

  colnames(res)[1] <- colnames(data)[1]

  return(res)
}


#' @title Average duplicates
#'
#' @description
#' Average duplicates.
#'
#' @param data data.frame.
#'
#' @returns data.frame.
#'
#' @importFrom stats aggregate
#'
#' @noRd
#'
#' @author Rico Derks
#'
average.process_duplicates <- function(data = NULL) {
  res <- stats::aggregate(x = data[, -1],
                          by = list(data[, 1]),
                          FUN = function(x) {
                            mean(x, na.rm = TRUE)
                          })

  colnames(res)[1] <- colnames(data)[1]

  return(res)
}


#' @title Rename duplicates
#'
#' @description
#' Rename duplicates.
#'
#' @param data data.frame.
#'
#' @returns data.frame.
#'
#' @noRd
#'
#' @author Rico Derks
#'
rename.process_duplicates <- function(data = NULL) {
  res <- data

  res[, 1] <- make.unique(names = res[, 1],
                          sep = "_")

  return(res)
}
