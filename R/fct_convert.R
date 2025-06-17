#' convert
#'
#'#' @title Extract experimental data from MSDIAL
#'
#' @description Extract the experimental data and feature data
#' and convert them to a data.frame in a format suited for iSODA.
#'
#' @param data data.frame.
#' @param selected_cols character() containing the column names.
#' @param clean character() what to clean.
#' @param duplicates character(1), what to do with duplicates.
#' @param option character(1) which convert step to do.
#' @param omics character(1) which omics are we using.
#'
#' @returns list with the experimental data and feature data.
#'
#' @noRd
#'
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @author Rico Derks
#'
extract_data <- function(data = NULL,
                         selected_cols = NULL,
                         clean = NULL,
                         duplicates = c("sum", "average", "rename"),
                         option = c("names", "ids"),
                         omics = c("lip", "met")) {
  omics <- match.arg(arg = omics,
                     choices = c("lip", "met"))

  res <- switch(
    omics,
    "met" = extract_data.met(data = data,
                             selected_cols = selected_cols,
                             clean = clean,
                             duplicates = duplicates,
                             option = option),
    "lip" = extract_data.lip(data = data,
                             selected_cols = selected_cols,
                             clean = clean,
                             duplicates = duplicates)
  )

  return(res)
}


#' @title Extract metabolomics data from MSDIAL
#'
#' @description Extract the metabolomics experimental data and feature data
#' and convert them to a data.frame in a format suited for iSODA.
#'
#' @param data data.frame.
#' @param selected_cols character() containing the column names.
#' @param clean character() what to clean.
#' @param duplicates character(1), what to do with duplicates.
#' @param option character(1) which convert step to do.
#'
#' @returns list with the experimental data and feature data.
#'
#' @noRd
#'
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @author Rico Derks
#'
extract_data.met <- function(data = NULL,
                             selected_cols = NULL,
                             clean = NULL,
                             duplicates = c("sum", "average", "rename"),
                             option = c("names", "ids")) {

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

  dupl_data <- process_duplicates(data = clean_data[, c(names_from, selected_cols)],
                                  duplicates = duplicates)

  data_long <- dupl_data |>
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

  feature_data <- clean_data[clean_data[, names_from] %in% colnames(data_wide)[-1], c("Alignment ID", "Metabolite name", "Ontology", "Adduct type")]
  if(duplicates == "rename") {
    feature_data$`Alignment ID` <- make.unique(names = feature_data$`Alignment ID`,
                                               sep = "_")
    feature_data$`Metabolite name` <- make.unique(names = feature_data$`Metabolite name`,
                                                  sep = "_")
  } else {
    feature_data <- aggregate(x = feature_data,
                              by = list(feature_data[, names_from]),
                              FUN = function(x) {
                                paste(x, collapse = ",")
                              })

    feature_data <- feature_data[, !(colnames(feature_data) %in% names_from)]
    colnames(feature_data)[1] <- names_from
  }

  return(list(exp_data = data_wide,
              feature_data = feature_data))
}


#' @title Extract lipidomics data from MSDIAL
#'
#' @description Extract the lipidomics experimental data and feature data
#' and convert them to a data.frame in a format suited for iSODA.
#'
#' @param data data.frame.
#' @param selected_cols character() containing the column names.
#' @param clean character() what to clean.
#' @param duplicates character(1), what to do with duplicates.
#'
#' @returns list with the experimental data and feature data.
#'
#' @noRd
#'
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @author Rico Derks
#'
extract_data.lip <- function(data = NULL,
                             selected_cols = NULL,
                             clean = NULL,
                             duplicates = c("sum", "average", "rename")) {
  if(!is.null(clean)) {
    clean_data <- clean_features.lip(data = data,
                                     clean = clean)
  } else {
    clean_data <- data
  }

  dupl_data <- process_duplicates(data = clean_data[, c("Metabolite name", selected_cols)],
                                  duplicates = duplicates)

  data_long <- dupl_data |>
    tidyr::pivot_longer(
      cols = selected_cols,
      names_to = "sampleId",
      values_to = "peakArea"
    )

  data_wide <- data_long |>
    tidyr::pivot_wider(
      id_cols = "sampleId",
      names_from = "Metabolite name",
      values_from = "peakArea"
    )

  feature_data <- clean_data[clean_data[, "Metabolite name"] %in% colnames(data_wide)[-1], c("Alignment ID", "Metabolite name", "Ontology", "Adduct type")]
  if(duplicates == "rename") {
    feature_data$`Metabolite name` <- make.unique(names = feature_data$`Metabolite name`,
                                                  sep = "_")
  } else {
    feature_data <- aggregate(x = feature_data,
                              by = list(feature_data[, "Metabolite name"]),
                              FUN = function(x) {
                                paste(x, collapse = ",")
                              })

    feature_data <- feature_data[, !(colnames(feature_data) %in% "Metabolite name")]
    colnames(feature_data)[1] <- "Metabolite name"
  }

  return(list(exp_data = data_wide,
              feature_data = feature_data))
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


#' @title Clean up the lipidomics data.frame
#'
#' @description
#' Clean up the lipidomics data.frame, remove the 'Unknowns'.
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
clean_features.lip <- function(data = NULL,
                               clean = c("unknown", "low score", "no MS2")) {
  clean <- match.arg(arg = clean,
                     choices = c("unknown", "low score", "no MS2"),
                     several.ok = TRUE)

  clean_data <- data[!grepl(x = data$`Metabolite name`,
                            pattern = paste0("^(", paste(clean, collapse = "|"), ").*$"),
                            ignore.case = TRUE), ]

  clean_data <- clean_data[!grepl(x = clean_data$`Metabolite name`,
                                  pattern = paste0("^(RIKEN.*|.*also known.*)$"),
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
