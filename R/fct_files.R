#' @title Read a file
#'
#' @description
#' Read a CSV, TSV or Excel file
#'
#' @param file_path character(1) path of the file.
#'
#' @return data.frame with the meta data.
#'
#' @noRd
#'
#' @importFrom stringr str_sub
#' @importFrom readxl read_xlsx
#' @importFrom utils read.table
#'
#' @author Damien Olivier
#' @author Rico Derks
#'
read_data <- function(file_path = NULL) {
  if(is.null(file_path)) {
    stop("No file path specified!")
  } else {
    if(!file.exists(file_path)) {
      stop("File path does not exist!")
    }
  }

  if (stringr::str_sub(file_path, -5, -1) == ".xlsx") {
    data_table <- as.data.frame(readxl::read_xlsx(path = file_path))
  } else {
    sep <- find_delim(file_path = file_path)
    data_table <- utils::read.table(file_path,
                                    header = TRUE,
                                    sep = sep,
                                    check.names = FALSE)
  }

  # check for dublicate column names
  original_count <- ncol(data_table)
  if (original_count > 1) {
    data_table <- data_table[, !duplicated(colnames(data_table))]
    final_count <- ncol(data_table)
    if(original_count != final_count) {
      print(paste0("Removed ", original_count - final_count, " duplicated columns"))
    }
  }

  return(data_table)
}


#' @title Find the delimiter of a file
#'
#' @description
#' Find the delimiter of a file by reading the first 10 lines.
#'
#' @param file_path character(1) path of the file.
#'
#' @return character(1), the delimiter found.
#'
#' @noRd
#'
#' @author Damien Olivier
#' @author Rico Derks
#'
find_delim = function(file_path = NULL) {
  if(is.null(file_path)) {
    stop("No file path specified!")
  } else {
    if(!file.exists(file_path)) {
      stop("File path does not exist!")
    }
  }

  probe <- paste(readLines(con = file_path,
                           n = 10),
                 collapse = "")
  sep <- c(
    "\t" = lengths(regmatches(x = probe,
                              m = gregexpr(pattern = "\t",
                                           text = probe))),
    "," = lengths(regmatches(x = probe,
                             m = gregexpr(pattern = ",",
                                          text = probe))),
    ";" = lengths(regmatches(x = probe,
                             m = gregexpr(pattern = ";",
                                          text = probe)))
  )

  return(names(which.max(sep)))
}


#' @title Read MS-DIAl files
#'
#' @description Read the result files from MS-DIAL.
#'
#' @param filename The filename of the MS-DIAL file.
#'
#' @return Returns a tibble
#'
#' @importFrom readr read_delim cols col_double col_character col_integer
#'
#' @noRd
#'
#' @author Rico Derks
#'
read_msdial <- function(filename = NULL) {
  # determine which version is loaded, read only the column names
  column_names <- colnames(readr::read_delim(file = filename,
                                             delim ="\t",
                                             na = c("", "NA", "null"),
                                             n_max = 1,
                                             show_col_types = FALSE,
                                             skip = 4))

  if("Simple dot product" %in% column_names) {
    # version > 5.1
    res <- readr::read_delim(file = filename,
                             delim ="\t",
                             na = c("", "NA", "null"),
                             show_col_types = FALSE,
                             col_types = readr::cols(`Alignment ID` = readr::col_integer(),
                                                     `Average Rt(min)` = readr::col_double(),
                                                     `Average Mz` = readr::col_double(),
                                                     `Metabolite name` = readr::col_character(),
                                                     `Adduct type` = readr::col_character(),
                                                     `Post curation result` = readr::col_character(),
                                                     `Fill %` = readr::col_double(),
                                                     `MS/MS assigned` = readr::col_character(),
                                                     `Reference RT` = readr::col_character(),
                                                     `Reference m/z` = readr::col_character(),
                                                     Formula = readr::col_character(),
                                                     Ontology = readr::col_character(),
                                                     INCHIKEY = readr::col_character(),
                                                     SMILES = readr::col_character(),
                                                     `Annotation tag (VS1.0)` = readr::col_character(),
                                                     `RT matched` = readr::col_character(),
                                                     `m/z matched` = readr::col_character(),
                                                     `MS/MS matched` = readr::col_character(),
                                                     Comment = readr::col_character(),
                                                     `Manually modified for quantification` = readr::col_character(),
                                                     `Manually modified for annotation` = readr::col_character(),
                                                     `Isotope tracking parent ID` = readr::col_character(),
                                                     `Isotope tracking weight number` = readr::col_character(),
                                                     `Total score` = readr::col_double(),
                                                     `RT similarity` = readr::col_double(),
                                                     `Simple dot product` = readr::col_double(),
                                                     `Reverse dot product` = readr::col_double(),
                                                     `Weighted dot product` = readr::col_double(),
                                                     `Matched peaks percentage` = readr::col_double(),
                                                     `S/N average` = readr::col_double(),
                                                     `Spectrum reference file name` = readr::col_character(),
                                                     `MS1 isotopic spectrum` = readr::col_character(),
                                                     `MS/MS spectrum` = readr::col_character()),
                             skip = 4)
  } else {
    # version 4.x
    res <- readr::read_delim(file = filename,
                             delim ="\t",
                             na = c("", "NA", "null"),
                             show_col_types = FALSE,
                             col_types = readr::cols(`Alignment ID` = readr::col_integer(),
                                                     `Average Rt(min)` = readr::col_double(),
                                                     `Average Mz` = readr::col_double(),
                                                     `Metabolite name` = readr::col_character(),
                                                     `Adduct type` = readr::col_character(),
                                                     `Post curation result` = readr::col_character(),
                                                     `Fill %` = readr::col_double(),
                                                     `MS/MS assigned` = readr::col_character(),
                                                     `Reference RT` = readr::col_character(),
                                                     `Reference m/z` = readr::col_character(),
                                                     Formula = readr::col_character(),
                                                     Ontology = readr::col_character(),
                                                     INCHIKEY = readr::col_character(),
                                                     SMILES = readr::col_character(),
                                                     `Annotation tag (VS1.0)` = readr::col_character(),
                                                     `RT matched` = readr::col_character(),
                                                     `m/z matched` = readr::col_character(),
                                                     `MS/MS matched` = readr::col_character(),
                                                     Comment = readr::col_character(),
                                                     `Manually modified for quantification` = readr::col_character(),
                                                     `Manually modified for annotation` = readr::col_character(),
                                                     `Isotope tracking parent ID` = readr::col_character(),
                                                     `Isotope tracking weight number` = readr::col_character(),
                                                     `Total score` = readr::col_double(),
                                                     `RT similarity` = readr::col_double(),
                                                     `Dot product` = readr::col_double(),
                                                     `Reverse dot product` = readr::col_double(),
                                                     `Fragment presence %` = readr::col_double(),
                                                     `S/N average` = readr::col_double(),
                                                     `Spectrum reference file name` = readr::col_character(),
                                                     `MS1 isotopic spectrum` = readr::col_character(),
                                                     `MS/MS spectrum` = readr::col_character()),
                             skip = 4)
  }

  res <- as.data.frame(res,
                       check.names = FALSE)


  return(res)
}
