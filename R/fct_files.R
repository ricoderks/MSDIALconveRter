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
    if (is.null(sep)) {
      sep <- find_delim(file_path = file_path)
      data_table <- utils::read.table(file_path,
                                      header = TRUE,
                                      sep = sep,
                                      check.names = FALSE)
    } else {
      data_table <- utils::read.table(file_path,
                                      header = TRUE,
                                      sep = sep,
                                      check.names = FALSE)
    }
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
