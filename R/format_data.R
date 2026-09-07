#' Format Student Data for National Student Clearinghouse Submission
#'
#' Format student records into header, detail and trailer rows and write
#' tab-delimited TXT and CSV files. This incorporates the February 4, 2026
#' implementation. It prepares files; it does not upload them.
#' @param school_code Institution's six-digit code supplied as character text
#'   to preserve leading zeros.
#' @param branch_code Two-digit branch code supplied as character text.
#' @param school_name Full institution name.
#' @param file_creation_date Header creation date in YYYYMMDD format.
#' @param query_option Query code: CO, DA, PA or SE. CO requires a single
#'   search date in the input.
#' @param file_dir Existing directory containing input files and receiving output.
#' @param input_file_name A seven-column data frame, or the filename of a
#'   headerless XLSX, XLS, tab-delimited TXT or CSV file in file_dir. Columns,
#'   in order: first name, middle initial, last name, name suffix, date of birth,
#'   search date, and requester-return identifier. Dates use YYYYMMDD format.
#'   File input is read as text so leading zeros are preserved.
#' @param suffix Text appended to the output filename after date and query code.
#' @return A data frame with 12 columns containing header, retained student
#'   detail rows and trailer. TXT and CSV files are also written to file_dir.
#' @details Latin accents in the four name fields are transliterated to ASCII.
#'   Records with missing or empty first/last names are excluded. The trailer
#'   count includes the header and trailer. NSC() is a compatibility wrapper
#'   with the same arguments and behavior.
#'
#'   This update retains the 20260204 implementation's validation and output
#'   behavior. Punctuation is removed from all input fields, including IDs.
#'   File input is read as text. For an R data frame, the identifier column must
#'   be character; zeros lost before the call cannot be recovered. Whitespace-only names are not
#'   excluded, and an input with no retained records is not supported.
#'   The existing date check rejects dates fewer than 59 days before the current
#'   system date, despite its error message referring to 60 days. These legacy
#'   behaviors need further review against current submission requirements.
#'   Same-day calls with the same query code and suffix overwrite output files.
#' @export
#' @examples
#' students <- data.frame(
#'   first = paste0("Student", seq_len(11)), middle = "A", last = "Example",
#'   suffix = "", dob = "20000101", search = "20200101",
#'   id = sprintf("%05d", seq_len(11)))
#' destination <- tempfile("NSC-example-")
#' dir.create(destination)
#' formatted <- format_data(
#'   school_code = "001234", branch_code = "00", school_name = "Example College",
#'   file_creation_date = format(Sys.Date(), "%Y%m%d"), query_option = "SE",
#'   file_dir = destination, input_file_name = students, suffix = "example")
#' dim(formatted)
format_data <- function (school_code, branch_code, school_name, file_creation_date,
          query_option, file_dir, input_file_name, suffix)
{
  if (!is.character(school_code) || !is.character(branch_code)) {
    stop("school_code and branch_code must be character data so leading zeros are preserved.")
  }

  if (isTRUE(typeof(input_file_name) == "list")) {
    data_table_input <- input_file_name
    message("The function has read a R object.")
  } else {
    input_extension <- tolower(tools::file_ext(input_file_name))
    if (input_extension %in% c("xlsx", "xls")) {
      data_table_input <- readxl::read_excel(
        paste0(file_dir, "/", input_file_name),
        col_names = FALSE,
        col_types = "text"
      )
      message("The function has read an Excel file.")
    } else if (input_extension == "txt") {
      data_table_input <- utils::read.csv(
        paste0(file_dir, "/", input_file_name),
        header = FALSE,
        sep = "\t",
        colClasses = "character"
      )
      message("The function has read a 'txt' file.")
    } else if (input_extension == "csv") {
      data_table_input <- utils::read.csv(
        paste0(file_dir, "/", input_file_name),
        header = FALSE,
        colClasses = "character"
      )
      message("The function has read a 'csv' file.")
    } else {
      stop("input_file_name must be a data frame or an xlsx, xls, txt, or csv file.")
    }
  }

  if (is.numeric(data_table_input[[7]])) {
    stop("The requester-return identifier must be character data so leading zeros are preserved.")
  }
  data_table_input[] <- lapply(data_table_input, as.character)

  # Working with middle initial
  mi_tbl <- NULL
  for (i in 1:nrow(data_table_input)) {
    mi_tbl[i] <- nchar(data_table_input[i, 2]) > 1
  }
  
  if (length(which(mi_tbl == TRUE)) > 1) {
    message(paste(length(which(mi_tbl == TRUE)), "row(s) for the Middle Initial have more than one character, \nand they may be truncated by the National Student Clearinghouse system. \nYou may get a matched data set with a warning. In the input data set, please check rows:"))
    # print(which(mi_tbl == TRUE))
  }
  
  
  # Remove punctuation
  data_table_input <- as.data.frame(gsub("[[:punct:]]", "", 
                                         as.matrix(data_table_input)))
  
  
  # Convert Accent to plain ASCII 
  ## First name
  data_table_input[[1]] <- stringi::stri_trans_general(data_table_input[[1]], "Latin-ASCII")
  
  ## Middle Initial
  data_table_input[[2]] <- stringi::stri_trans_general(data_table_input[[2]], "Latin-ASCII")
  
  ## Last Name
  data_table_input[[3]] <- stringi::stri_trans_general(data_table_input[[3]], "Latin-ASCII")
  
  ## Suffix
  data_table_input[[4]] <- stringi::stri_trans_general(data_table_input[[4]], "Latin-ASCII")
                                         
  # Remove null first name and keep all columns 
  data_table_input <- data_table_input[!is.na(data_table_input[,1]), ]
  
  # Remove blank first name and keep all columns
  data_table_input <- data_table_input[(data_table_input[,1]) != "", ]
  
  # Remove null last name and keep all columns
  data_table_input <- data_table_input[!is.na(data_table_input[,3]), ]
  
  # Remove blank last name and keep all columns 
  data_table_input <- data_table_input[(data_table_input[,3]) != "", ]
  

  
# Working with query
  
  if (query_option == "CO") {
    if (length(unique(data_table_input[, 6])) > 1) {
      stop("search date for the query option 'CO' must be a single date per file. You might want to use the query option = 'SE' for multiple search dates.")
    }
  }
  extracted_dates <- as.Date(as.character(data_table_input[, 
                                                           6]), "%Y%m%d")
  for (i in 1:length(extracted_dates)) {
    if (Sys.Date() - extracted_dates[i] < 59) {
      latest_date <- gsub("-", "", Sys.Date() - 60)
      stop(paste("Your latest search date allowed is: ", 
                 latest_date, ".", " It must be at least 60 days to the current date."))
    }
  }
  header_row <- c("H1", school_code, branch_code, school_name, 
                  file_creation_date, query_option, "I", 
                  "", "", "", "", "")
  data_table_input2 <- cbind("D1", "", data_table_input[, 1:6], 
                             "", school_code, branch_code, data_table_input[, 7])
  colnames(data_table_input2) <- c("A", "B", "C", "D", "E", 
                                   "F", "G", "H", "I", "J", "K", "L")
  trailer_row <- c("T1", (nrow(data_table_input2) + 2), "", 
                   "", "", "", "", "", "", "", "", "")
  data_output <- rbind(header_row, data_table_input2, trailer_row)
  current_date <- Sys.Date()
  current_date <- gsub("[[:punct:]]", "", current_date)
  file_name_output_txt <- paste0(current_date, "_", query_option, 
                                 "_", suffix, ".txt")
  utils::write.table(data_output, file = paste0(file_dir, "/", file_name_output_txt), 
              sep = "\t", na = "", row.names = FALSE, col.names = FALSE, 
              quote = FALSE)
  file_name_output_csv <- paste0(current_date, "_", query_option, 
                                 "_", suffix, ".csv")
  utils::write.table(data_output, file = paste0(file_dir, "/", file_name_output_csv), 
              sep = ",", na = "", row.names = FALSE, col.names = FALSE, 
              quote = FALSE)
  if (file.exists(paste0(file_dir, "/", file_name_output_txt))) {
    message(paste("\"", file_name_output_txt, "\" ", "has been created in ", 
                  file_dir, ".", collapse = "", sep = ""))
  }  else {
    message(paste("Failed to create ", "\"", file_name_output_txt, 
                  "\".", sep = ""))
  }
  if (file.exists(paste0(file_dir, "/", file_name_output_csv))) {
    message(paste("\"", file_name_output_csv, "\" ", "has been created in ", 
                  file_dir, ".", collapse = "", sep = ""))
  }  else {
    message(paste("Failed to create ", "\"", file_name_output_csv, 
                  "\".", sep = ""))
  }
  return(data_output)
}
