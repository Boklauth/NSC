#' A Function to Create a National Student Clearinghouse Data Layout
#'
#' This package contains the NSC function {NSC()},
#' which allows you to format your data according to the National Student Clearinghouse format to obtain
#' the information you need. So, you can don't need to stay busy formatting the data.
#'
#'This package will remove white space and special characters in the fields. It will produce output files with '.txt' and '.csv'. 
#'
#' @param school_code A six-digit code identifying your institution. This is a number but will be formatted as text. The number must be in quotation marks. See the example below.
#' @param branch_code A two-digit code identifying your institution's branch if applicable. This is a number but will be formatted as text. The number must be in quotation marks. See the example below.
#' @param school_name The official full name of your institution.
#' @param file_creation_date This is the date you created the file. It must follow the 'YYYYMMDD' format.
#' @param query_option This is a two letter code specifying the type of information you are requesting from NSC. The available options are 'CO' (Longitudinal Cohort), 'DA' (Declined Admissions), 'PA' (Pror Attendance), and 'SE' (Subsequent Enrollment)
#' @note CO: Requires one search date only per file; otherwise, NSC will reject your file. If you have multiple cohorts, it is recommended that you break cohorts into multiple files: one file with one cohort and one search date. Primarily used for the purposes of reporting to Student Achievement Measure (SAM) or Voluntary System of Accountability (VSA). This query type uses your search date and looks forward, hence, you may want to select a period in the past (less than 16 years) to run a holistic student enrollment record for reporting.
#' @note DA: Searches enrollment of former applicants who chose not to enroll at your institution or you elected to not accept the student for admissions. This query type uses your search date, presumably the first semester the student would have enrolled at your institution, and looks forward.
#' @note PA: Historical enrollment of pending applicants to your institution. This is the query type used to find prior educational records from your prospective students for the purposes of validation or verification, hence this search uses your search date and goes backwards.
#' @note SE: Allows multiple search dates per file. A search data must be at least sixty days to the file submission date. Concurrent enrollment of current students and subsequent enrollment of prior students. This query type allows you to understand if your current or prior students are dual enrolled or continuing to enroll in educational institutions after being enrolled at your institution, hence this search uses your search date and goes forward.

#' @param file_dir This is the location of the data set input. Whatever type of input object you have, this director is needed to store the output files.
#' @param input_file_name This is a data set input that does NOT contain a header. The file has one of the following extensions: '.xslx', 'xls', or a tab delimited file with an extension '.txt'. Or you can supply an R object containing a data set. It should contain in that order: first name, middle initial, last name, suffix, date of birth in 'YYYYMMDD' format, search date in 'YYYYMMDD' format, and a column for student unique identifiers.  You do NOT need to supply NSC with social security numbers of students.The column for the student unique identifiers will not be used by NSC, but it is there for student matching purposes after the data are granted by NSC. Note that you don't need to include a blank column, school code, and branch code in your input data set because this function will include them and produce a file in the right layout that is ready to be uploaded on to NSC portal. For more instructional information, see https://studentclearinghouse.info/onestop/wp-content/uploads/STCU_User_Manual.pdf.
#' @return NSC will return a data frame output and at the same time output '.txt' and '.xlsx' files according to the
#'         the file directory you have provided. The '.txt' file is the file that you will use to upload to the
#'         StudentTracker (https://ftps.nslc.org/) to obtain students' information.
#'
#' @export
#' @examples
#' # Read an excel file
#' NSC(school_code = '002330',
#' branch_code =  '00',
#' school_name = 'Western Michigan University',
#' file_creation_date = 20210708,
#' query_option = 'CO',
#' file_dir = 'C:/Users/Dell/Documents/National Student Clearinghouse/tests',
#' input_file_name = 'test_data.xlsx')
#' 
#' # Read an a tab delimited 'txt' file
#' NSC(school_code = '002330',
#' branch_code =  '00',
#' school_name = 'Western Michigan University',
#' file_creation_date = 20210708,
#' query_option = 'CO',
#' file_dir = 'C:/Users/Dell/Documents/National Student Clearinghouse/tests',
#' input_file_name = 'test_data.txt')
#' 
#' # Read a data set as an R object using Oracle (PL/SQL) connection
#' library(ROracle)
#' library(keyring)
#'
#' # Connect to your database using Oracle driver 
#' drv <- dbDriver("Oracle")
#' con <- dbConnect(drv, 
#'                 username = key_get("My_ID", keyring="Oracle"), 
#'                 password = key_get("Oracle_pw", keyring="Oracle"), 
#'                 dbname = "DBS")
#'
#' # Get the data containing 100 rows from the database via Oracle
#'
#' y <- dbGetQuery(con, 
#'                 "
#'                SELECT  DISTINCT    first_name, 
#'                                    middle_initial, 
#'                                    last_name, 
#'                                    name_suffix, 
#'                                    TO_CHAR(birth_date, 'YYYYMMDD') as birth_date, 
#'                                    '20210115' as search_date,
#'                                    person_id 
#'                FROM schema1.person_table
#'                WHERE rownum <=100 
#'                ")
#'
#'
#' # Disconnect the database
#' dbDisconnect(con)
#'
#' # Read a data set that is stored as an R object
#' NSC(school_code = '002330',
#'         branch_code =  '00',
#'         school_name = 'Western Michigan University',
#'         file_creation_date = 20210708,
#'         query_option = 'CO',
#'         file_dir = 'C:/Users/Dell/National Student Clearinghouse/tests',
#'         input_file_name = y)
#'
#' @references
#' \insertRef{NationalStudentClearinghouse2017}{NSC}
#' 
#' @seealso [keyring()], [Oracle()], [dbDriver()], [dbConnect()], [dbGetQuery()], [dbDisconnect()].

NSC <- function (school_code,
                 branch_code,
                 school_name,
                 file_creation_date,
                 query_option,
                 file_dir,
                 input_file_name){
  
  # Require libraries
  # library(writexl)
  # library(readxl)
  
  
  
  ## Testing file extensions
  if (isTRUE(typeof(input_file_name)=="list")){
    data_table_input <- input_file_name
    message("The function has read a R object.")
  } else {
    if(unlist(strsplit(input_file_name, '\\.'))[2]=='xlsx'){
      data_table_input <- readxl::read_excel(paste0(file_dir, '/',input_file_name), 
                                             col_names = FALSE) 
      message("The function has read an 'xls' file.")
    } else if (unlist(strsplit(input_file_name, '\\.'))[2]=='xls'){
      data_table_input <- readxl::read_excel(paste0(file_dir, '/',input_file_name), 
                                             col_names = FALSE) 
      message("The function has read an 'xls' file.")
    }  else if (unlist(strsplit(input_file_name, '\\.'))[2]=='txt'){
      data_table_input <- read.csv(paste0(file_dir, '/',input_file_name), 
                                   header = FALSE, sep='\t')
      message("The function has read a 'txt' file.")
    } else if (unlist(strsplit(input_file_name, '\\.'))[2]=='csv'){
      data_table_input <- read.csv(paste0(file_dir, '/',input_file_name), 
                                   header = FALSE)
      message("The function has read a 'csv' file.")
  }}
  
  
  # Checking middle initials ####
  mi_tbl <- NULL
  for (i in 1:nrow(data_table_input)){
    mi_tbl[i] <- nchar(data_table_input[i,2]) > 1
  }
  
  # message about middle initials ####
  if(length(which(mi_tbl==TRUE)) > 0){
    message(paste(length(which(mi_tbl==TRUE)), "row(s) for the Middle Initial have more than one character, 
and they may be truncated by the National Student Clearinghouse. 
You may get a matched data set with a warning. In the input data set, please check rows:"))
    print(which(mi_tbl==TRUE))
  }
  
  # Removing special characters from a data frame ####
  data_table_input <- as.data.frame(gsub("[[:punct:]]", "", as.matrix(data_table_input))) 
  
  # Check for a single unique dates for CO
  if(length(unique(data_table_input[,6]))>1){
    stop("search date for the query option 'CO' must be a single date per file. You might want to use the query option = 'SE' for multiple search dates.")
  } else if (data_table_input[,6]) 
    
    # Check n of days before submission
    extracted_dates <- as.Date(as.character(data_table_input[,6]), "%Y%m%d")
  if (Sys.Date() - extracted_dates< 59){
    earliest_date <- gsub("-", "", Sys.Date()-60)
    stop(paste("Your earliest search data must be at least 60 days to the current date: ", 
               earliest_date))
  }
  
  # create a header row ####
  header_row <- c('H1',
                  school_code,
                  branch_code,
                  school_name,
                  file_creation_date,
                  query_option,
                  'I', '', '', '', '', '', '')
  
  
  # student detail rows  ####
  
  data_table_input2 <- cbind('D1', 
                             '', 
                             data_table_input[,1:6], '', 
                             school_code, branch_code, 
                             data_table_input[,7])
  colnames(data_table_input2) <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L')
  
  # create the trailer row ####
  trailer_row <- c('T1', 
                   (nrow(data_table_input2)+2), 
                   '', '', '', '', '', '', 
                   '', '', '', '', '', '', 
                   '')
  
  
  data_output <- rbind(header_row, data_table_input2, trailer_row)
  
  
  # eliminate an Excel output
  # file_name_output_xlsx <- 'data_output.xlsx'
  # 
  # writexl::write_xlsx(data_output,
  #                     path = paste0(file_dir, '/',file_name_output_xlsx),
  #                     col_names = FALSE)
  
  
  current_date <- Sys.Date()
  # remove special character in date, leaving only numbers
  current_date <- gsub("[[:punct:]]", "", current_date)
  

  
  
  # output file as txt
  file_name_output_txt <- paste0(current_date, '_',
                                 query_option, 
                                 '_data_output.txt')
  
  write.table(data_output,
              file = paste0(file_dir, '/',file_name_output_txt),
              sep = '\t', # tab-separated
              na = '',
              row.names = FALSE,
              col.names = FALSE,
              quote = FALSE)
  
  # output file as csv
  file_name_output_csv <- paste0(current_date, '_',
                                 query_option, 
                                 '_data_output.csv')

  write.table(data_output, 
              file = paste0(file_dir, '/',file_name_output_csv),
              sep = ",",# comma-separated
              na = '',
              row.names = FALSE,
              col.names = FALSE,
              quote = FALSE)
  
  # give message for the conditions
  # for text file
  if (file.exists(paste0(file_dir, "/", file_name_output_txt))) {
    message(paste('"', file_name_output_txt,'" ', 'has been created in ', file_dir, '.',
                  collapse = '', sep=''))
  } else {
    message(paste('Failed to create ', '"', file_name_output_txt,'".', sep=''))
  }
  
  # for csv file
  if (file.exists(paste0(file_dir, "/", file_name_output_csv))) {
    message(paste('"', file_name_output_csv,'" ', 'has been created in ', file_dir, '.',
                  collapse = '', sep=''))
  } else {
    message(paste('Failed to create ', '"', file_name_output_csv,'".', sep=''))
  }
  
  
  # Return
  return(data_output)
}
