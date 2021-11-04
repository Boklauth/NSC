#' A Function to Prepare Data Obtained from NSC
#'
#' This function will prepare data before analysis. It will create time indicators
#', which are easy for an analyst to select data (i.e., first year fall semester,
#' second year fall semester). This is for the query option = "SE". 
#' 
#' @param file_dir A file directory, in which the data file is located.
#' is located. 
#' @param file_name The name of the data file obtained from the NSC. It must have
#' the original extension ".csv". 
#' @param replace_dates These are two date values, all of which are in the "YYYYMMDD" format. 
#' These dates are the search dates. 
#' The first date is in your data file and will be replaced by the second date. 
#' This is only relevant when you had changed the search date in your data for 
#' by-passing the NSC system. Now, you want to change it back. If this field is not 
#' provided, no dates will be changed. 
#' @return It will return a data set with the periods (".") removed 
#' from the column names. See notes. 
#' @note cohort_asis A variable in the data set. It represents a cohort. 
#' It uses the first four digits of the SearchDate. 
#' @note cohort_ret A variable in the data set. It represents a cohort for 
#' retention data. Thus, it uses the first four digits in the SearchDate minus 1.
#' @note d_indicator1, m_indicator1, and y_indicator1 are the values for day, month, 
#' and year, respectively, for the variable EnrollmentBegin. 
#' @note d_indicator2, m_indicator2, and y_indicator2 are the values for day, month, 
#' and year, respectively, for the variable EnrollmentEnd. 
#' @note SearchDate_y and SearchDate_m are the month and year of the 
#' variable SearchDate. 
#' @note EnrollmentBegin_y and EnrollmentBegin_m are the month and year of the 
#' variable EnrollmentBegin. 
#' @export
#' @examples
#' # File directory
#' myfile_dir <- 'C:/Users/shh6304/Documents/My Documents/WORK/National Student Clearinghouse/FTIAC 2Y retention/NSC data obtained'
#' # File_name 
#'	myfile_name <- "00233001_468503_DETLRPT_SE_10272021155850_20211027_se_ftiacretention_output.csv"
#'	myreplace_dates <- c(20210729, 20210915)
#'	mynsc_ds <- prepare_data(file_dir = myfile_dir, 
#'	             file_name = myfile_name, 
#'	             replace_dates = myreplace_dates)



prepare_data <- function(file_dir, file_name, replace_dates=NULL){
  my_dir <- getwd()
  
  ds <- read.csv(paste0(file_dir, "/", file_name), 
                 header=TRUE, check.names = TRUE)
  # Remove "." in names
  names(ds) <- gsub("\\.", "", names(ds))
  
  # change cohort 2021's search date to 20210915 from 20210715
  if(!is.null(replace_dates)){
    ds[ds==replace_dates[1]] <- replace_dates[2]
  }
  
  ## Cohorts
  
  # trim search dates to make cohort years
  cohort_asis <- as.numeric(substring(ds$SearchDate, 1, 4))
  cohort_ret <- as.numeric(substring(ds$SearchDate, 1, 4))-1
  SearchDate_y <- as.numeric(substring(ds$SearchDate, 1, 4))
  SearchDate_m <- as.numeric(substring(ds$SearchDate, 5, 6))
  # calculate time indicator from the search date
  d_indicator1 <-as.numeric(gsub("days", "", (lubridate::ymd(ds$EnrollmentBegin)-ymd(ds$SearchDate))))
  m_indicator1 <- as.numeric(gsub("days", "", (lubridate::ymd(ds$EnrollmentBegin)-ymd(ds$SearchDate))/30))
  y_indicator1 <- as.numeric(gsub("days", "", (lubridate::ymd(ds$EnrollmentBegin)-ymd(ds$SearchDate))/(30*12)))
  EnrollmentBegin_y <- as.numeric(substring(lubridate::ymd(ds$EnrollmentBegin),1,4))
  EnrollmentBegin_m <- as.numeric(substring(lubridate::ymd(ds$EnrollmentBegin),6,7))
  d_indicator2 <-as.numeric(gsub("days", "", (lubridate::ymd(ds$EnrollmentEnd)-ymd(ds$SearchDate))))
  m_indicator2 <- as.numeric(gsub("days", "", (lubridate::ymd(ds$EnrollmentEnd)-ymd(ds$SearchDate))/30))
  y_indicator2 <- as.numeric(gsub("days", "", (lubridate::ymd(ds$EnrollmentEnd)-ymd(ds$SearchDate))/(30*12)))
  EnrollmentEnd_y <- as.numeric(substring(lubridate::ymd(ds$EnrollmentEnd),1,4))
  EnrollmentEnd_m <- as.numeric(substring(lubridate::ymd(ds$EnrollmentEnd),6,7))
  # integrate cohorts in the original dataset
  set3 <- cbind(cohort_asis, 
                cohort_ret,
                ds, 
                SearchDate_y, 
                SearchDate_m,
                EnrollmentBegin_y,
                EnrollmentBegin_m, 
                d_indicator1, 
                m_indicator1, 
                y_indicator1,
                EnrollmentEnd_y,
                EnrollmentEnd_m, 
                d_indicator2, 
                m_indicator2, 
                y_indicator2)
  
}