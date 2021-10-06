#' A Function to Create Longitudinal NSC Data
#' 
#' This function create a longitudinal table of data obtained from the National Student Clearinghouse (NSC). Some variables include
#'  enrollment at less-than-two-year, two-year, and four-year colleges, 
#'  graduation from those colleges, and highest degree obtained. This table is 
#'  for crosstabulation of data. This is for the query option = "SE". 
#' 
#' @param file_dir A file directory to the data obtained from NSC.
#' @param file_name The name of the data file. 
#' @param college_name The name of your institution in all CAPS. This is to determine 
#' @param x A data set that follows a specific way of reading an NSC file 
#' (see the example 2 below). When a data set is provided, the function does not 
#' read a data file, file_dir and file_name are not needed, but your_college 
#' is required.
#' whether the students have left your institution and did not enroll else where. 
#' In this case, the students are assumed to drop out of the higher education 
#' system (HES).
#' @return It will return a table with higher education system dropout status,
#' enrollment, graduation, and highest degree obtained. This table contains only 
#' one row per student.  
#'
#' @export
#' @examples
#' # example 1
#' setwd('C:/Dell/National Student Clearinghouse/NSC data obtained')
#' getwd()
#' my_dir <- getwd()
#' myfile_name <-  "00233001_463454_DETLRPT_SE_09282021192135_20210927_se_data_output.csv"
#' college_name <- "WESTERN MICHIGAN UNIVERSITY"
#'
#' myoutput <- for_NSC_crosstab(file_dir = my_dir, 
#'                         file_name = myfile_name, 
#'                         college_name = "WESTERN MICHIGAN UNIVERSITY", 
#'                         x = NULL)
#' View(myoutput)
#' 
#' # example 2
#' #' setwd("C/Users/Dell/Documents")
#' file_name <- "00233001_463454_DETLRPT_SE_09282021192135_20210927_se_data_output.csv"
#' data <- read.csv(paste0(getwd(), "/", file_name), 
#'                  header=TRUE, check.names = TRUE)
#' # Remove "." in names
#' names(data) <- gsub("\\.", "", names(data))
#' #' myoutput2 <- for_NSC_crosstab(file_dir = my_dir, 
#'                         file_name = myfile_name, 
#'                         college_name = "WESTERN MICHIGAN UNIVERSITY", 
#'                         x = data)
#' View(myoutput2)

for_NSC_crosstab <- function(file_dir,
                         file_name,
                         college_name, 
                         x = NULL){
  require("dplyr")
  require("NSC")
 
  
  
  if (is.null(x)){
    # Pull data ####
    data <- read.csv(paste0(getwd(), "/", file_name), 
                     header=TRUE, check.names = TRUE)
    # Remove "." in names
    names(data) <- gsub("\\.", "", names(data))
  } else {
    data <- x
    if (is.null(college_name)){
      stop("Please provide a value for college_name.")
    }
  }
  
  # Step 1: get one enrollment ####
  ## In higher ed sys (IN_HES) or not 
  # set up in higher ed system or not after WMU
  # Step1: working with RecordFoundYN not from your institution####
  enr1 <- data %>% 
    filter(CollegeName != college_name) %>% 
    select(RequesterReturnField, RecordFoundYN) %>% 
    distinct() %>% 
    mutate(IN_HES = case_when(RecordFoundYN == "Y" ~ "Y", 
                              RecordFoundYN == "N" ~ "N")) %>% 
    select(RequesterReturnField, RecordFoundYN, IN_HES)
  
  
  # In HES or not or returned to your college 
  # Working with students enrolled at WMU according to NSC
  
  # if a student enrolled at WMU, a target college, and other colleges
  # it will identify which students enrolled at WMU only, 
  # and which enrolled at other colleges
  # This is helpful when trying screen out the students who enrolled at a target university
  # if the only enrollment at the target university exists, the function will mark 'N' in IN_HES
  
  # Step 2: working with students enrolled at your institution ####
  #library(NSC)
  set2 <- NSC::your_college(x=data, 
                            target_college <- college_name)
  
  your_institution_students <- set2$all
  
  
  # add a column to enr1 for unioning
  enr2 <- cbind(enr1, RE_RETURN = "N/A")
  # Union enr1 and out_hes
  enr_final <- union_all(enr2, your_institution_students)
  # head(enr_final) # good
  
  # Step 3: Enrolled at 2 or 4 year college ####
  # This section will get types of institution of student enrollment
  
  college_type <- NSC::college_length(x=data)
  
  # Step 4: get graduation ####
  graduated_all <- NSC::one_from_Graduated(x=data)
  
  
  # Step 5: Get degree levels ####
  students_highestd <- NSC::highest_degree(x=data)
  
  # merge it to previous data frame
  
  final_ds <- left_join(enr_final, college_type,
                        by = "RequesterReturnField") %>% 
    left_join(., graduated_all, 
              by = "RequesterReturnField") %>% 
    left_join(., students_highestd, 
              by = "RequesterReturnField")          
  
  # Change all NA to "N/A"
  final_ds[is.na(final_ds)] <- "N/A"
  return(final_ds)
} # function


