#' A Function to Analyze Students Who Attended Your Institution
#'
#' When you conduct a search at a specific search date, you may end up having 
#' students enrolled at your institution. This function allows you to find out 
#' whether they returned to your institution after they left for a semester or 
#' more; whether they left the higher education system (HES) completely. This 
#' is for the query option = "SE". 
#' 
#' @param x A data set obtained from NSC. The query option is "SE". The original 
#' column names given by the National Student Clearinghouse must be used. You 
#' must read it into R and remove "." in the column names. See the example below. 
#' @param target_college Your institution's name in all CAPS in single or double 
#' quotation markds. 
#' @return It will return the students that dropped out of the higher education system (HES) and 
#' the stopout students, the students that left your institution for at least one semester and returned 
#' to your institution, and students who graduated from your institution as a reason for not returning.
#' This is only true when the search date associated with each student is
#' the semester the students were not enrolled at your institution. 
#'
#' @export
#' @examples
#' # Read data
#' setwd("C/Users/Dell/Documents")
#' file_name <- "00233001_463454_DETLRPT_SE_09282021192135_20210927_se_data_output.csv"
#' data <- read.csv(paste0(getwd(), "/", file_name), 
#'                  header=TRUE, check.names = TRUE)
#' # Remove "." in names
#' names(data) <- gsub("\\.", "", names(data))
#'
#' eg1 <- your_college(x = data, 
#'              id_col_num = 6, 
#'              CollegeName_col_num = 10, 
#'              target_college = "WESTERN MICHIGAN UNIVERSITY")
#'              
#'              
#' # Get a list of students who returned to your institution, left higher ed. system
#' # and those who graduated from your institution. 
#' eg1$all
#' 
#' # Students who dropped out of HES 
#' eg1$HE_dropout
#' 
#' # Students who returned to your institution after they left 
#' eg1$returned_later
#' 
#' # Students who graduated from your institution as a reason of not returning
#' eg1$your_grad
#' 
#' @seealso [NSC()],

your_college <- function(x, target_college){
  require(dplyr)
  
  # error message about target_college
  if(is.na(target_college) | is.null(target_college)){
    stop("You must provide a value for terget_college.")
  }
  
  # Studentss who enrolled at your college
  enr_person <- x %>% 
    filter(CollegeName == target_college) %>% 
    select(RequesterReturnField) %>% 
    distinct()
  
  ds <-  x %>% 
    filter(RequesterReturnField %in% enr_person$RequesterReturnField) %>%
    #filter(Graduated == 'N') %>% 
    arrange(RequesterReturnField,EnrollmentBegin) 
  
  
  # change the value of the target institution by adding "ZZ" for odering
  ds[ds == target_college] <- paste0("ZZ", target_college) 
  # order the data using CollegeName
  ds2 <-  ds[order(ds$RequesterReturnField, ds$CollegeName),]  
  
  id <-NULL
  id[1] <- 1
  df <- ds2
  for (i in 1:nrow(df)-1){
    j<-i+1
    
    if(identical(df$RequesterReturnField[i], df$RequesterReturnField[j])==TRUE){
      id[j]<-id[i]+1  
    } else {
      id[j] <-1
    }
  }
  
  
  # combind id partitioned over person and the data
  # select only id = 1
  # This select a non-targe college if students attend the your (target) institution and 
  # another institution, resulting in enrollment at one institution. 
  # one_college = ds3 # is one college per student. 
  # If student attends your college and another college subsequently
  # then another college is selected.
  
  ds3 <- cbind(id, ds2) %>% filter(id==1) %>% 
    select(1:dim(ds2)[2]+1)
  
  # select those who did not enroll at all (HES drop out)
  
  # The students who left your institution and did not enroll elsewhere, 
  # and those who left your institution and come back
  # Then there are those who graduated from your institution, no enrollment status
  ds4 <- ds3 %>% 
    filter(ds3$CollegeName== paste0("ZZ", target_college)) %>% 
    mutate(IN_HES = case_when(EnrollmentStatus == "W" & Graduated == "N"~"N", 
                              EnrollmentStatus != "W" & Graduated == "N"~"Y", 
                              (is.na(EnrollmentBegin) | EnrollmentStatus=="") & 
                                Graduated == "Y"~"N/A"))%>% 
    mutate(RE_RETURN = case_when(IN_HES == "Y"~"Y", 
                                 TRUE ~ "N/A")) %>% 
    select(RequesterReturnField, RecordFoundYN, IN_HES, RE_RETURN) 
  
  # select those who left your institution and did not enroll elsewhere
  ds5 <- ds4 %>% 
    filter(IN_HES == "N")
  # select those who left your institution and comeback compared to the search date
  ds6 <- ds4 %>%   
    filter(IN_HES=="Y")
  # select those who graduated from your institution
  ds7 <- ds4 %>%  
    filter(IN_HES == 'N/A')
  # select everyone above: ds4  

  
  return(list(all = ds4, 
              HE_dropout = ds5, 
              returned_later = ds6, 
              your_grad = ds7))
}


