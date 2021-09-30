#' your_college(): A Function to Analyze Students Who Attended Your Institution
#'
#' When you conduct a search at a specific search date, you may end up having students enrolled 
#' at your institution. This function allows you to find out whether they returned to your institution 
#' after they left for a semester or more; whether they left the higher education system (HES) 
#' completely. This is known to work with the query option = "SE". 
#' 
#' @param x A data set obtained from NSC. The query option is "SE". You must read it into R and remove "." in the column names. 
#' @param id_col_num A column number for the "RequesterReturnField", a column name in the 
#' data set. 
#' @param CollegeName_col_num A column number for "CollegeName", a column name in the data set.
#' @param target_college Your institution's name in all CAPS. 
#' @return It will return the students who dropped out of the higher education system [HES], 
#' the students who left your institution and did not enroll else where.
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
#' # Get a list of students who attended your institution and other colleges. 
#' # If they attend your institution and other colleges at different times, 
#' # other colleges (the later colleges) are selected.
#' eg1$one_college
#' 
#' # Students who dropped out of HES 
#' eg1$HE_dropout
#' 
#' # Students who returned to your institution after they left 
#' eg1$returned_later
#' 
#' @seealso [NSC()],

your_college <- function(x, 
                         id_col_num, 
                         CollegeName_col_num, 
                         target_college){
  require(dplyr)
  # Studentss who enrolled at your college
  enr_person <- x %>% 
    filter(CollegeName == "WESTERN MICHIGAN UNIVERSITY") %>% 
    select(RequesterReturnField) %>% 
    distinct()
  
  ds <-  x %>% 
    filter(RequesterReturnField %in% enr_person$RequesterReturnField) %>%
    filter(Graduated == 'N') %>% 
    arrange(RequesterReturnField,EnrollmentBegin) 
  
  
  # change the value of the target institution by adding "ZZ" for odering
  ds[ds == target_college] <- paste0("ZZ", target_college) 
  # order the data using CollegeName
  ds2 <-  ds[order(ds[,id_col_num], (ds[,CollegeName_col_num])),]  
  id <-NULL
  id[1] <- 1
  for (i in 1:nrow(ds2)-1){
    j<-i+1
    
    if(identical(ds2[i,id_col_num], ds2[j,id_col_num])==TRUE){
      id[j]<-id[i]+1  
    } else {
      id[j] <-1
    }
  }
  # combind id partitioned over person and the data
  # select only id = 1
  # This select a non-targe college if students attend the your (target) institution and 
  # another institution, resulting in enrollment at one institution. 
  ds3 <- cbind(id, ds2) %>% filter(id==1) %>% 
    select(1:dim(ds2)[2]+1)
  
  # select those who did not enroll at all (HES drop out)
  
  # The students who left your institution and did not enroll elsewhere, 
  # and those who left your institution and come back
  ds4 <- ds3 %>% 
    filter(ds3[,CollegeName_col_num]== paste0("ZZ", target_college)) %>% 
    mutate(IN_HES = case_when(EnrollmentStatus == "W" & Graduated == "N"~"N", 
                              EnrollmentStatus != "W" & Graduated == "N"~"Y")) %>% 
    select(RequesterReturnField, IN_HES) 
  # select those who left your institutin and did not enroll elsewhere
  ds5 <- ds4 %>% 
    filter(IN_HES == "N")
  # select those who left your institution and comeback compared to the search date
  ds6 <- ds4 %>%   
    filter(IN_HES=="Y")
  
  
  return(list(one_college = ds3, HE_dropout = ds5, returned_later = ds6))
}


