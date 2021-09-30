#' A Function to Put College Length Types in a Row
#'
#' When you conduct a search at a specific search date, you may end up having 
#' students enrolled at less-than-two-year colleges (L), two-year colleges, and 
#' four-year colleges. This function will place all college length types in a row
#' so that each student has one row of college. This 
#' is for the query option = "SE". 
#' 
#' @param x A data set obtained from NSC. The query option is "SE". The original 
#' column names given by the National Student Clearinghouse must be used. You 
#' must read it into R and remove "." in the column names. See the example below. 
#' @return It will return a list of students who were enrolled in any colleges with various length types. 
#' 
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
#' eg1 <- college_length(x = data) 
#'              
#'              
#'              
#' # Get a list of students who enrolled in the college
#' eg1$colleges_flat
#' 
#' 
#' @seealso [NSC()],[your_college()]


college_length <- function(x){
  require(dplyr)
  # Studentss who enrolled at your college
  allcollege <- x %>% 
    filter(EnrollmentStatus != "W") %>% 
    filter(!is.na(EnrollmentBegin)) %>% 
    filter(Graduated=="N") %>% # NSC uses Graduated = N for enrollment
    select(RequesterReturnField, X2year4year, PublicPrivate) %>% 
    distinct() %>% 
    arrange(RequesterReturnField, X2year4year)
  
  
  allcollege[allcollege == "L"] <- 1.9
  
  
  # Separate data
  # base data set for left join
  base <- allcollege %>% 
    select(RequesterReturnField) %>% 
    distinct()
  # college less than 2 years
  C1.9 <- allcollege %>% 
    select(RequesterReturnField, X2year4year) %>% 
    filter(X2year4year == 1.9) %>% distinct()
  # 2 year college
  C2 <- allcollege %>% 
    select(RequesterReturnField, X2year4year) %>% 
    filter(X2year4year == 2) %>% distinct()
  # four year college
  C4 <- allcollege %>% 
    select(RequesterReturnField, X2year4year) %>% 
    filter(X2year4year == 4) %>% distinct()
  
  # join data
  join1 <- left_join(base, C1.9, 
                     by=c("RequesterReturnField" = "RequesterReturnField"))
  join2 <- left_join(join1, C2, 
                     by=c("RequesterReturnField" = "RequesterReturnField"), 
                     suffix=c("1.9", "2"))
  join3 <- left_join(join2, C4, 
                     by=c("RequesterReturnField" = "RequesterReturnField"), 
                     ) %>% 
    mutate(all_colleges = gsub(" ", ", ", trimws(gsub("NA", "", 
                              paste(X2year4year1.9, X2year4year2, X2year4year))))) %>% 
    rename(CL2Y = X2year4year1.9, C2Y = X2year4year2, C4Y = X2year4year)
  
  
  return(collegerows = join3)
}


