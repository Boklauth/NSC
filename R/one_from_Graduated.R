#' A Function to Graduation Status from Different Length Types of Colleges
#'
#' When you conduct a search at a specific search date, you may end up having 
#' students enrolled at more than one college concurrently. This function helps
#' you get graduation status ("Y" = Graduated) if you want to know whether 
#' a students graduated at all from one of the many colleges. If students did 
#' not at all graduate, it will also give the status "N". It will also map out 
#' the type of college (in terms of length), from which student graduated. 
#' This is for the query option = "SE". 
#' 
#' @param x A data set obtained from NSC. The query option is "SE". The original 
#' column names given by the National Student Clearinghouse must be used. You 
#' must read it into R and remove "." in the column names. See the example below. 
#' @return It will return a list of students who were enrolled in any colleges with various length types. 
#' 
#' @note In the output data set, 
#' @note Graduated_L is a graduation status for a less-than-two-year college (L).
#' @note Graduated_2 is a graduation status for a two-year college (2).
#' @note Graduated_4 is a graduation status for a four_year college (4).
#' #note ComGraduated results from concatenating graduation status combined for all colleges: L, 2, and 4.
#' @note Overall_Graduated is a graduation status for any colleges. 
#' If a student graduated from any colleges, it was coded as "Y" for graduated;
#' otherwise, "N" for not graduated from any colleges.
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
#' eg1 <- one_from_Graduated(x = data) 
#'              
#'              
#'              
#' # Get a list of students who graduated from various colleges
#' eg1
#' 
#' 
#' @seealso [NSC()],[your_college()], [college_length()]


one_from_Graduated <- function(x){
  require(dplyr)
  # one Graduated = Y and one Graduated = N
  x2 <- x %>% 
    dplyr::arrange(RequesterReturnField, 
                   desc(Graduated))  # order: Y then N
  id <-NULL
  id[1] <- 1
  df <- x2
  for (i in 1:nrow(df)-1){
    j<-i+1
    
    if(identical(df$RequesterReturnField[i], df$RequesterReturnField[j])==TRUE){
      id[j]<-id[i]+1  
    } else {
      id[j] <-1
    }
  }

  x3 <- cbind(id, x2) %>% 
    filter(id==1) %>% 
    select(1:dim(xx)[2]+1) %>% 
    select(RequesterReturnField, Graduated) %>% 
    rename(Overall_Graduated = Graduated)
  
  
  # Graduated from L, 2, and 4
  # function to get graduation status for each college
  
  GCollege <- function(x, college){
    ds <- x %>% 
      select(RequesterReturnField, X2year4year, Graduated) %>% 
      distinct() %>% 
      arrange(RequesterReturnField, desc(Graduated)) %>% 
      filter(X2year4year==college)
    
    # iterate to get one Y and N
    id <-NULL
    df <- ds
    for (i in 1:nrow(df)-1){
      j<-i+1
      
      if(identical(df$RequesterReturnField[i], df$RequesterReturnField[j])==TRUE){
        id[j]<-id[i]+1  
      } else {
        id[j] <-1
      }
    }
    
    ds_final <- cbind(id, df) %>% 
      filter(id==1) %>% 
      select(1:dim(df)[2]+1) 
      
    
    return(ds_final)
  }
  
  # for L college
  GCL <- GCollege(x, college= "L")  %>% 
    select(RequesterReturnField, Graduated) %>% 
    rename(Graduated_L = Graduated)
  # for 2 y college
  GC2 <- GCollege(x, college="2") %>% 
    select(RequesterReturnField, Graduated) %>% 
    rename(Graduated_2 = Graduated)
  # for 4 y college
  GC4 <- GCollege(x, college="4") %>% 
    select(RequesterReturnField, Graduated) %>% 
    rename(Graduated_4 = Graduated)
  
  # combine them
  join1 <- left_join(x3, GCL, by=c("RequesterReturnField" = "RequesterReturnField"))
  join2 <- left_join(join1, GC2, by=c("RequesterReturnField" = "RequesterReturnField"))
  join3 <- left_join(join2, GC4, by=c("RequesterReturnField" = "RequesterReturnField"))
  join3_add <- join3 %>% 
    mutate(ComGraduated =  paste0("L", Graduated_L, "_2", Graduated_2, "_4", Graduated_4))
  join3_add_reorder <- join3_add %>% 
    select(RequesterReturnField, Graduated_L, Graduated_2, Graduated_4, ComGraduated, Overall_Graduated)
  head(join3_add_reorder)
  return(join3_reorder)
}

