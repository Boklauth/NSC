#' A Function to classify Degree Levels
#'
#' Given a student may have a certificate, associate's degree, bachelor's 
#' degree and possible doctoral degree, this function classify those degree, 
#' titles and arrange them horizontally (flat) for each student from a 
#' a non-degree certificate, associate's degree, bachelor's degree, master's
#' degree, and doctoral degree. This is for the query option = "SE". 
#' 
#' @param x A data set obtained from NSC. The query option is "SE". The original 
#' column names given by the National Student Clearinghouse must be used. You 
#' must read it into R and remove "." in the column names. See the example below. 
#' @return It will return a dataframe of students with degrees. Only one degree 
#' at each degree level (e.g., Associate's Degree) is used if a student has 
#' more than one degree at that level. 
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
#' eg1 <- highest_degree(x = data)
#'              
#'              
#' # Get a list of students with/without their degrees
#' eg1
#' 
#' @seealso [your_college()],[college_length()], [one_from_Graduated()],
#' [keyring()], [Oracle()], [dbDriver()], [dbConnect()], [dbGetQuery()], [dbDisconnect()].

highest_degree <- function(x){
  require(dplyr)
  require(tidyverse)
  
  # select DegreeTitles that are present
x2 <- x %>% 
  select(RequesterReturnField, X2year4year, DegreeTitle) %>% 
  filter(DegreeTitle!="")
# changing values of degrees that conflict with each other
x2[x2=="MBA"] <- "MASTER OF Business Administration"
x2[x2=="MASTER OF BUSINESS ADMINISTRATION"] <- "MASTER OF Business Administration"
x2[x2=="MASTER OF BUSINESS ADMINISTRAT"] <- "MASTER OF Business Administration"
x2[x2=="MASTER OF BUSINESS ADMIN."] <- "MASTER OF Business Administration"
x2[x2=="MASTER OF BUSINESS ADMIN"] <- "MASTER OF Business Administration"
x2[x2=="MAC"] <- "MASTER"
x2[x2=="AS"] <- "ASSOCIATE DEGREE"
x2[x2=="BAA"] <- "BACHELOR DEGREE"
x2[x2=="BA"] <- "BACHELOR OF ARTS"
x2[x2=="BS"] <- "BACHELOR OF SCIENCE"
x2[x2=="CT"] <- "CERTIFICATE"
x2[x2=="BA LIBERAL ARTS AND SCIENCE"] <- "BACHELOR OF ARTS AND SCIENCE"

# classify DegreeTitle
  
  x3 <- cbind(x2, degree_level="N/A")
  degree_title_col_num <- 3
  for (i in 1:nrow(x3)){
    if(grepl("ASSOCIATE", x3[i,degree_title_col_num])|
       grepl("ASSOC", x3[i,degree_title_col_num])|
       grepl("A.A", x3[i,degree_title_col_num])|
       grepl("AS DEGREE", x3[i,degree_title_col_num])|
       grepl("AS -", x3[i,degree_title_col_num])|
       grepl("AA", x3[i,degree_title_col_num])|
       grepl("A.S.", x3[i,degree_title_col_num])|
       grepl("A.SCI.", x3[i,degree_title_col_num])|
       grepl("A.G.S.", x3[i,degree_title_col_num])|
       grepl("MACRAO", x3[i,degree_title_col_num])|
       grepl("PRACTICAL NURSING", x3[i,degree_title_col_num])|
       grepl("A.B.A.", x3[i,degree_title_col_num])){
      
      x3[i,dim(x3)[2]] = "A"
      
    } else if (grepl(".*BACH.*", x3[i,degree_title_col_num])|
               grepl(".*UNDERG.*", x[i,degree_title_col_num])|
               grepl("BS.", x3[i,degree_title_col_num])|
               grepl("B.S.", x3[i,degree_title_col_num])|
               #grepl("B S.", x3[i,degree_title_col_num])|
               grepl("B. S.", x3[i,degree_title_col_num])|
               grepl("BFA", x3[i,degree_title_col_num])|
               grepl("B A", x3[i,degree_title_col_num])|
               grepl("B B A.", x3[i,degree_title_col_num])|
               grepl("BBA", x3[i,degree_title_col_num])|
               grepl("B.B.A.", x3[i,degree_title_col_num])|
               grepl("BACH", x3[i,degree_title_col_num])|
               grepl("BA IN", x3[i,degree_title_col_num])|
               grepl("BA ", x3[i,degree_title_col_num])|
               grepl("UNDERGRADUATE", x3[i,degree_title_col_num])
               ){
      
      x3[i,dim(x3)[2]] = "B"
      
    } else if (grepl("ADDITIONAL MAJOR", x3[i,degree_title_col_num])|
               grepl(".*MINOR.*", x3[i,degree_title_col_num])){
      x3[i,dim(x3)[2]] = "B_MAJOR/MINOR" # additional major/minor
      } else if (grepl("CERT", x3[i,degree_title_col_num])|
                 grepl("CERTIFICATE", x3[i,degree_title_col_num])|
                 grepl("CRT", x3[i,degree_title_col_num])|
                 grepl("CORE CURRICULUM", x3[i,degree_title_col_num])|
                 grepl("GC", x3[i,degree_title_col_num])|
                 grepl("DIPLOMA", x3[i,degree_title_col_num])|
                 grepl("NON DEGREE", x3[i,degree_title_col_num])|
                 grepl("SPECIALIST", x3[i,degree_title_col_num])|
                 grepl("CREDENTIAL", x3[i,degree_title_col_num])|
                 grepl("LICENSURE", x3[i,degree_title_col_num])|
                 grepl("PBC IN DIETETICS", x3[i,degree_title_col_num])|
                 grepl("CRIMINAL JUSTICE", x3[i,degree_title_col_num])|
                 grepl("FIRE FIGHTER TECHNOLOGY", x3[i,degree_title_col_num])|
                 grepl("AIDED DESIGN", x3[i,degree_title_col_num])|
                 grepl("MAJOR CERT", x3[i,degree_title_col_num])){
      x3[i,dim(x3)[2]] = "C"
    } else if (grepl("MAST", x3[i,degree_title_col_num])|
               grepl("MSW", x3[i,degree_title_col_num])|
               grepl("MBA", x3[i,degree_title_col_num])|
               grepl("M S", x3[i,degree_title_col_num])|
               grepl("MS", x3[i,degree_title_col_num])|
               grepl("M.A", x3[i,degree_title_col_num])|
               grepl("MAE", x3[i,degree_title_col_num])| # MASTER OF COUNSELING
               grepl("CONCENTRATION", x3[i,degree_title_col_num])|
               grepl("M. S. ", x3[i,degree_title_col_num])
               ){
      
      x3[i,dim(x3)[2]] = "M"
      
    } else if (grepl("DOCT", x3[i,degree_title_col_num])|
               grepl("JD", x3[i,degree_title_col_num])|
               grepl("PSYD", x3[i,degree_title_col_num])|
               grepl("DR", x3[i,degree_title_col_num])|
               grepl("DC", x3[i,degree_title_col_num]) # Doctor of Chiropractic
              ){
        
        x3[i,dim(x3)[2]] = "D"
    } 
  }
  
  # highest degree obtained
  xx <- x3 %>% 
    filter(degree_level!= "B_MAJOR/MINOR" ) %>% 
    mutate(degree_value =case_when(degree_level == "C"~1,
                                   degree_level == "A"~2,
                                   degree_level == "B"~3, 
                                   degree_level == "M"~4, 
                                   degree_level == "D"~5)) %>% 
    arrange(RequesterReturnField, desc(degree_value))
    
  id <-NULL
  df <- xx
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
    select(1:dim(df)[2]+1) %>% 
    select(RequesterReturnField, degree_level) %>% 
    rename(highest_degree = degree_level)
  
  
  # Organize degrees/non-degrees: Certificate, A, B, M, D
  x4 <- x3 %>% 
    select(RequesterReturnField, degree_level) %>% 
    distinct()
  # spread data
  x5 <- cbind(x4, value = "Y")
  
  x6 <- spread(x5, degree_level, value=value) %>% 
    select(RequesterReturnField, C, A, B, M, D) %>% 
    rename(Certificate = C, Adegree = A, Bdegree=B, Mdegree=M, Ddegree=D)

  # join with original unique students
  base <- x %>% select(RequesterReturnField) %>% 
    distinct()
  join1 <- left_join(base, x6, 
                     by = "RequesterReturnField")
  # join with highest degree attained
  
  join2 <- left_join(join1, ds_final, by ="RequesterReturnField")

  
  return(join2)
}
