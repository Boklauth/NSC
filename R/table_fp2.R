#' Create A Table with Frequency and Percentage with Total Columns
#'
#' This function allows you to create a frequency table with percentage and Total columns. 
#' It can use the total from a frequency data or a user's total vector. This 
#' is for the query option = "SE". 
#' 
#' @param x A frequency table (e.g., one created by table()). 
#' @param rounding_dec An integer value indicating the rounding decimal 
#' (e.g., 1, 2, or 3). The default value is 1. 
#' @param use_mytotal If provided, this is a vector of numeric values, which will
#' be used as the denominators when computing percentages. The vector length 
#' must equal the number of the row of x. If it is NULL, the total vector will 
#' be calculated row-wise from x. 
#' @return It will return a frequency and percentage table showing enrollment 
#' of students in any colleges with various length types. 
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
#' mytotal <- c(2891, 3138, 2990, 2893, 2539) # the total number of students              
#' table_fp2(x = eg1, use_mytotal = mytotal) 
#' 
#' @seealso [NSC()], [college_length()], [for_NSC_crosstab()], [highest_degree()],[one_from_Graduated()], [your_college()]

table_fp2 <- function(x, rounding_dec=NULL, use_mytotal=NULL){
  
  if(is.null(rounding_dec)){
    rounding_dec = 1
  } else {
    rounding_desc = rounding_desc
  }
  
  # set data set
  data <- x
  
  # get number of columns
  ncols <- ncol(data)
  
  # use total from the table
  if(is.null(use_mytotal)){
      # data <- rbind(data, Total = colSums(data))
      data2 <- cbind(data, Total = rowSums(data[,1:ncols]))  
      user_cnames <- colnames(data)
      # create variable names for frequency
      user_cnames_f <- NULL
      for (i in 1:ncols){
          user_cnames_f <-cbind(user_cnames_f, paste0(user_cnames[i], "_F"))
      }
    user_cnames_f <- cbind(user_cnames_f, "Total_F")
    # apply the new columns names to freq table
    colnames(data2) <- user_cnames_f
    # calculate percentages
    data_p <- matrix(rep(0, nrow(data2)*(ncols)), 
                     nrow=nrow(data2))
    for (i in 1:ncols){
      for(j in 1:nrow(data2)){
        data_p[j,i] <- 100*data2[j,i]/data2[j,ncols+1]
      }
    }
    data_p2 <- cbind(data_p, Total=rowSums(data_p))
    
    # create variable names for percentage
    user_cnames_p <- NULL
    for (i in 1:ncols){
      user_cnames_p <-cbind(user_cnames_p, paste0(user_cnames[i], "_P"))
    }
    user_cnames_p <- cbind(user_cnames_p, "Total_P")
    
    # apply new names to the percentage table
    colnames(data_p2) <- user_cnames_p
    
    # combine freq and percentage tables
    data3 <- round(cbind(data2, data_p2), rounding_dec)
    return(data3)
  } # end condition when use_mytotal is NULL 
  
  # start condition when use_mytotal is NOT NULL
  if (!is.null(use_mytotal)){
      if (length(use_mytotal)!=nrow(x)){
        stop("The vector of total must be of the same length as the number 
             of rows of your data set.")
      } else {
        # data <- rbind(data, Total = colSums(data))
        data2 <- cbind(data, Total = rowSums(data[,1:ncols]))  
        
        user_cnames <- colnames(data)
        # create variable names for frequency
        user_cnames_f <- NULL
        for (i in 1:ncols){
          user_cnames_f <-cbind(user_cnames_f, paste0(user_cnames[i], "_F"))
        }
        user_cnames_f <- cbind(user_cnames_f, "Total_F")
        # apply the new columns names to freq table
        colnames(data2) <- user_cnames_f
        
        
        # calculate percentages
        data_p <- matrix(rep(0, nrow(data2)*(ncol(data2))), 
                         nrow=nrow(data2))
        for (i in 1:ncol(data2)){
          for(j in 1:nrow(data2)){
            data_p[j,i] <- 100*data2[j,i]/use_mytotal[j]
          }
        }
        data_p2 <- data_p
        
          # create variable names for percentage
          user_cnames_p <- NULL
          for (i in 1:ncol(data)){
            user_cnames_p <-cbind(user_cnames_p, paste0(user_cnames[i], "_P"))
          }
          user_cnames_p <- cbind(user_cnames_p, "Total_P")
          
          # apply new names to the percentage table
          colnames(data_p2) <- user_cnames_p
          
          # combind freq and percentage tables
          data3 <- round(cbind(data2, data_p2), rounding_dec)
          # return output
            return(data3)
    } # End condition when length of total is not equal to nrow(x) 
  } # End condition when use_mytotal is NOT NULL
} # End function

