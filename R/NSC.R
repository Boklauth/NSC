#' @rdname format_data
#' @export
NSC <- function(school_code, branch_code, school_name, file_creation_date,
                query_option, file_dir, input_file_name, suffix) {
  format_data(school_code = school_code, branch_code = branch_code,
              school_name = school_name, file_creation_date = file_creation_date,
              query_option = query_option, file_dir = file_dir,
              input_file_name = input_file_name, suffix = suffix)
}
