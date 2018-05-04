# test functions

test.csv <- function(filename) {
  if(length(grep('.csv', filename)) == 0) {
    tempname <- deparse(substitute(filename))
    stop(paste("No file extension given for input", tempname, "that was defined in master_run_file.R"), 
         call. = FALSE)
  }
}