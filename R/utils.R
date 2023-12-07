#' Converts a list of parameters into a table
#'
#' @param opt List of parameters
#'
#' @return data.frame with two columns called 'Parameter' and 'Value'
#' @export
#' @importFrom utils stack
#'
#' @examples
#' opt_to_df(opt_list)
#'
opt_to_df<-function(opt){
  opt<-stack(opt)
  names(opt)<-c('Value','Parameter')
  opt<-opt[,c('Parameter','Value')]
  return(opt)
}

#' Create log file with standard header including script name and command line options
#'
#' This function creates a log file and writes a standard header to it. The header includes the script name, contact information, and any command line options passed to the script. It's useful for tracking and documenting script runs.
#'
#' @param log_file A string specifying the path and name of the log file to be created or appended.
#' @param opt A list or other object containing the command line options to be written to the log file.
#' @param script A string specifying the name of the script for which the log is being created.
#' @param start.time The start time of the process or operation, expected to be an object of class POSIXct as obtained from Sys.time(). This parameter typically captures the time at which a particular process or analysis was initiated. It's used within the function to calculate durations, log time stamps, or for other time-related operations.
#'
#' @return Invisibly returns NULL. This function is used for its side effect of writing to a log file.
#' @export
#'
#' @examples
#' # Assuming 'opts' is a list of options and 'script_name' is the name of the script
#' start_time <- Sys.time()
#' log_header(paste0(tempdir(),"/logfile.log"), opt_list, "script_name.R", start_time)
log_header <- function(log_file, opt, script, start.time) {
  sink(file = log_file, append = FALSE)
  cat0(
    '#################################################################\n',
    '# ', script, '\n',
    '# For questions contact Oliver Pain (oliver.pain@kcl.ac.uk)\n',
    '#################################################################\n'
  )
  cat0('---------------\n')
  print.data.frame(opt_to_df(opt), row.names = FALSE, quote = FALSE, right = FALSE)
  cat0('---------------\n')
  cat0('Analysis started at ', as.character(start.time), '\n')
  sink()
}

#' Add text to an existing log file
#'
#' This function adds a specified text message to an existing log file. If no log file is specified, the message is printed to the console.
#'
#' @param log_file A string specifying the path and name of the log file to be appended. If NULL (default), the message is printed to the console.
#' @param message A string or object containing the text message to be added to the log file.
#' @param sep A string specifying the separator to be used after the message. Defaults to a newline character ('\\n').
#'
#' @return Invisibly returns NULL. The function is used for its side effect of writing to a log file or printing to the console.
#' @export
#'
#' @examples
#' log_add(paste0(tempdir(),"/logfile.log"), "This is a log message")
#' log_add(message = "This is a console message")
log_add <- function(log_file = NULL, message, sep = '\n') {
  if(is.null(log_file)) {
    cat(message, sep = sep)
  } else {
    sink(file = log_file, append = TRUE)
    cat(message, sep = sep)
    sink()
  }
}

#' cat function with '' as default separator
#'
#' This function is a wrapper around the base R 'cat' function, with the default separator set to an empty string.
#'
#' @param ... Arguments passed to the base 'cat' function. These can include objects to be concatenated and printed.
#' @param sep A string specifying the separator to be used between the objects. Defaults to an empty string.
#' @param file A connection, or a character string naming the file to print to. If "" (the default), cat0 prints to the standard output connection.
#' @param append Logical. If TRUE, the output will be appended to the file; otherwise, it will overwrite the contents of the file.
#'
#' @return This function invisibly returns NULL. It is used for its side effect of printing to the console or a file.
#' @export
#'
#' @examples
#' cat0("Hello", "World") # prints "HelloWorld"
#' cat0("Hello", "World", sep = " ") # prints "Hello World"
#' cat0("Line1", "Line2", sep = "\n") # prints "Line1" and "Line2" on separate lines
cat0 <- function(..., sep = '', file = "", append = FALSE) {
  cat(..., sep = sep, file = file, append = append)
}
