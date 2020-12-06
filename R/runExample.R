#' @export
runExample <- function() {
  
  print(list.files())
  
  appDir <- system.file("shiny-examples", "interviewr", package = "interviewr")
  
  if (appDir == "") {
    
    stop("Could not find example directory. Try re-installing `interviewr`.", call. = FALSE)
    
  }
  
  shiny::runApp(appDir, display.mode = "normal")
  
}
