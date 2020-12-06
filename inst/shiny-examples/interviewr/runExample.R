#' @export
runExample <- function() {
  
  print(list.files())
  print(file.exists("R/questions.R"))
  print(dir.exists("R"))
  
  appDir <- system.file("shiny-examples", "interviewr", package = "interviewr")
  
  if (appDir == "") {
    
    stop("Could not find example directory. Try re-installing `interviewr`.", call. = FALSE)
    
  }
  
  shiny::runApp(appDir, display.mode = "normal")
  
}
