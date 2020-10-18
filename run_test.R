pckgs <- c("shiny", "shinyAce", "shinyjs", "glue", "V8", "reticulate", "Rcpp")


mssng_pckg <- pckgs[!(pckgs %in% installed.packages())]
if(length(mssng_pckg)) install.packages(mssng_pckg)

sapply(pckgs, library, character.only = TRUE)
# library(shiny)
# library(shinyAce)
# library(shinyjs)
# library(glue)
# library(V8)
# library(reticulate)
# library(Rcpp)

setwd("C:/Users/User11/Desktop/Transfer/TMP/AWS_Backup/OnlineCodeTest")
options(stringsAsFactors = FALSE)
source("questions.R")

language_choices <- c("R", "Python", "SQL", "Javascript", "C++")

selectedQuestionIndex <- c(1, 3, 4)
selectedLanguageIndex <- c(1, 2)

language_choices <- language_choices[selectedLanguageIndex]
questions <- questions[selectedQuestionIndex]

time_to_use <- 140

py_evaluate <- function(code) {
  builtins <- reticulate::import_builtins(convert = TRUE)
  globals <- reticulate::py_eval("globals()", convert = FALSE)
  locals <- globals
  parsed <- builtins$compile(code, "<string>", "single")
  a <<- builtins$eval(parsed, globals, locals)
}
x <- capture.output(py_evaluate("22"))



modes <- getAceModes()
themes <- getAceThemes()

ui <- shinyUI(
  fluidPage(
    useShinyjs(),
    h2("Online Code Test:"),
    # selectInput("mode", "Mode: ", choices = modes, selected = "r"),
    # selectInput("theme", "Theme: ", choices = themes, selected = "ambience"),
    uiOutput("config"),
    uiOutput("codeEditor"),
    uiOutput("nextQue")
  )
)

server <- shinyServer(function(input, output, session) {
  
  observe({
    req(global$time_left)
    if(global$time_left == 30){
      showModal(modalDialog(
        title = "Interruption",
        "This is a simulated interruption from your potential colleague/customer. Corporate workplaces might
      have interruptions while you code."
      ))
    }
  })
  
  output$nextQue <- renderUI({
    if(!global$finished){
      actionButton("nextQuestion", "Submit / Next Question")      
    }
  })
  
  output$config <- renderUI({
    if(!global$finished){
      tagList(
        htmlOutput("time_left"),
        selectInput(inputId = "language", label = "Select Language: ", choices = language_choices),
        textOutput("question")
      )
    }
  })
  
  # not working
  observe({
    req(input$comment)
    print(input$comment)
    shinyjs::runjs("$('#comment').attr('maxlength', 10)")    
  })
  
  global <- reactiveValues(
    # time_left  = time_to_use,
    questionNr = 1, 
    start_time = Sys.time(),
    time_to_use = time_to_use,
    display = TRUE,
    answersCode = list(),
    answersResult = list(),    
    answersCorrect = list(),
    finished = FALSE
  )
  
  observe({
    invalidateLater(millis = 1000)
    global$time_left <- round(difftime(global$start_time, Sys.time(), units = "secs") + global$time_to_use)
  })
  
  output$time_left <- renderText({
    print(global$time_left)
    if(global$time_left < 1){
      return("Sorry, you dont have time left.")
    }else if(global$time_left > 0 & global$time_left < 30){
      return(paste('<font color=\"#FF0000\"><b> ', global$time_left,' seconds left!</b></font>. Editor will disappear afterwards.'))
    }
    glue("Seconds left: {global$time_left}")
  })
  
  observeEvent(global$time_left, {
    global$display <- global$time_left > 0
  })
  
  output$codeEditor <- renderUI({
    input$nextQuestion
    if(global$display & !global$finished){
      isolate({
        fluidRow(
          column(
            12,
            h5("Editor:"),
            # uiOutput("uiAce"),
            aceEditor("code", mode = "r", height = "200px", value = "1"), #init[[input$language]]
            actionButton("eval", "Run Code")
          ),
          fluidRow(
            column(
              12,
              h5("Console / Output:"),
              verbatimTextOutput("output")
            )
          )
        )      
      })      
    }else if(!global$display & !global$finished){
      textInput(
        inputId = "comment", 
        label = "Optional: Leave a comment - (What would you have done with more time)", 
        placeholder = "I would have vectorised the code."
      )
    }else{
      h4("You are finished! Thank you for participating.")
    }
    
  })
  
  output$question <- renderText({
    questions[[global$questionNr]]$text
  })
  
  observeEvent(eventExpr = input$nextQuestion, {
    global$answersCode[[global$questionNr]] <- input$code
    correctSolution <- questions[[global$questionNr]]$solution
    providedSolution <- eval(parse(text = isolate(input$code)))
    global$answersResult[[global$questionNr]] = providedSolution
    global$answersCorrect[[global$questionNr]] = correctSolution == providedSolution
    print(global$answersCode)
    print(global$answersResult)
    print(global$answersCorrect)    
    
    
    if(global$questionNr < length(questions)){
      global$questionNr <- global$questionNr + 1
      global$time_to_use <- questions[[global$questionNr]]$time
      global$start_time <- Sys.time()
      print(global$answers)
    }else{
      global$finished <- TRUE
    }
  })
  
  output$uiAce <- renderUI({
    aceEditor("code", mode = "r", height = "200px", value = "1") #init[[input$language]]   
  })
  
  observe({
    updateAceEditor(
      session,
      "ace",
      theme =  "ambience", #input$theme,
      mode = "r",  #input$mode,
      tabSize = 4,
      useSoftTabs = TRUE,
      showInvisibles = TRUE
    )
  })
  
  output$output <- renderPrint({
    input$eval
    if(input$language == "R"){
      return(eval(
        parse(
          text = isolate(input$code)
        )
      ))
    }
    if(input$language == "Javascript"){
      ctx <- v8();
      return(ctx$eval(isolate(input$code)))
    }
    
    if(input$language == "Python"){
      print(input$code)
      print(py_evaluate(input$code))
      print(88)
      isolate({
        print(py_evaluate(input$code))
        # return(py_evaluate(input$code))        
      })
    }
  })
  
})

shinyApp(ui, server) #, launch.browser = TRUE)

