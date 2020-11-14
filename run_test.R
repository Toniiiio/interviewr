user_info <- data.frame(
  id = "awd2z",
  name = "",
  participated = FALSE
)
#write.csv2(x = user_info, file = "users.txt")
#


pckgs <- c("shiny", "shinyAce", "shinyjs", "glue", "V8", "reticulate", "Rcpp", "magrittr")
#VG9uaW8gTGllYnJhbmQ=

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

#setwd("C:/Users/User11/Desktop/Transfer/TMP/AWS_Backup/OnlineCodeTest")
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
  builtins$eval(parsed, globals, locals)
}
py_evaluate("22")



modes <- getAceModes()
themes <- getAceThemes()

ui <- shinyUI(
  fluidPage(
    useShinyjs(),
    # uiOutput("ui2"),
    h2("Online Code Test:"),
    uiOutput("test_ui"),
  )
)

server <- shinyServer(function(input, output, session) {



  observe({
    global$user_info <- read.csv2(file = "users.txt")
  })

  observe({

    global$user_id <- urltools::param_get(session$clientData$url_search)$id
    req(global$user_id)
    global$is_valid_id <- global$user_id %in% global$user_info$id

    if(is.null(global$user_id)) global$is_valid_id <- FALSE

    if(global$is_valid_id){
      global$user_name <- global$user_info[global$user_info$id == global$user_id]$name
      # global$user_name = print(RCurl::base64Decode(x))

      global$has_participated = global$user_info[global$user_info$id == global$user_id]$participated
      if(global$has_participated) print("The test was already taken for the provided id.")

    }

  })

  global <- reactiveValues(
    # time_left  = time_to_use,
    test_started = FALSE,
    question_nr = 1,
    questions = questions,
    is_valid_id = NULL,
    has_participated = FALSE,
    start_time = Sys.time() + 1e6, # avoid to trigger interruption message before test was started
    time_to_use = time_to_use,
    display = TRUE,
    hint_nr = 0,
    answersCode = list(),
    answersResult = list(),
    answersCorrect = list(),
    finished = FALSE
  )

  observeEvent(input$start_test, {
    global$test_started <- TRUE
  })

  observeEvent(global$set_as_participated, {
    global$start_time <- Sys.time()
    global$user_info[global$user_info$id == global$user_id]$participated <- TRUE
    #write.csv2(x = global$user_info, file = "users.txt")
  })

  output$test_ui <- renderUI({

    req(!is.null(global$is_valid_id))

    if(global$test_started){

      out <- tagList(
        uiOutput("config"),
        uiOutput("codeEditor"),
        uiOutput("nextQue")
      )

      global$set_as_participated <- TRUE

    }else if(!global$is_valid_id){

      out <- h5(paste0("We cant find an open test for the given id!"))

    }else if(global$has_participated){

      out <- h5(paste0("The test was already taken for the provided id!"))

    }else{

      out <- tagList(
        h5(paste0("Welcome ", global$user_name, "!")),
        actionButton(inputId = "start_test", label = "Start test!")
      )

    }

    return(out)

  })


  observe({

    hint_title <- "Hint"
    req(input$show_hint)

    if(input$show_hint){
      isolate(global$hint_nr <- global$hint_nr + 1)

      showModal(modalDialog(
        title = hint_title,
        global$questions[[global$question_nr]]$hints[global$hint_nr]
#        "What do all points on the circle have in common?."
      ))

    }
  })


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
      actionButton("next_question", "Submit / Next Question")
    }
  })

  output$config <- renderUI({

    global$amt_hints <- length(global$questions[[global$question_nr]]$hints)
    
    # could use shinyjs::disable that does not work when being integrated
    if(global$amt_hints <= global$hint_nr){
      hint <- NULL 
    }else{
      hint <- actionButton(inputId = "show_hint", label = paste0("Show hint (", global$hint_nr, " / ", global$amt_hints,")"))
    }

    if(!global$finished){
      out <- tagList(
        htmlOutput("time_left"),
        selectInput(inputId = "language", label = "Select Language: ", choices = language_choices),
        hint,
        textOutput("question")
      )
      return(out)
    }
  })

  
  ## not working  
  # observeEvent((input$dss | global$hint_nr), {
  #   req(global$amt_hints)
  #   if(global$amt_hints > global$hint_nr){
  #     print("yoo")
  #     shinyjs::disable("show_hint")
  #   }
  # }, priority = -1)

  # not working
  observe({
    req(input$comment)
    print(input$comment)
    shinyjs::runjs("$('#comment').attr('maxlength', 10)")
  })

  observe({
    invalidateLater(millis = 1000)
    global$time_left <- round(difftime(global$start_time, Sys.time(), units = "secs") + global$time_to_use)
  })

  output$time_left <- renderText({

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
    
    input$next_question
    
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
    global$questions[[global$question_nr]]$text
  })

  observeEvent(eventExpr = input$next_question, {
    
    global$answersCode[[global$question_nr]] <- input$code
    correctSolution <- global$questions[[global$question_nr]]$solution
    providedSolution <- eval(parse(text = isolate(input$code)))
    global$answersResult[[global$question_nr]] = providedSolution
    global$answersCorrect[[global$question_nr]] = correctSolution == providedSolution
    # print(global$answersCode)
    print(global$answersResult)
    # print(global$answersCorrect)

    if(global$question_nr < length(global$questions)){
      
      global$question_nr <- global$question_nr + 1
      global$time_to_use <- global$questions[[global$question_nr]]$time
      global$start_time <- Sys.time()

    }else{


      global$finished <- TRUE
      save_global <- reactiveValuesToList(global)
      save(x = save_global, file = paste("results_", global$user_info$id, ".RData"))
      rmarkdown::render("test.Rmd", params = list(
        test_data = save_global
      ))
      print("rr4")
      
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
      # print(input$code)
      # print(py_evaluate(input$code))
      # print(88)
      isolate({
        print(py_evaluate(input$code))
        # return(py_evaluate(input$code))
      })
    }
  })
  
})

shinyApp(ui, server) #, launch.browser = TRUE)

