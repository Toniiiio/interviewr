user_info <- data.frame(
  id = "awd2z",
  name = "",
  participated = FALSE
)
#write.csv2(x = user_info, file = "users.txt")
#


# pckgs <- c("shiny", "DBI", "shinythemes", "shinyAce", "shinyjs", "glue", "V8", "reticulate", "Rcpp", "magrittr")
# #VG9uaW8gTGllYnJhbmQ=
# mssng_pckg <- pckgs[!(pckgs %in% installed.packages())]
# if(length(mssng_pckg)) install.packages(mssng_pckg)
# sapply(pckgs, library, character.only = TRUE)
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

selectedQuestionIndex <- 1:5
allowed_language <- 1:5

questions <- questions[selectedQuestionIndex]

time_to_use <- 140

py_evaluate <- function(code) {
  builtins <- reticulate::import_builtins(convert = TRUE)
  globals <- reticulate::py_eval("globals()", convert = FALSE)
  locals <- globals
  parsed <- builtins$compile(code, "<string>", "single")
  builtins$eval(parsed, globals, locals)
}
x <- py_evaluate("print(22)")
x


modes <- shinyAce::getAceModes()
themes <- shinyAce::getAceThemes()


ui <- shinyUI(
  fluidPage(theme = shinythemes::shinytheme("cerulean"),
    shinyjs::useShinyjs(),
    # uiOutput("ui2"),
    h2("Online Code Test:"),
    uiOutput("test_ui")
  )
)

server <- shinyServer(function(input, output, session) {

  global <- reactiveValues(
    # time_left  = time_to_use,
    test_started = FALSE,
    question_nr = 1,
    user_id = NULL,
    editor_value = "1",
    is_r_plot = FALSE,
    questions = questions,
    used_language = rep(0, length(questions)) %>% as.list(),
    language_choices = language_choices,
    is_valid_id = NULL,
    hint_nr = rep(0, length(questions)) %>% as.list(),
    comments = rep("", length(questions)) %>% as.list(),
    has_participated = FALSE,
    start_time = Sys.time() + 1e6, # avoid to trigger interruption message before test was started
    time_to_use = time_to_use,
    display = TRUE,
    answers_code = rep("9", length(questions)) %>% as.list(),
    answers_result = rep("9", length(questions)) %>% as.list(),
    is_answer_correct = rep(FALSE, length(questions)) %>% as.list(),
    finished = FALSE
  )

  observe({
    
    global$language_choices <- global$questions[[global$question_nr]]$languages
    
  })
  
  observe({
    
    global$used_language[[global$question_nr]] <- input$language
    
  })
  
  
  observe({
    
    global$user_info <- read.csv2(file = "R/users.txt")
    
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


  observeEvent(input$start_test, {
    
    global$test_started <- TRUE
    
  })

  observeEvent(global$set_as_participated, {
    global$start_time <- Sys.time()
    global$user_info[global$user_info$id == global$user_id]$participated <- TRUE
    #write.csv2(x = global$user_info, file = "users.txt")
  })
  
  output$sql_table <- renderDataTable(
    mtcars,
    options = list(pageLength = 5)
  )
  
  output$show_sql_table <- renderUI({
    
    if(input$language == "SQL"){
      
      tagList(
        h5("Preview of the sql data table."),
        br(),
        dataTableOutput("sql_table")        
      )

      
    }
    
  })

  output$test_ui <- renderUI({

    req(!is.null(global$is_valid_id))

    if(global$test_started){

      out <- tagList(
        uiOutput("config"),
        uiOutput("show_sql_table"),
        uiOutput("code_editor")
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
      isolate(global$hint_nr[[global$question_nr]] <- global$hint_nr[[global$question_nr]] + 1)

      showModal(modalDialog(
        title = hint_title,
        global$questions[[global$question_nr]]$hints[global$hint_nr[[global$question_nr]]]
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
    if(global$amt_hints <= global$hint_nr[[global$question_nr]] & !global$finished){
      
      hint <- NULL 
      
    }else{
      
      hint <- actionButton(inputId = "show_hint", label = paste0("Show hint (", global$hint_nr[[global$question_nr]], " / ", global$amt_hints,")"))
      
    }

    if(!global$finished){
      
      out <- tagList(
        htmlOutput("time_left"),
        selectInput(inputId = "language", label = "Select Language: ", choices = global$language_choices),
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


  observe({
    
    req(input$comment)
    global$comments[[global$question_nr]] <- input$comment
    # not working    
    #shinyjs::runjs("$('#comment').attr('maxlength', 10)")
    
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
  
  # observe({
  # 
  #   req(input$code)
  #   global$editor_value <- input$code
  # 
  # })
  

  observeEvent(input$language, {

    isolate({
      # todo: refactor
      global$editor_value = ifelse(
        test = input$language == "SQL",
        yes = "SELECT * FROM airquality LIMIT 5",
        no = "1"
      )

      if(input$language == "Python"){
        global$editor_value = "result = 42"
      }

    })

  })


  output$code_editor <- renderUI({
    
    input$next_question
    global$editor_value
    
    isolate({
      
      is_r_plot <- grepl(pattern = "plot(", x = input$code, fixed = TRUE)
      
      if(global$display & !global$finished){
        print(is_r_plot)
        
        # if(is_r_plot){
        # 
        #   console_output <- "" #plotOutput("console_output2")
        #    
        # }else{
        #    
        # 
        #  
        # }
        console_output <- verbatimTextOutput("console_output")
        
        isolate({
          fluidRow(
            column(
              12,
              h5("Editor:"),
              # uiOutput("uiAce"),
              
              aceEditor("code", mode = "r", height = "200px", value = global$editor_value), #init[[input$language]]
              fluidRow(
                column(width = 1, offset = 0, actionButton("eval", "Run Code")),
                column(width = 1, offset = 0, style='padding:0px;', uiOutput("nextQue")),
                column(width = 10)
              )
            ),
            column(
              12,
              h5("Console / Output:"),
              console_output,
              plotOutput("console_output2")
            )
          )
        })
      }else if(!global$finished){ # also make it possible for user to provide comment when the time
        # is not up yet -> dont use !global$display & 
        textInput(
          inputId = "comment",
          label = "Optional: Leave a comment - (What would you have done with more time?)",
          placeholder = "I would have vectorised the code."
        )
      }else{
        h4("You are finished! Thank you for participating.")
      }
      
    })

  })

  output$question <- renderText({
    
    global$questions[[global$question_nr]]$text
    
  })

  observeEvent(eventExpr = input$next_question, {
      
    global$answers_code[[global$question_nr]] <- as.character(input$code)
    correct_solution <- global$questions[[global$question_nr]]$solution
    provided_solution <- result_from_code() #tryCatch(eval(parse(text = isolate(input$code))), error = function(e) return(e))

    # prevent accidentally removing items by setting them to NULL, with toString()
    if(is.null(provided_solution)) provided_solution <- ""
    global$answers_result[[global$question_nr]] = provided_solution
    global$is_answer_correct[[global$question_nr]] = identical(correct_solution, provided_solution)

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
      
    }
  })

  output$uiAce <- renderUI({
      aceEditor("code", mode = "r", height = "200px", value = "1")
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

  result_from_code <- reactive({
  
    if(is.null(input$eval)) return("")
    if(!input$eval) return("")

    global$is_r_plot <- grepl(pattern = "plot(", x = input$code, fixed = TRUE) #input$code
    
    if(global$is_r_plot){
      
      return(input$code)
      
    }

      
    isolate({

      no_code <- !nchar(input$code)
      if(no_code) return("")
      
      if(input$language == "R"){
        parsed_code <- tryCatch(eval(parse(text = isolate(input$code))), error = function(e) e)
        return(parsed_code)
      }
      
      if(input$language == "Javascript"){
        
        ctx <- v8()
        return(
          ctx$eval(
            isolate(
              input$code
            )
          )
        )
        
      }
      
      if(input$language == "Python"){
        
        isolate({
          # clear the cache - otherwise the assignment of the previous session might get displayed
          py_result_raw <- reticulate::py_run_string("result = None") 
          py_result_raw <- reticulate::py_run_string(input$code)
          py_result <- py_result_raw$result
          if(is.null(py_result)) py_result <- "Please assign your result to a variable called result. E.g. 'result = 314'." 
          return(py_result)
        })
        
      }
      
      if(input$language == "SQL"){
        
        isolate({
          
          data("airquality")
          conn <- dbConnect(RSQLite::SQLite(), "interviewr.db")
          dbWriteTable(conn, "airquality", airquality, overwrite = TRUE)
          sql_result <- dbGetQuery(conn, input$code)
          return(sql_result)
          
        })
        
      }
      
    })
      
  })
  
  observeEvent(input$eval, {

    is_r_plot <- grepl(pattern = "plot(", x = input$code, fixed = TRUE)    

    # toggle somehow isnt triggered if its not a plot code
    if(is_r_plot){
      
      shinyjs::hide(id = "console_output")
      shinyjs::show(id = "console_output2")
      
    }else{
      
      shinyjs::show(id = "console_output")
      shinyjs::hide(id = "console_output2")
      
    }

    
  }, priority = 99)
  
    
  observe({

    input$eval
    
    isolate({
    
      req(input$code)
      is_r_plot <- grepl(pattern = "plot(", x = input$code, fixed = TRUE)
      
      if(is_r_plot){
        
        output$console_output2 <- renderPlot({
          
          input$eval 
          
          isolate({
            
            code <- input$code
            if(is_r_plot){
              eval(parse(text = code))
              p <- recordPlot()
              plot.new()
              return(p)
            }
            
          })
        })
        
      }else{
        
        output$console_output <- renderPrint({
          
          input$eval 
          
          isolate({
            
            result <- result_from_code()
            req(nchar(result) > 0)
            print(result)
            
          })
          
        })
        
      }
        
    })
    
  })

  
})


shinyApp(ui, server) #, launch.browser = TRUE)  

