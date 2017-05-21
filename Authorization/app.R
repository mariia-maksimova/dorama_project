rm(list = ls())
library(shiny)
fields <- c("userName", "passwd", "genres", "stories")
Logged = FALSE;


ui1 <- function(){
  tagList(
    div(id = "login",
      wellPanel(  titlePanel("Step 1: authorization")),
        wellPanel(textInput("userName", "Username"),
                  passwordInput("passwd", "Password"),
                  br(),actionButton("Login", "Log in"))),
    tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  )
 
  }

ui2 <- function(){ 
  tagList(
    div(id="genre",
    wellPanel(titlePanel("Step 2 :Genres")),
 sidebarLayout(sidebarPanel(checkboxGroupInput("genres", "Genres to choose:", c("Romance" = "7",
                                                                               "Drama" = "6",
                                                                               "Fantasy" = "12",
                                                                                "Action"="4",
                                                                                "Comedy"="1"   ))), 
               actionButton(inputId = "button_in_genres",label = "Go", icon = NULL)))
  )
}


ui3<- function(){tagList(
  div(id="story",
      wellPanel(titlePanel("Step 3 :Stories")),
      sidebarLayout(sidebarPanel(checkboxGroupInput("stories", "Stories to choose:", c("Historical" = "historical",
                                                                                     "Cinderella Story" = "cinderella",
                                                                                     "Investigation" = "investigation",
                                                                                     "Secrets of the past"="secrets",
                                                                                     "Supernatural"="supernatural"))), 
                    actionButton(inputId = "button_in_stories",label = "Go", icon = NULL)))
) }

ui4<-function(){fluidPage(
  
      titlePanel("Step 4 : Choice making"),
     
      # wellPanel("Personal info",DT::dataTableOutput("responses")),
      
 
 
  wellPanel("",
                
                selectInput(inputId = 'in0', label = 'Choose doramas', 
                            choices = colnames(titles), 
                            multiple = TRUE, selectize = TRUE),
            uiOutput("variables")
                
                ))}
     # wellPanel("Blabla",DT::dataTableOutput("user_genres")) )
  
  
  #div( fluidRow(
                              ##column(width = 4,tableOutput("userinfo")),
          #                   column(width = 4, offset = 3)),
                             #fluidRow(column(width = 4))
  

ui = (htmlOutput("page"))



server = (function(input, output,session) {
  
  library(recommenderlab)
  library(dplyr)
  library(readr)
  library(registry)
  
  setwd("~/dorama_project")
  getwd()
  load('matrix_rates.RData')
  load('recc_model.RData')
  load('dramas_matrix.RData')
  load('KR_genres.RData')
  load('KR_rates.RData')
  load('KR_rating_full.RData')
  load('KR_shows.RData')
  
  

  saveData <- function(data) {
    data <- as.data.frame(t(data))
    if (exists("responses")) {
      responses <<- rbind(responses, data)
    } else {
      responses <<- data
    }
  }
  
  loadData <- function() {
    if (exists("responses")) {
      responses
    }
  }
  
  
  USER <- reactiveValues(Logged = Logged)
  values<-reactiveValues()
  observe({ 
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Id.username <- which(input$userName== Username)
          Id.password <- which(input$passwd == Password)
          if (length(Id.username) > 0 & length(Id.password) > 0) {
            if (Id.username == Id.password) {
              USER$Logged <- TRUE
            } 
          }
        } 
      }
    }    
  })
  observe({
    if (USER$Logged == FALSE) {
      
      output$page <- renderUI({
        div(class="outer",do.call(bootstrapPage,c("",ui1())))
      })
    }
    if (USER$Logged == TRUE) 
    {
    #  newLine <- isolate(c(input$userName))
      output$page <- renderUI({ ui2()
        #div(class="outer",do.call(navbarPage,c(ui2())))
        
        })
      
      formData <- reactive({
        data <- sapply(fields, function(x) input[[x]])
        data
        })
      
       observeEvent(input$button_in_genres,
      {#newLine<-rbind(newLine, isolate(c(input$genres)))
        
       #saveData(formData())
        
       user_genres <-as.data.frame(matrix(nrow=1, ncol=5))  
       
       user_genres<- t(input$genres)
      # user_genres[is.na(user_genres)] <- 0 
       
      
       output$user_genres<-DT::renderDataTable(user_genres)
       
       
       
      dramas_matrix_work<-filter(dramas_matrix, is.element(genre_id,user_genres)==TRUE)
      
       
      first_page_dramas<-data.frame(dorama_id=stack(dramas_matrix_work[,2:6])[,1])
      
      
      
      first_page_dramas2 <-filter(KR_shows, is.element(dorama_id,first_page_dramas$dorama_id)==TRUE )
      
      titles<<-as.data.frame(t(first_page_dramas2$dorama_title))
      colnames(titles) <<- as.character(unlist(titles[1,]))
      
      
      
      
      
      #output$first_page_dramas2<-DT::renderDataTable(first_page_dramas2)
      
       
      for (i in first_page_dramas2) {y<-split( first_page_dramas2, f=first_page_dramas2$dorama_title)
      }
      
      
      for (i in seq(y))
      {  assign(paste("df", i, sep = ""), y[[i]])}
      
      output$variables <- renderUI({
        numVar <- length(as.integer(input$in0))
      
      lapply(input$in0, function(x) {
        list(radioButtons(paste0("dynamic",x), x, 
                          choices = c("Not watch" = "0",
                                      "Terrible" = "1", 
                                      "Awful" = "2",
                                      "Normal" = "3", 
                                      "Excellent" = "4",
                                      "Perfect" = "5"
                                      ), selected = "one"))
      })
      })
      
       #user vector (doramas)
       
       user_data<-matrix(data=NA, ncol = ncol(matrix_rates))
       colnames(user_data)<-colnames(matrix_rates)
       user_data<-t(user_data)
       
       
       
       
       
       
       
        output$page<-renderUI({
                     
                     ui3()})}
        )
      
      
       
    # output$userinfo<- renderDataTable({list(input$userName, input$genres)})
      
       
       
      observeEvent(input$button_in_stories,
     {#newLine<-rbind(newLine, input$stories)
       
       saveData(formData())
       output$page<-renderUI({
      ui4()})
      })
      
      
      output$responses <- DT::renderDataTable({
        input$button_in_stories
        loadData()
      })     
      
      
     # output$info<- renderDataTable(isolate(c(input$userName, input$genres)))
      
      print(ui)
      

    }
  })


  })

runApp(list(ui = ui, server = server))
