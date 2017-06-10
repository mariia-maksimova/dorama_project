rm(list = ls())
library(shiny)
fields <- c("userName", "passwd")
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
        sidebarLayout(sidebarPanel(checkboxGroupInput("genres", "Genres to choose:", c("Drama" = "6",
                                                                                       "Romance/Dating" = "7",
                                                                                       "Comedy" = "1",
                                                                                       "Family"="10",
                                                                                       "Action"="4",
                                                                                       "Celebrities"="37",
                                                                                       "History"="45",
                                                                                       "Thriller"="26",
                                                                                       "Music"="32",
                                                                                       "Fantasy"="12"
        ))), 
        actionButton(inputId = "button_in_genres",label = "Go", icon = NULL)))
  )
}



ui3<-function(){fluidPage(
  
  titlePanel("Step 3 : Choice making"),
  
  
  
  
  wellPanel("",
            
            selectInput(inputId = "in0", label = 'Choose doramas', 
                        choices = titles2, 
                        multiple = TRUE, selectize = TRUE),
            uiOutput("variables"),
            wellPanel("", actionButton(inputId = "button_in_choice",label = "Go", icon = NULL)),
            
            wellPanel("Personal info",textOutput("dynamic"))
            
            
  ))}



ui4<- function(){fluidPage(
  titlePanel("Step 4 :Stories"),
  fluidRow(
    column(2, 
           sliderInput("History", "History", min=0, max=10, value=0),
           sliderInput("Cinderella", "Cinderella", min=0, max=10, value=0),           
           sliderInput("Investigation", "Investigation", min=0, max=10, value=0),
           sliderInput("School", "School", min=0, max=10, value=0),
           sliderInput("Gender", "Gender", min=0, max=10, value=0)),
    
  column(2,
             sliderInput("Super", "Supernatural", min=0, max=10, value=0),
             sliderInput("Pro", "Professional", min=0, max=10, value=0),
             sliderInput("Revenge", "Revenge", min=0, max=10, value=0),
             sliderInput("Friendship", "Friendship", min=0, max=10, value=0),
             sliderInput("Secrets", "Secrets of the past", min=0, max=10, value=0)),

  column(8,
         DT::dataTableOutput("table")
  ))
  
  
  
  
) }


ui = (htmlOutput("page"))



server = (function(input, output,session) {
  
  library(recommenderlab)
  library(dplyr)
  library(readr)
  library(registry)
  
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
      
      output$page <- renderUI({ ui2()
        
        
      })
      
      formData <- reactive({
        data <- sapply(fields, function(x) input[[x]])
        data
      })
      
      observeEvent(input$button_in_genres,
                   {
                     
                     
                     user_genres <-as.data.frame(matrix(nrow=1, ncol=5))  
                     
                     user_genres<- t(input$genres)
                     # user_genres[is.na(user_genres)] <- 0 
                     
                     
                     output$user_genres<-DT::renderDataTable(user_genres)
                     
                     
                     
                     dramas_matrix_work<-filter(dramas_matrix, is.element(genre_id,user_genres)==TRUE)
                     
                     
                     first_page_dramas<-data.frame(dorama_id=stack(dramas_matrix_work[,2:6])[,1])
                     
                     
                     
                     first_page_dramas2 <-filter(KR_shows, is.element(dorama_id,first_page_dramas$dorama_id)==TRUE )
                     
                     #titles<<-as.data.frame(t(first_page_dramas2))
                     #colnames(titles) <<- as.character(unlist(titles[2,]))
                     
                     titles2<<-as.list(first_page_dramas2$dorama_id)
                     names(titles2)<<-as.character(first_page_dramas2$dorama_title)
                     
                     
                     
                     
                     
                     #output$first_page_dramas2<-DT::renderDataTable(first_page_dramas2)
                     
                     
                 #    for (i in first_page_dramas2) {y<-split( first_page_dramas2, f=first_page_dramas2$dorama_title)
                #     }
                 #    
                     
                  #   for (i in seq(y))
                   #  {  assign(paste("df", i, sep = ""), y[[i]])}
                     
                     
                     
                     user_data<<-matrix(data=NA, ncol = ncol(matrix_rates))
                     colnames(user_data)<<-colnames(matrix_rates)
                     #user_data<<-t(user_data)
                     
                     
                     output$variables <- renderUI({
                       numVar <- length(as.integer(input$in0))
                       
                       lapply(input$in0, function(x) {
                         list(radioButtons(paste0("dynamic",x), first_page_dramas2$dorama_title[first_page_dramas2$dorama_id==x], 
                                           choices = c("Not watch" = "NA",
                                                       "Terrible" = "1", 
                                                       "Awful" = "2",
                                                       "Normal" = "3", 
                                                       "Excellent" = "4",
                                                       "Perfect" = "5"),
                                           selected = "one")
                                   )
                         
                         
                         
                       })
                       
                       
                     })
                     
                     
                     
                     
                     
                     output$page<-renderUI({
                       
                       ui3()})}
      )
      
      
      
      # output$userinfo<- renderDataTable({list(input$userName, input$genres)})
      
      
      
      observeEvent(input$button_in_genres,
                   {
                     
                     saveData(formData())
                     output$page<-renderUI({
                       ui3()})
                   })
      
      
      output$responses <- DT::renderDataTable({
        input$button_in_genres
        loadData()
      })     
      
      
      observeEvent(input$button_in_choice,
                  {
                
                 
                    
                 for (var in titles2){
                  if(is.null(input[[paste0("dynamic", var)]])==FALSE){
                  user_data[colnames(user_data)==as.character(var)] <<- as.numeric(input[[paste0("dynamic", var)]])
                  }
                 }
                    
                 
                 
                 
        
                 # for (x in titles2){
                  #  if (input$paste0("dynamic",x) != ""){
                   # user_data[colnames(user_data)==as.character(x)]=input$paste0("dynamic",x)}
                     
                  # }
                     
                   
                   #
                   #%   for (i in ncol(titles)){
                   #%  user_data[rownames(user_data)==i]<-output$paste("dynamic",i, sep = "")
                   #%  }
                   
                   
                   #user vector (doramas)
                   
                   
                   # user_data<-t(user_data)
                  #user_data<- sapply(user_data, as.numeric)
                  user_data<-as(user_data,"realRatingMatrix")
                  
                  recc_predicted <- predict(object = recc_model, newdata = user_data, n = 700)
                  recc_user_1 <- recc_predicted@items[[1]]
                  recc_user_ratings<-recc_predicted@ratings[[1]]
                  
                  dorama_id<- sapply( recc_predicted@itemLabels[recc_user_1], as.integer)
                  dorama_ids<-cbind(dorama_id, data.frame(rating=recc_user_ratings))
                  
                  KR_rating_full$id=as.numeric(rownames(KR_rating_full))
                  
                  dramas=inner_join(dorama_ids, KR_rating_full, by='dorama_id') 
                  dramas<-dramas[order(dramas$rating, decreasing = TRUE),]
                  
                # dramas2<-select(dramas, dorama_title, dorama_myshows)
                  
                  filter_dramas <- reactive({
                    
                    school <- as.numeric(input$School)
                    history <- as.numeric(input$History)
                    gender <- as.numeric(input$Gender)
                    super <- as.numeric(input$Super)
                    pro <- as.numeric(input$Pro)
                    investigation <- as.numeric(input$Investigation)
                    cinderella <- as.numeric(input$Cinderella)
                    revenge <- as.numeric(input$Revenge)
                    friendship <- as.numeric(input$Friendship)
                    secrets <- as.numeric(input$Secrets)
                    
                    dramas2 <<- dramas %>%
                      filter(
                        School <= school+4,
                        School >= school-4,
                        History <= history+4, 
                        History >= history-4,
                        Gender <= gender+4,
                        Gender >= gender-4,
                        Super <= super+4,
                        Super >= super-4,
                        Pro <= pro+4,
                        Pro >= pro-4,
                        Investigation <= investigation+4,
                        Investigation >= investigation-4,
                        Cinderella <= cinderella+4,
                        Cinderella >= cinderella-4,
                        Revenge <= revenge+4,
                        Revenge >= revenge-4,
                        Friendship <= friendship+4,
                        Friendship >= friendship-4,
                        Secrets <= secrets+4,
                        Secrets >= secrets-4
                      ) 

                    
                  
                     dramas2<<-select(dramas2, dorama_title, dorama_myshows)
                    
                    
                  })
                  
                  output$table<-DT::renderDataTable({ filter_dramas() })
                  
                  output$page<-renderUI({ui4()})
                   
                   })
      
      print(ui)
      
      
    }
  })
  
  
})

