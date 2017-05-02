rm(list = ls())
library(shiny)

Logged = FALSE;


ui1 <- function(){
  tagList(
    div(id = "login",
      wellPanel(  titlePanel("Step 1: authorization")),
        wellPanel(textInput("userName", "Username"),
                  passwordInput("passwd", "Password"),
                  br(),actionButton("Login", "Log in"))),
    tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  )}

ui2 <- function(){ 
  tagList(
    div(id="genre",
    wellPanel(titlePanel("Step 2 :Genres")),
 sidebarLayout(sidebarPanel(checkboxGroupInput("genres", "Genres to choose:", c("Romance" = "romance",
                                                                               "Drama" = "drama",
                                                                               "Fantasy" = "fantasy",
                                                                                "Action"="action",
                                                                                "Comedy"="comedy"   ))), 
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

ui4<-function(){tagList(div( fluidRow(column(width = 4,renderDataTable({input$userName})),
                                      column(width = 4, offset = 3)),
                             fluidRow(column(width = 4))
) 
  
)}

ui = (htmlOutput("page"))


server = (function(input, output,session) {

  
  
  USER <- reactiveValues(Logged = Logged)
  
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
        #div(class="outer",do.call(navbarPage,c(ui2())))
      })
      
      
       observeEvent(input$button_in_genres,
                   {output$page<-renderUI({
                     
                     ui3()})}
        )
      
      
     output$userName<- renderDataTable({input$userNane})
      
      observeEvent(input$button_in_stories,{output$page<-renderUI({
        
        ui4()})
      })
      
      

      print(ui)
      

    }
  })


  })

runApp(list(ui = ui, server = server))
