rm(list = ls())
library(shiny)

ui <- function()  {
  fluidPage(
 titlePanel("Step 1: authorization"),
    sidebarLayout(sidebarPanel(textInput("log-in", "Log-in", ""),passwordInput("password", "Password", ""), submitButton(text = "Go", icon = NULL)),
                  mainPanel(actionButton("button", "Registration"))
                  
                  
                  )
    
    
      # splitLayout(
      
    #wellPanel(actionButton("button_left", "Log in"),
    #tags$style(type="text/css", "#button {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
    #)),
    #wellPanel(actionButton("button_right", "Registration"),    
    #tags$style(type="text/css", "#button {font-size:100px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
)}
  
  
server<-function(input, output){
  

  }  
  


runApp(list(ui = ui, server = server))
            
