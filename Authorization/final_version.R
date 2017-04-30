<<<<<<< HEAD
rm(list = ls())
library(shiny)

ui <- function()  {
  fluidPage(
    splitLayout(
      
    wellPanel(actionButton("button_left", "Log in"),
    tags$style(type="text/css", "#button {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
    )),
    wellPanel(actionButton("button_right", "Registration"),    
    tags$style(type="text/css", "#button {font-size:100px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
))}
  
  
server<-function(input, output){
  
  
  obs<-observe({
    
    if(input$button_left)
    {
      cat("just click add button")
      cat("test")
      print (input$button_left)
      output$button_right<-  renderUI({ 
        pageWithSidebar(h4("ha! change"))
      })
      
    }
    
  })  
  

 # renderPrint(output$button)
}

runApp(list(ui = ui, server = server))
            
=======
library(shiny)
>>>>>>> 697b9073e6355ca448b24c200f183b6543b2696a
