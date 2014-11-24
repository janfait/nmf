library(shiny)


shinyUI(bootstrapPage(
  
  #include anything that should land in the <head> section of the website
  tags$head( 
    
    #stemmark css and js
    HTML('<link rel="stylesheet" href="style.css" type="text/css" media="screen, projection">')
    
  ),
  
  #render login and content
  uiOutput("uiLogin"),
  uiOutput("pass"),
  uiOutput("content")
  
))
