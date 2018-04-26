
library(data.table)
library(shiny)

shinyUI(fluidPage(
     
  textAreaInput("text", label = h3("Type a message"), value = " ", width='100%', height='100%'),
  uiOutput('suggest1'),
  uiOutput('suggest2'),
  uiOutput('suggest3')
  
))
