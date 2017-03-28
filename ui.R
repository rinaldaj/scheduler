
#created by Anthony Rinaldo
library(shiny)
library(DT)


shinyUI(fluidPage(

  
  titlePanel("Schedule Generator"),

  
  sidebarLayout(
    sidebarPanel(
      HTML('<!-- made by Anthony Rinaldo -->'),
      textInput('subjCode','Class(eg. MA131,CS142,PH141 )',placeholder = 'subject letter'),
      #numericInput('courseNum','The course number (eg. 131,132...)',value = 0),
      actionButton('addClass','add class'),
      dataTableOutput('classDB'),
      fluidRow(
      checkboxInput('full','use full classes in schedule')
      ),
      fluidRow(
       actionButton('generSch','Generate schedule'),
       actionButton('rmClass','Remove Class')
      ),
	singleton(tags$head(tags$script(src = "message-handler.js"))
	)
    ),

    
    mainPanel(
      dataTableOutput('schedule'),
      actionButton('swapClass','swap class')
    )
  )
))
