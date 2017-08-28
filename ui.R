
#created by Anthony Rinaldo
library(shiny)
library(DT)


shinyUI(fluidPage(

  
  titlePanel("Schedule Generator"),

  
  sidebarLayout(
    sidebarPanel(
      HTML('<!-- made by Anthony Rinaldo -->'),
      textInput('subjCode','Class(eg. MA131,CS142,PH141 )',placeholder = 'subject letter'),
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
      
      tabsetPanel(
      tabPanel("List View",
        dataTableOutput('schedule'),
        conditionalPanel(
          "input.schedule_rows_selected != null",
          dataTableOutput('otherClas'),
          actionButton('swapClass','swap class')
        )
      ),
      tabPanel("Calendar",dataTableOutput('calendar'),numericInput("interval","Interval in Minutes",value = 15),
               downloadButton("schedDown"))
      )
      
      )
  )
))
