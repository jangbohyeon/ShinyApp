library(shiny)
library(tidyverse)
library(shinythemes)
library(dplyr)

#Sys.setlocale("LC_ALL","korean")
ui<- fluidPage(
  titlePanel('homework2 : Shiny App'),
  sidebarPanel(
    h6("Data extracted only 30 out of 360 data"),
    h6("Variable d is the file path. Select other variables except d."),
    selectInput(inputId = 'y',
                 label = 'Please select a column.',
                 choices = cols),
    submitButton(text='Apply the changes.',
                 icon = icon(name='sync'))),
  mainPanel(
    uiOutput(outputId = 'mainUI')
  )
)

server <- function(input, output, session){
  output$boxplot <- renderPlot({
    ggplot(data=test, mapping=aes(y = !!input$vars[[1]]))+
      geom_boxplot() + theme_bw()
  })
  observe({
    cols <- colnames(x=summary_peak_changepoint)
    updateSelectInput(session=session, inputId = 'y', choices = cols)
  })
  output$Plot1 <- renderPlot({
    ggplot(data=summary_peak_changepoint, aes(x=1:length(d), y=summary_peak_changepoint[,!!input$y]))+
      geom_line()
  })
  output$Plot2 <- renderPlot({
    ggplot(data=summary_peak_changepoint, aes(x=1:length(d), y=summary_peak_changepoint[,!!input$y]))+
      geom_line() + facet_wrap(.~activity,nrow=3)
  })
  
  output$mytable <- DT::renderDataTable({
    summary_peak_changepoint
  })
  output$myML1 <- renderPrint({
    e_RF
  })
  output$myML2 <- renderPrint({
    e_J48
  })
  output$mainUI<-renderUI({
    tabsetPanel(
      type = 'tabs',
      tabPanel(title = 'Data',
               DT::dataTableOutput(outputId='mytable')),
      tabPanel(title = 'DataML',
               h6('e_RF Results'),
               verbatimTextOutput(outputId="myML1"),
               h6('e_J48 Results'),
               verbatimTextOutput(outputId="myML2")),
      tabPanel(title = 'Plot',
               h6('Graph'),
               plotOutput(outputId='Plot1'),
               h6('Graph by activity'),
               plotOutput(outputId='Plot2'))
    )
  })
}

shinyApp(ui=ui, server=server)

#setwd('C:/Users/hansung/Desktop/shiny')
#save.image("20171489.RData")
#load("20171489.RData")
