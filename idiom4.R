# Load packages ----
library(shiny)
library(ggplot2)
library(reshape2)
library("data.table")
library(tidyverse)
library(tidyr)
library(plotly)
library(ggthemes)
library (plyr)

myData <- read.csv("data/Unplanned_Hospital_Visits-State.csv")

States = unique(myData$State)
measureNames = unique(myData$Measure.Name)


# User interface ----
ui <- fluidPage(
  titlePanel("Comparing the return rate of patients of hospitals between states to the national return rate (for a selected procedure)"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select the states you want to compare and the measure you want to display"),
      selectInput("inputStates", 
                  "Select the state(s)",
                  choices = States, 
                  selected = c("AK", "AR", "CO"), 
                  multiple = TRUE),
      selectInput("varMeasure",
                  label = "Choose a measure to display",
                  choices = measureNames,
                  selected = "Hospital return days for heart attack patients"
                  )
    ),
    
    mainPanel(
      textOutput("measureChosen"),
      p("For the following states : "),
      textOutput("statesChosen"),
      plotlyOutput("hospitals")
    )
  )
)

# Server logic
server <- function(input, output) {

  df <- reactive({
    print(input$inputStates)
    myDf <- myData[myData$Measure.Name == input$varMeasure, ]
    
    myDf <- myDf[myDf$State  %in% input$inputStates, ]
    
    return(myDf)
  })
  
  output$measureChosen <- renderText({ 
    paste("You have selected : ", input$varMeasure)
  })
  
  output$statesChosen <- renderText({ 
    paste(input$inputStates)
  })
  
  output$hospitals <- renderPlotly({
    myDf <- df()
    
    if (myDf[1,]$Number.of.Hospitals.Worse != "Not Applicable") {
      chosenColumns <- c("State", "Number.of.Hospitals.Worse", "Number.of.Hospitals.Same", "Number.of.Hospitals.Better", "Number.of.Hospitals.Too.Few")
    } else {
      chosenColumns <- c("State", "Number.of.Hospitals.Fewer", "Number.of.Hospitals.Average", "Number.of.Hospitals.More", "Number.of.Hospitals.Too.Small")
    }
    
    df2 <- melt(myDf[, chosenColumns])
    setDT(df2)
    
    df_long <- df2 %>% 
      gather(categoryHospitals, value, -1)
    
    df_long <- df_long[df_long$value != "Not Available", ]

    df_long$value <- as.numeric(as.character(df_long$value))
    
    ggplotly(
      ggplot(df_long[order(df_long$categoryHospitals, decreasing = T),], aes(x = State, y = value, fill = categoryHospitals,group = categoryHospitals))
      + geom_bar(stat = "identity", position = "fill")
      + scale_y_continuous(labels = scales::percent)
      + labs(y= "Percent (%)", x = "States", fill = "Hospitals return rate compared \n to the national's")
      + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.title = element_text(size=8), legend.text = element_text(size=7))
    )
    
  })
  
}

# Run the app
shinyApp(ui, server)
