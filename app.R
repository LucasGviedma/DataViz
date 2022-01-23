library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinycssloaders)
library(maps)
library(mapproj)
library(ggplot2)
library(shinyWidgets)
library(plotly) #interactive graphs
library(shinyBS) #modals
library(dplyr)
library("data.table")
library(tidyverse)
library(tidyr)

options(warn = -1)

suppressPackageStartupMessages(library(circlize))

source("helpers_idiom1.R")
source("helpers_idiom2.R")

source("ui_idiom1.R")
source("ui_idiom2.R")


# Load data ----
hospital_data <- read.csv("data/Unplanned_Hospital_Visits-Hospital.csv")
hospital_data_state <- read.csv("data/Unplanned_Hospital_Visits-State.csv")


# Idiom 1 processing
id1_df<- subset(hospital_data, select=c(State, County.Name, Measure.Name, Compared.to.National, Score, Denominator))
id1_df<- id1_cleaning(id1_df)

id1_df$Score       <- as.numeric(id1_df$Score)
id1_df$Denominator <- as.numeric(id1_df$Denominator)

id1_missing_counties <- subset(read.csv("aux_missing_counties.csv"), select=c(Location, mean_score))  
id1_missing_counties <- transform(id1_missing_counties, mean_score = as.numeric(mean_score))

id1_df$State    <- sapply(id1_df$State, id1_state_to_name)
id1_df$Location <- paste(id1_df$State, id1_df$County.Name, sep=",")

id1_df <- subset(id1_df, select=c(Location, Measure.Name, Compared.to.National, Score, Denominator))
id1_df <- id1_df[with(id1_df, order(Location)), ]

id1_counties     <- map_data("county")
id1_counties$loc <- paste(id1_counties$region, id1_counties$subregion, sep=",")
id1_counties     <- id1_counties[with(id1_counties, order(loc)), ]
id1_fill_pattern <- count(group_by(id1_counties, loc))$n



# Idiom 2 processing
data <- subset(hospital_data)
  
id2_facilities <- sort(unique(data$Facility.Name))
id2_states     <- sort(unique(data$State))
id2_measures   <- sort(unique(data$Measure.Name))
id2_counties   <- sort(unique(data[data$State == "AK", "County.Name"]))

id2_del_states <- c('AK', 'AS', 'DC', 'GU', 'HI', 'MP','NC', 'ND', 'NY',
                'PR')

id2_measure_names  <- levels(factor(data$Measure.Name))
id2_states         <- levels(factor(data$State))
id2_states_choices <- append(list("--"), as.list(id2_states))
id2_states_choices <- id2_states_choices[!(id2_states_choices %in% id2_del_states)]



# Idiom 3 processing
id3_df <- unique(hospital_data[,c(2, 10, 13)]) # select columns of interest

as.numeric(as.character(id3_df$Score))
id3_df <- id3_df[!is.na(as.numeric(as.character(id3_df$Score))),]

id3_facility.Names <- unique(id3_df$Facility.Name)

id3_measure.Names <- unique(id3_df$Measure.Name)

# Idiom 4 processing
id4_data <- subset(hospital_data_state)
id4_states = unique(id4_data$State)
id4_measureNames = unique(id4_data$Measure.Name)



ui <- navbarPage(
  
  "Hospital Data Analytics",

  tabPanel("Idiom 1",fluidPage(theme = shinytheme("cerulean")),
           tags$head(
             tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
             id1_ui),

  tabPanel("Idiom 2",fluidPage(theme = shinytheme("cerulean")),
           tags$head(
             tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
             id2_ui),

  tabPanel("Idiom 3",fluidPage(theme = shinytheme("cerulean")),
           tags$head(
             tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
           fluidPage(
             theme = bslib::bs_theme(bootswatch = "cerulean", font_scale = 0.8),
             titlePanel("Dot Chart Comparing Measure scores for different hospitals"),
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("hospital1", "Hospital Name", NULL, options = list(maxItems = 10, placeholder = 'Choose one or more hospitals'), selected = NULL)
               ),
               mainPanel(
                 plotlyOutput("plot", height = 700),
                 textInput("filename", "File name:", placeholder = "My Plot"),
                 radioButtons("format", "Choose format", c("png", "bmp", "pdf"), inline = TRUE),
                 downloadButton("download", "Download Chart")
               )
             ),
           )),
  
  tabPanel("Idiom 4",fluidPage(theme = shinytheme("cerulean")),
           tags$head(
             tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
             fluidPage(
               titlePanel("Comparing the return rate of patients of hospitals between states to the national return rate (for a selected procedure)"),
               
               sidebarLayout(
                 sidebarPanel(
                   helpText("Select the states you want to compare and the measure you want to display"),
                   selectInput("inputStates", 
                               "Select the state(s)",
                               choices = id4_states, 
                               selected = c("AK", "AR", "CO"), 
                               multiple = TRUE),
                   selectInput("varMeasure",
                               label = "Choose a measure to display",
                               choices = id4_measureNames,
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
  )
)

# Server logic ----
server <- function(input, output, session) {

  # Idiom 1 server logic
  output$id1_map <- renderPlot({
    
    map_data <- id1_mean_score_county(id1_df, input$id1_variable_selection_input, input$id1_format_selection_input, id1_missing_counties)
    
    id1_hospital_scores_distribution_map(input$id1_variable_selection_input, input$id1_format_selection_input, map_data, id1_counties, id1_fill_pattern)
    
  }, width = 800, height = 550)
  
  
  # Idiom 2 server logic
  output$states_selector <- renderUI({
    div(
      title = "Use this option to select a state",
      selectInput(
        "states_selector",
        "Select State:",
        choices = id2_states,
        selected = 1
      )
    )
  })
  output$facility_selector <- renderUI({
    div(
      title = "use this option to select an Hospital",
      
      selectInput(
        "facility_selector",
        "Select facility:",
        choices = id2_facilities,
        selected = 1
      )
    )
  })
  output$facility_selector_2 <- renderUI({
    div(
      title = "use this option to select an Hospital",
      
      selectInput(
        "facility_selector_2",
        "Select facility:",
        choices = id2_facilities,
        selected = 1
      )
    )
  })
  output$county_selector <- renderUI({
    counties <-
      sort(unique(data[data$State == input$states_selector_2, "County.Name"]))
    div(
      title = "use this option to select a county",
      selectInput(
        "states_selector_2",
        "Select State:",
        choices = id2_states,
        selected = ifelse(
          !is.null(input$states_selector_2),
          input$states_selector_2,
          1
        )
      ),
      selectInput(
        "county_selector",
        "Select county:",
        choices = id2_counties,
        selected = 1
      )
    )
  })
  output$states_selector_3 <- renderUI({
    div(
      title = "use this option to select a state",
      
      selectInput(
        "states_selector_3",
        "Select a state to compare:",
        choices = id2_states,
        selected = 1
      )
    )
  })
  output$county_selector_2 <- renderUI({
    counties <-
      sort(unique(data[data$State == input$states_selector_4, "County.Name"]))
    div(
      title = "use this option to select a county",
      selectInput(
        "states_selector_4",
        "Select State:",
        choices = id2_states,
        selected = ifelse(
          !is.null(input$states_selector_4),
          input$states_selector_4,
          1
        )
      ),
      selectInput(
        "county_selector_3",
        "Select county:",
        choices = id2_counties,
        selected = 1
      )
    )
  })
  
  
  
  make_chart_subtitle<-function(){
    comp_area <-ifelse(!is.null(input$comp_area),input$comp_area,1)
    comparation_selector <-ifelse(!is.null(input$comparation_selector),input$comparation_selector,1)
    case_when(
      comp_area==1 & comparation_selector==1 ~ paste0("Average state indicator compared against the nation"),
      comp_area==1 & comparation_selector==2 ~ paste0("Average state indicator compared against the state ", input$states_selector_3),
      comp_area==1 & comparation_selector==3 ~ paste0("Average state indicator compared against the county ", input$county_selector_3, " of", input$states_selector_4),
      comp_area==1 & comparation_selector==4 ~ paste0("Average state indicator compared against the facility ", input$facility_selector_2),
      comp_area==2 & comparation_selector==1 ~ paste0("Average County indicator compared against the nation"),
      comp_area==2 & comparation_selector==2 ~ paste0("Average County indicator compared against the state ", input$states_selector_3),
      comp_area==2 & comparation_selector==3 ~ paste0("Average County indicator compared against the county ", input$county_selector_3, " of", input$states_selector_4),
      comp_area==2 & comparation_selector==4 ~ paste0("Average County indicator compared against the facility ", input$facility_selector_2),
      comp_area==3 & comparation_selector==1 ~ paste0("Facility indicator compared against the nation"),
      comp_area==3 & comparation_selector==2 ~ paste0("Facility indicator compared against the state ", input$states_selector_3),
      comp_area==3 & comparation_selector==3 ~ paste0("Facility indicator compared against the county ", input$county_selector_3, " of", input$states_selector_4),
      comp_area==3 & comparation_selector==4 ~ paste0("Facility indicator compared against the facility ", input$facility_selector_2)
    )
  }
  output$chart_title<- renderText(paste0(input$indicator_selector))
  output$chart_subtitle<-renderText({make_chart_subtitle()})
  #output$location_info<-renderText(paste0(input&facility_selector," Located in ", data[data$]))
  bar_chart_data <-reactive({
    if(input$comp_area ==1 ){ #states
      
      if(grepl("Rate", input$indicator_selector, fixed = TRUE)) {
        
        bar_chart <-
          data %>% subset(
            Measure.Name == input$indicator_selector &
              Score != "Not Available",
            select = c(State, Score)
          )%>% transform(Score = as.numeric(Score))%>% group_by(State) %>% summarize(avg_score = mean(Score))%>% rename(values=avg_score,categories=State)%>%arrange(desc(values))%>%as.data.frame()
        
        
        
        if(input$comparation_selector ==1){#national
          mean_nat_val<-mean(bar_chart$values)
          
          bar_chart$comp <-ifelse(bar_chart$values<mean_nat_val,"Better than the national indicator",
                                  ifelse(bar_chart$values==mean_nat_val,"No different than the national indicator",
                                         ifelse(bar_chart$values>mean_nat_val,"Worse than the national indicator","")))
        }else if(input$comparation_selector ==2){ #state
          state_data = data%>%subset(
            Measure.Name ==input$indicator_selector &
              State == ifelse(!is.null(input$states_selector_3),input$states_selector_3,"AK")&
              Score != "Not Available",
            select = c(Facility.Name, Score))%>% transform(Score = as.numeric(Score))
          mean_state = mean(state_data$Score)
          
          bar_chart$comp <-ifelse(bar_chart$values<mean_state,"Better Than the State Rate",
                                  ifelse(bar_chart$values==mean_state,"No Different Than the State Rate",
                                         ifelse(bar_chart$values>mean_state,"Worse Than the State Rate","")))
          
        }else if(input$comparation_selector ==3){#county
          county_data = data%>%subset(
            Measure.Name == input$indicator_selector &
              State == input$states_selector_4&
              County.Name == input$county_selector_3&
              Score != "Not Available",
            select = c(Facility.Name, Score))%>% transform(Score = as.numeric(Score))
          mean_county = mean(county_data$Score)
          bar_chart$comp <-ifelse(bar_chart$values<mean_county,"Better Than the County Rate",
                                  ifelse(bar_chart$values==mean_county,"No Different Than the County Rate",
                                         ifelse(bar_chart$values>mean_county,"Worse Than the County Rate","")))
        } else if(input$comparation_selector == 4){#Facility
          facility<-ifelse(!is.null(input$facility_selector_2),input$facility_selector_2, "SOUTHEAST ALABAMA MEDICAL CENTER")
          facility_score <- data%>%subset(Facility.Name==facility&Measure.Name==input$indicator_selector,select=Score)%>% transform(Score = as.numeric(Score))
          
          bar_chart$comp <-ifelse(bar_chart$values<mean(facility_score$Score),"Better Than the Facility Rate",
                                  ifelse(bar_chart$values==mean(facility_score$Score),"No Different Than the Facility Rate",
                                         ifelse(bar_chart$values>mean(facility_score$Score),"Worse Than the Facility Rate","")))
        } else{
          
        }
        
      }
      else{
        bar_chart <-
          data %>% subset(
            Measure.Name == input$indicator_selector &
              Number.of.Patients.Returned != "Not Applicable",
            select = c(State, Number.of.Patients.Returned)
          )%>% transform(Number.of.Patients.Returned = as.numeric(Number.of.Patients.Returned))%>% group_by(State) %>% summarize(avg_patients = mean(Number.of.Patients.Returned))%>%rename(values=avg_patients, categories=State) %>%arrange(desc(values)) %>%as.data.frame()
        
        
        if(input$comparation_selector ==1){#national
          mean_nat_val<-mean(bar_chart$values)
          
          bar_chart$comp <-ifelse(bar_chart$values<mean_nat_val,"Better than the national indicator",
                                  ifelse(bar_chart$values==mean_nat_val,"No different than the national indicator",
                                         ifelse(bar_chart$values>mean_nat_val,"Worse than the national indicator","")))
        }else if(input$comparation_selector ==2){ #state
          state_data = data%>%subset(
            Measure.Name == input$indicator_selector &
              State == input$states_selector_3&
              Number.of.Patients.Returned != "Not Applicable",
            select = c(Facility.Name, Number.of.Patients.Returned))%>%transform(Number.of.Patients.Returned = as.numeric(Number.of.Patients.Returned))
          mean_state = mean(state_data$Number.of.Patients.Returned)
          
          bar_chart$comp <-ifelse(bar_chart$values<mean_state,"Better Than the state indicator",
                                  ifelse(bar_chart$values==mean_state,"No different than the state indicator",
                                         ifelse(bar_chart$values>mean_state,"Worse than the state indicator","")))
          
        }else if(input$comparation_selector ==3){#county
          county_data = data%>%subset(
            Measure.Name == input$indicator_selector &
              State == input$states_selector_4&
              County.Name == input$county_selector_3&
              Number.of.Patients.Returned != "Not Applicable",
            select = c(Facility.Name, Number.of.Patients.Returned))%>% transform(Number.of.Patients.Returned = as.numeric(Number.of.Patients.Returned))
          mean_county = mean(county_data$Number.of.Patients.Returned)
          bar_chart$comp <-ifelse(bar_chart$values<mean_county,"Better than the county indicator",
                                  ifelse(bar_chart$values==mean_county,"No different than the county indicator",
                                         ifelse(bar_chart$values>mean_county,"Worse than the county indicator","")))
        } else if(input$comparation_selector == 4){#Facility
          facility<-ifelse(!is.null(input$facility_selector_2),input$facility_selector_2, "SOUTHEAST ALABAMA MEDICAL CENTER")
          facility_score <- data[data$Facility.Name==facility,"Number.of.Patients.Returned"]
          
          bar_chart$comp <-ifelse(bar_chart$avg_score<facility_score,"Better Than the Facility indicator",
                                  ifelse(bar_chart$avg_score==facility_score,"No Different Than the Facility indicator",
                                         ifelse(bar_chart$avg_score>facility_score,"Worse Than the Facility indicator","")))
        } else{
          
        }
        
      }
      
      
    }else if(input$comp_area == 2){ #by county
      if(grepl("Rate", input$indicator_selector, fixed = TRUE)) {
        
        bar_chart <-
          data %>% subset(
            Measure.Name == input$indicator_selector &
              State==input$states_selector_2&
              County.Name== input$county_selector&
              Score != "Not Available",
            select = c(State, County.Name, Score, Facility.Name)
          )%>% transform(Score = as.numeric(Score))%>%rename(values=Score,categories=Facility.Name)%>%arrange(desc(values))%>% as.data.frame()
        
        
        if(input$comparation_selector ==1){#national
          mean_nat_val<-mean(bar_chart$values)
          
          bar_chart$comp <-ifelse(bar_chart$values<mean_nat_val,"Better Than the National Indicator",
                                  ifelse(bar_chart$values==mean_nat_val,"No Different Than the National Indicator",
                                         ifelse(bar_chart$values>mean_nat_val,"Worse Than the National Indicator","")))
        }else if(input$comparation_selector ==2){ #state
          state_data = data%>%subset(
            Measure.Name == input$indicator_selector &
              State == input$states_selector_3&
              Score != "Not Available",
            select = c(Facility.Name, Score))%>%transform(Score = as.numeric(Score))
          mean_state = mean(state_data$Score)
          
          bar_chart$comp <-ifelse(bar_chart$values<mean_state,"Better Than the State Indicator",
                                  ifelse(bar_chart$values==mean_state,"No Different Than the State Indicator",
                                         ifelse(bar_chart$values>mean_state,"Worse Than the State Indicator","")))
          
        }else if(input$comparation_selector ==3){#county
          county_data = data%>%subset(
            Measure.Name == input$indicator_selector &
              State == input$states_selector_4&
              County.Name == input$county_selector_3&
              Score != "Not Available",
            select = c(Facility.Name, Score))%>%transform(Score = as.numeric(Score))
          mean_county = mean(county_data$Score)
          bar_chart$comp <-ifelse(bar_chart$values<mean_county,"Better Than the County Indicator",
                                  ifelse(bar_chart$values==mean_county,"No Different Than the County Indicator",
                                         ifelse(bar_chart$values>mean_county,"Worse Than the County Indicator","")))
        } else if(input$comparation_selector == 4){#Facility
          facility<-ifelse(!is.null(input$facility_selector_2),input$facility_selector_2, "SOUTHEAST ALABAMA MEDICAL CENTER")
          facility_score <- data%>%subset(Facility.Name==facility&Measure.Name==input$indicator_selector,select=Score)%>% transform(Score = as.numeric(Score))
          
          bar_chart$comp <-ifelse(bar_chart$values<mean(facility_score$Score),"Better Than the Facility Indicator",
                                  ifelse(bar_chart$values==mean(facility_score$Score),"No Different Than the Facility Indicator",
                                         ifelse(bar_chart$values>mean(facility_score$Score),"Worse Than the Facility Indicator","")))
        } else{
          
        }
        
        
      }
      else{
        bar_chart <-
          data %>% subset(
            Measure.Name == input$indicator_selector &
              Number.of.Patients.Returned != "Not Applicable",
            select = c(State, Number.of.Patients.Returned)
          )%>% transform(Number.of.Patients.Returned = as.numeric(Number.of.Patients.Returned))%>% group_by(State) %>% summarize(avg_patients = mean(Number.of.Patients.Returned))%>%rename(values=avg_patients,categories=State)%>%arrange(desc(values))%>%as.data.frame()
        
        
        if(input$comparation_selector ==1){#national
          mean_nat_val<-mean(bar_chart$values)
          
          bar_chart$comp <-ifelse(bar_chart$values<mean_nat_val,"Better Than the National Indicator",
                                  ifelse(bar_chart$values==mean_nat_val,"No Different Than the National Indicator",
                                         ifelse(bar_chart$values>mean_nat_val,"Worse Than the National Indicator","")))
        }else if(input$comparation_selector ==2){ #state
          state_data = data%>%subset(
            Measure.Name == input$indicator_selector &
              State == input$states_selector_3&
              Number.of.Patients.Returned != "Not Applicable",
            select = c(Facility.Name, Number.of.Patients.Returned))%>%transform(Number.of.Patients.Returned = as.numeric(Number.of.Patients.Returned))
          mean_state = mean(state_data$Number.of.Patients.Returned)
          
          bar_chart$comp <-ifelse(bar_chart$values<mean_state,"Better Than the State Indicator",
                                  ifelse(bar_chart$values==mean_state,"No Different Than the State Indicator",
                                         ifelse(bar_chart$values>mean_state,"Worse Than the State Indicator","")))
          
        }else if(input$comparation_selector ==3){#county
          county_data = data%>%subset(
            Measure.Name == input$indicator_selector &
              State == input$states_selector_4&
              County.Name == input$county_selector_3&
              Number.of.Patients.Returned != "Not Applicable",
            select = c(Facility.Name, Number.of.Patients.Returned))%>%transform(Number.of.Patients.Returned = as.numeric(Number.of.Patients.Returned))
          mean_county = mean(county_data$Number.of.Patients.Returned)
          bar_chart$comp <-ifelse(bar_chart$values<mean_county,"Better Than the County Indicator",
                                  ifelse(bar_chart$values==mean_county,"No Different Than the County Indicator",
                                         ifelse(bar_chart$values>mean_county,"Worse Than the County Indicator","")))
        } else if(input$comparation_selector == 4){#Facility
          facility<-ifelse(!is.null(input$facility_selector_2),input$facility_selector_2, "SOUTHEAST ALABAMA MEDICAL CENTER")
          facility_score <- data[data$Facility.Name==facility&data$Measure.Name==input$indicator_selector,"Number.of.Patients.Returned"]%>%transform(Number.of.Patients.Returned = as.numeric(Number.of.Patients.Returned))
          bar_chart$comp <-ifelse(bar_chart$values<facility_score,"Better Than the Facility Indicator",
                                  ifelse(bar_chart$values==facility_score,"No Different Than the Facility Indicator",
                                         ifelse(bar_chart$values>facility_score,"Worse Than the Facility Indicator","")))
        } else{
          
        }
        
      }
    }
    else if(input$comp_area == 3){ #by Facility
      if(grepl("Rate", input$indicator_selector, fixed = TRUE)) {
        
        county<- data[data$Facility.Name==input$facility_selector,"County.Name"]%>%head(1)
        
        bar_chart <-
          data %>% subset(
            Measure.Name == input$indicator_selector &
              County.Name == county&
              Score != "Not Available",
            select = c(State, County.Name, Score, Facility.Name, Compared.to.National)
          )%>% transform(Score = as.numeric(Score))%>%rename(values=Score,categories=Facility.Name)%>%arrange(desc(values))%>% as.data.frame()
        
        
        if(input$comparation_selector ==1){#national
          
          
          bar_chart$comp <-bar_chart$Compared.to.National
        }else if(input$comparation_selector ==2){ #state
          state_data = data%>%subset(
            Measure.Name == input$indicator_selector &
              State == input$states_selector_3&
              Score != "Not Available",
            select = c(Facility.Name, Score))%>%transform(Score = as.numeric(Score))
          mean_state = mean(state_data$Score)
          
          bar_chart$comp <-ifelse(bar_chart$values<mean_state,"Better Than the State Indicator",
                                  ifelse(bar_chart$values==mean_state,"No Different Than the State Indicator",
                                         ifelse(bar_chart$values>mean_state,"Worse Than the State Indicator","")))
          
        }else if(input$comparation_selector ==3){#county
          county_data = data%>%subset(
            Measure.Name == input$indicator_selector &
              State == input$states_selector_4&
              County.Name == input$county_selector_3&
              Score != "Not Available",
            select = c(Facility.Name, Score))%>%transform(Score = as.numeric(Score))
          mean_county = mean(county_data$Score)
          bar_chart$comp <-ifelse(bar_chart$values<mean_county,"Better Than the County Indicator",
                                  ifelse(bar_chart$values==mean_county,"No Different Than the County Indicator",
                                         ifelse(bar_chart$values>mean_county,"Worse Than the County Indicator","")))
        } else if(input$comparation_selector == 4){#Facility
          facility<-ifelse(!is.null(input$facility_selector_2),input$facility_selector_2, "SOUTHEAST ALABAMA MEDICAL CENTER")
          facility_score <- data%>%subset(Facility.Name==facility&Measure.Name==input$indicator_selector,select=Score)%>% transform(Score = as.numeric(Score))
          bar_chart$comp <-ifelse(bar_chart$values<mean(facility_score$Score),"Better Than the Facility Indicator",
                                  ifelse(bar_chart$values==mean(facility_score$Score),"No Different Than the Facility Indicator",
                                         ifelse(bar_chart$values>mean(facility_score$Score),"Worse Than the Facility Indicator","")))
        } else{
          
        }
        
      }
      else{
        
        county<- data[data$Facility.Name==input$facility_selector,"County.Name"]%>%head(1)
        
        bar_chart <-
          data %>% subset(
            Measure.Name == input$indicator_selector &
              County.Name == county&
              Number.of.Patients.Returned != "Not Applicable",
            select = c(State, County.Name, Number.of.Patients.Returned, Facility.Name, Compared.to.National)
          )%>% transform(Number.of.Patients.Returned = as.numeric(Number.of.Patients.Returned))%>%rename(values=Number.of.Patients.Returned,categories=Facility.Name)%>%arrange(desc(values))%>%as.data.frame()
        
        if(input$comparation_selector ==1){#national
          bar_chart$comp <-bar_chart$Compared.to.National
        }else if(input$comparation_selector ==2){ #state
          state_data = data%>%subset(
            Measure.Name == input$indicator_selector &
              State == input$states_selector_3&
              Number.of.Patients.Returned != "Not Applicable",
            select = c(Facility.Name, Number.of.Patients.Returned))%>% transform(Number.of.Patients.Returned = as.numeric(Number.of.Patients.Returned))
          mean_state = mean(state_data$Number.of.Patients.Returned)
          
          bar_chart$comp <-ifelse(bar_chart$values<mean_state,"Better Than the State Indicator",
                                  ifelse(bar_chart$values==mean_state,"No Different Than the State Indicator",
                                         ifelse(bar_chart$values>mean_state,"Worse Than the State Indicator","")))
          
        }else if(input$comparation_selector ==3){#county
          county_data = data%>%subset(
            Measure.Name == input$indicator_selector &
              State == input$states_selector_4&
              County.Name == input$county_selector_3&
              Number.of.Patients.Returned != "Not Applicable",
            select = c(Facility.Name, Number.of.Patients.Returned))%>% transform(Number.of.Patients.Returned = as.numeric(Number.of.Patients.Returned))
          mean_county = mean(county_data$Number.of.Patients.Returned)
          bar_chart$comp <-ifelse(bar_chart$values<mean_county,"Better Than the County Indicator",
                                  ifelse(bar_chart$values==mean_county,"No Different Than the County Indicator",
                                         ifelse(bar_chart$values>mean_county,"Worse Than the County Indicator","")))
        } else if(input$comparation_selector == 4){#Facility
          facility<-ifelse(!is.null(input$facility_selector_2),input$facility_selector_2, "SOUTHEAST ALABAMA MEDICAL CENTER")
          facility_score <- data[data$Facility.Name==facility&data$Measure.Name==input$indicator_selector,"Number.of.Patients.Returned"]
          
          bar_chart$comp <-ifelse(bar_chart$values<facility_score,"Better Than the Facility Indicator",
                                  ifelse(bar_chart$values==facility_score,"No Different Than the Facility Indicator",
                                         ifelse(bar_chart$values>facility_score,"Worse Than the Facility Indicator","")))
        } else{
          
        }
        
      }
    }
    bar_chart$colorpalete <-ifelse(grepl("Better", bar_chart$comp, fixed = TRUE), "#4da6ff",ifelse(grepl("Worse", bar_chart$comp, fixed = TRUE),"#ffa64d",ifelse(grepl("No Different", bar_chart$comp, fixed = TRUE), "#999966", "#cccccc")))
    return(bar_chart)
  }
  )
  
  plot_bar_chart<-function(){
    
    bar_chart_data<-bar_chart_data()
    bar_plot <-plot_ly(data = bar_chart_data)
    
    
    bar_plot<-bar_plot%>% add_trace(x=~categories, y=~values, marker = list(color=~colorpalete))%>%layout( annotations = list(),
                                                                                                           xaxis = list(title="Categories", categoryorder="array", categoryarray=bar_chart_data$categories),
                                                                                                           margin=list(b = 180, t = 5), # to prevent labels getting cut out
                                                                                                           hovermode = 'false' # to get hover compare mode as default
    )%>%config(displayModeBar = FALSE, displaylogo = F)
    
  }
  
  output$bar_plot<-renderPlotly({plot_bar_chart()})
  output$plot_legend<-renderUI(
    p(tags$b("Legend"),
      br(),
      img(src='signif_better.png', height=12, style="padding-right: 2px; vertical-align:middle"),"Better than indicator",
      img(src='signif_nocalc2.png', height=12, style="padding-right: 2px; vertical-align:middle"), "No different to indicator",
      br(),
      img(src='signif_worse.png', height=12, style="padding-right: 2px; vertical-align:middle"), "Worse than indicator",
      img(src='non_signif.png', height=12, style="padding-right: 2px; vertical-align:middle"), "No differences can be calculated")
  )
  
  get_inputs_scatter <- reactive({
    args <- list(data = data, measure = input$measure,
                 selected_states = c(input$state_1, input$state_2, input$state_3, input$state_4, input$state_5))
  })
  
  
  output$scatter <- renderPlot({
    do.call(make_scatterplot, get_inputs_scatter())
  })
  
  # Idiom 3 server logic
  
  updateSelectizeInput(session, 'hospital1', choices = id3_facility.Names, server = TRUE)
  
  
  
  output$value <- renderText({ input$hospital })
  hospital_name <- reactive({ input$hospital })
  
  output$plot <- renderPlotly({
    
    data <- id3_df[id3_df$Facility.Name %in% input$hospital1,]
    
    ggplotly(ggplot(data, aes(x =Measure.Name, y = Score, col=Facility.Name)) + geom_point() +
               labs(x='Measure Name', fill='Hospital Name') + theme(legend.position=c(0,0))+ 
               scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) ) %>% 
      layout(legend = list(x = 0, y = -0.3, title=list(text='<b> Hospital Name </b>'), orientation="h"))
    
  })
  
  
  # Idiom 4 server logic
  id4_basedf <- reactive({
    
    id4_df <- id4_data[id4_data$Measure.Name == input$varMeasure, ]
    
    id4_df <- id4_df[id4_df$State %in% input$inputStates, ]
    
    return(id4_df)
  })
  
  output$measureChosen <- renderText({ 
    paste("You have selected : ", input$varMeasure)
  })
  
  output$statesChosen <- renderText({ 
    paste(input$inputStates)
  })
  
  output$hospitals <- renderPlotly({
    id4_df <- id4_basedf()

    if (id4_df[1,]$Number.of.Hospitals.Worse != "Not Applicable") {
      chosenColumns <- c("State", "Number.of.Hospitals.Worse", "Number.of.Hospitals.Same", "Number.of.Hospitals.Better", "Number.of.Hospitals.Too.Few")
    } else {
      chosenColumns <- c("State", "Number.of.Hospitals.Fewer", "Number.of.Hospitals.Average", "Number.of.Hospitals.More", "Number.of.Hospitals.Too.Small")
    }
    
    df2 <- melt(id4_df[, chosenColumns])
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

# Run app ----
shinyApp(ui, server)
