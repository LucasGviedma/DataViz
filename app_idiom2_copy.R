library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)
library(shinyBS) #modals
library(plotly) #interactive graphs
library(dplyr)

# suppressPackageStartupMessages(library(circlize))


source("helpers_idiom2.R")

# Load data ----
data <- read.csv("data/Unplanned_Hospital_Visits-Hospital.csv")

#globals:
facilities <- sort(unique(data$Facility.Name))
states <- sort(unique(data$State))
measures <- sort(unique(data$Measure.Name))
counties <- sort(unique(data$County.Name))
#counties <- sort(unique(data[data$State == "AK", "County.Name"]))

del_states <- c('AK', 'AS', 'DC', 'GU', 'HI', 'MP','NC', 'ND', 'NY',
                'PR')

measure_names <- levels(factor(data$Measure.Name))
states <- levels(factor(data$State))
states_choices <- append(list("--"), as.list(states))
# states_choices <- states_choices[!(states_choices %in% del_states)]

ui <- navbarPage(
  "Hospital Returns Data Analytics",
  
  tabPanel(
    "Idiom 2",
    fluidPage(theme = shinytheme("cerulean")),
    tags$head(tags$style(
      HTML(".shiny-output-error-validation{color: red;}")
    )),
    pageWithSidebar(
      headerPanel('Ranks'),
      sidebarPanel(
        #filter options
        width = 4,
        helpText("Unplanned hospital visits values comparison"),
        fluidRow(
          column(
            12,
            div(
              title = "Use this option to select a measure",
              selectInput(
                "indicator_selector",
                "Step 1. Select a measure:",
                choices = measures,
                selected = 1
              )
            ),
            div(
              awesomeRadio(
                "comp_area",
                label = shiny::HTML("<p>Step 2. Select option to compare by:</p>"),
                #br required to try and keep alignment across columns
                choices = list(
                  "States" = 1,
                  "Counties" = 2,
                  "Facilities" = 3
                ),
                selected = 1,
                inline = TRUE,
                checkbox = TRUE
              )
            ),
            #conditionalPanel(condition = "input.comp_area == 1",
            #                 uiOutput("states_selector")),
            conditionalPanel(condition = "input.comp_area==2",
                             uiOutput("county_selector")),
            conditionalPanel(condition = "input.comp_area==3",
                             uiOutput("facility_selector")),
            div(
              title = "Use this option to select an option to compare",
              awesomeRadio(
                "comparation_selector",
                label = shiny::HTML("<p>Step 3. Select element to compare with</p>"),
                #br required to try and keep alignment across columns
                choices = list(
                  "National level"=1,
                  "A state" = 2,
                  "A county" = 3,
                  "A facility" = 4
                ),
                selected = 1,
                inline = TRUE,
                checkbox = TRUE
              )
            ),
            conditionalPanel(condition = "input.comparation_selector == 2",
                             uiOutput("states_selector_3")),
            conditionalPanel(condition = "input.comparation_selector == 3",
                             uiOutput("county_selector_2")),
            conditionalPanel(condition = "input.comparation_selector == 4",
                             uiOutput("facility_selector_2"))
          ),
          br()
        )
      ),
      mainPanel(
        #Bootstrap modal dialog
        bsModal(
          "mod_defs_rank",
          "Definitions",
          "defs_rank",
          htmlOutput('defs_text_rank')
        ),
        #uiOutput("rank_summary"), #description of the charts
        
        fluidRow(
          column(
            width = 12,
            h4(textOutput("chart_title"), style = "color: black; text-align: left"),
            h5(textOutput("chart_subtitle"), style = "color: black; text-align: left"),
            withSpinner(plotlyOutput("bar_plot"))
          )
        ),
        fluidRow(
          column(
            width = 6,
            uiOutput("plot_legend")
          ),
          column(
            width = 6,
            h5(textOutput("location_info"),style = "color: black; text-align: right")
          )
        )
      )
    )
  )
)

# Server logic ----
server <- function(input, output) {
  
  # IDIOM 2 tab!! ----
  output$states_selector <- renderUI({
    div(
      title = "Use this option to select a state",
      selectInput(
        "states_selector",
        "Select State:",
        choices = states,
        selected = 1
      )
    )
  })
  output$facility_selector <- renderUI({
    div(
      title = "use this option to select a Hospital",
      
      selectInput(
        "facility_selector",
        "Select facility:",
        choices = facilities,
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
        choices = facilities,
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
        choices = states,
        selected = ifelse(
          !is.null(input$states_selector_2),
          input$states_selector_2,
          1
        )
      ),
      selectInput(
        "county_selector",
        "Select county:",
        choices = counties,
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
        choices = states,
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
        choices = states,
        selected = ifelse(
          !is.null(input$states_selector_4),
          input$states_selector_4,
          1
        )
      ),
      selectInput(
        "county_selector_3",
        "Select county:",
        choices = counties,
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
          
          bar_chart$comp <-ifelse(bar_chart$values<mean_nat_val,"Better Than the National Rate", 
                                  ifelse(bar_chart$values==mean_nat_val,"No Different Than the National Rate",
                                         ifelse(bar_chart$values>mean_nat_val,"Worse Than the National Rate","")))
        }else if(input$comparation_selector ==2){ #state
          state_data = data%>%subset(
            Measure.Name ==input$indicator_selector &
              State == ifelse(!is.null(input$states_selector_3),input$states_selector_3,"AK")&
              Score != "Not Available",
            select = c(State, Score))%>% transform(Score = as.numeric(Score))
            #select = c(Facility.Name, Score))%>% transform(Score = as.numeric(Score))
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
            #select = c(Facility.Name, Score))%>% transform(Score = as.numeric(Score))
            select = c(County.Name, Score))%>% transform(Score = as.numeric(Score))
          mean_county = mean(county_data$Score)
          bar_chart$comp <-ifelse(bar_chart$values<mean_county,"Better Than the County Rate", 
                                  ifelse(bar_chart$values==mean_county,"No Different Than the County Rate",
                                         ifelse(bar_chart$values>mean_county,"Worse Than the County Rate","")))
        } else if(input$comparation_selector == 4){#Facility
          facility<-ifelse(!is.null(input$facility_selector_2),input$facility_selector_2, "SOUTHEAST ALABAMA MEDICAL CENTER")
          facility_score <- data%>%subset(
            Facility.Name==facility&
              Measure.Name==input$indicator_selector,select=Score)%>% transform(Score = as.numeric(Score))
          
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
          
          bar_chart$comp <-ifelse(bar_chart$values<mean_nat_val,"Better Than the National Rate", 
                                  ifelse(bar_chart$values==mean_nat_val,"No Different Than the National Rate",
                                         ifelse(bar_chart$values>mean_nat_val,"Worse Than the National Rate","")))
        }else if(input$comparation_selector ==2){ #state
          state_data = data%>%subset(
            Measure.Name == input$indicator_selector &
              State == input$states_selector_3&
              Number.of.Patients.Returned != "Not Applicable",
            select = c(Facility.Name, Number.of.Patients.Returned))%>%transform(Number.of.Patients.Returned = as.numeric(Number.of.Patients.Returned))
          mean_state = mean(state_data$Number.of.Patients.Returned)
          
          bar_chart$comp <-ifelse(bar_chart$values<mean_state,"Better Than the State Rate", 
                                  ifelse(bar_chart$values==mean_state,"No Different Than the State Rate",
                                         ifelse(bar_chart$values>mean_state,"Worse Than the State Rate","")))
          
        }else if(input$comparation_selector ==3){#county
          county_data = data%>%subset(
            Measure.Name == input$indicator_selector &
              State == input$states_selector_4&
              County.Name == input$county_selector_3&
              Number.of.Patients.Returned != "Not Applicable",
            select = c(Facility.Name, Number.of.Patients.Returned))%>% transform(Number.of.Patients.Returned = as.numeric(Number.of.Patients.Returned))
          mean_county = mean(county_data$Number.of.Patients.Returned)
          bar_chart$comp <-ifelse(bar_chart$values<mean_county,"Better Than the County Rate", 
                                  ifelse(bar_chart$values==mean_county,"No Different Than the County Rate",
                                         ifelse(bar_chart$values>mean_county,"Worse Than the County Rate","")))
        } else if(input$comparation_selector == 4){#Facility
          facility<-ifelse(!is.null(input$facility_selector_2),input$facility_selector_2, "SOUTHEAST ALABAMA MEDICAL CENTER")
          facility_score <- data[data$Facility.Name==facility,"Number.of.Patients.Returned"]
          
          bar_chart$comp <-ifelse(bar_chart$avg_score<facility_score,"Better Than the Facility Rate", 
                                  ifelse(bar_chart$avg_score==facility_score,"No Different Than the Facility Rate",
                                         ifelse(bar_chart$avg_score>facility_score,"Worse Than the Facility Rate","")))
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
            #select = c(State, County.Name, Score)
          )%>% transform(Score = as.numeric(Score))%>%rename(values=Score,categories=Facility.Name)%>%arrange(desc(values))%>% as.data.frame()
        
        
        if(input$comparation_selector ==1){#national
          mean_nat_val<-mean(bar_chart$values)
          
          bar_chart$comp <-ifelse(bar_chart$values<mean_nat_val,"Better Than the National Rate", 
                                  ifelse(bar_chart$values==mean_nat_val,"No Different Than the National Rate",
                                         ifelse(bar_chart$values>mean_nat_val,"Worse Than the National Rate","")))
        }else if(input$comparation_selector ==2){ #state
          state_data = data%>%subset(
            Measure.Name == input$indicator_selector &
              State == input$states_selector_3&
              Score != "Not Available",
            select = c(Facility.Name, Score))%>%transform(Score = as.numeric(Score))
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
            select = c(Facility.Name, Score))%>%transform(Score = as.numeric(Score))
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
          )%>% transform(Number.of.Patients.Returned = as.numeric(Number.of.Patients.Returned))%>% group_by(State) %>% summarize(avg_patients = mean(Number.of.Patients.Returned))%>%rename(values=avg_patients,categories=State)%>%arrange(desc(values))%>%as.data.frame()
        
        
        if(input$comparation_selector ==1){#national
          mean_nat_val<-mean(bar_chart$values)
          
          bar_chart$comp <-ifelse(bar_chart$values<mean_nat_val,"Better Than the National Rate", 
                                  ifelse(bar_chart$values==mean_nat_val,"No Different Than the National Rate",
                                         ifelse(bar_chart$values>mean_nat_val,"Worse Than the National Rate","")))
        }else if(input$comparation_selector ==2){ #state
          state_data = data%>%subset(
            Measure.Name == input$indicator_selector &
              State == input$states_selector_3&
              Number.of.Patients.Returned != "Not Applicable",
            select = c(Facility.Name, Number.of.Patients.Returned))%>%transform(Number.of.Patients.Returned = as.numeric(Number.of.Patients.Returned))
          mean_state = mean(state_data$Number.of.Patients.Returned)
          
          bar_chart$comp <-ifelse(bar_chart$values<mean_state,"Better Than the State Rate", 
                                  ifelse(bar_chart$values==mean_state,"No Different Than the State Rate",
                                         ifelse(bar_chart$values>mean_state,"Worse Than the State Rate","")))
          
        }else if(input$comparation_selector ==3){#county
          county_data = data%>%subset(
            Measure.Name == input$indicator_selector &
              State == input$states_selector_4&
              County.Name == input$county_selector_3&
              Number.of.Patients.Returned != "Not Applicable",
            select = c(Facility.Name, Number.of.Patients.Returned))%>%transform(Number.of.Patients.Returned = as.numeric(Number.of.Patients.Returned))
          mean_county = mean(county_data$Number.of.Patients.Returned)
          bar_chart$comp <-ifelse(bar_chart$values<mean_county,"Better Than the County Rate", 
                                  ifelse(bar_chart$values==mean_county,"No Different Than the County Rate",
                                         ifelse(bar_chart$values>mean_county,"Worse Than the County Rate","")))
        } else if(input$comparation_selector == 4){#Facility
          facility<-ifelse(!is.null(input$facility_selector_2),input$facility_selector_2, "SOUTHEAST ALABAMA MEDICAL CENTER")
          facility_score <- data[data$Facility.Name==facility&data$Measure.Name==input$indicator_selector,"Number.of.Patients.Returned"]%>%transform(Number.of.Patients.Returned = as.numeric(Number.of.Patients.Returned))
          bar_chart$comp <-ifelse(bar_chart$values<facility_score,"Better Than the Facility Rate", 
                                  ifelse(bar_chart$values==facility_score,"No Different Than the Facility Rate",
                                         ifelse(bar_chart$values>facility_score,"Worse Than the Facility Rate","")))
        } else{
          
        }
        
      }
    }
    else if(input$comp_area == 3){ #by Facility
      if(grepl("Rate", input$indicator_selector, fixed = TRUE)) {
        
        #county <- data[data$Facility.Name==input$facility_selector,"County.Name"]%>%head(1)
        data_facility <- data[data$Facility.Name==input$facility_selector,"Facility.Name"]%>%head(1)
        
        bar_chart <-
          data %>% subset(
            Measure.Name == input$indicator_selector &
              Facility.Name == data_facility&
              #County.Name == county&
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
          
          bar_chart$comp <-ifelse(bar_chart$values<mean_state,"Better Than the State Rate", 
                                  ifelse(bar_chart$values==mean_state,"No Different Than the State Rate",
                                         ifelse(bar_chart$values>mean_state,"Worse Than the State Rate","")))
          
        }else if(input$comparation_selector ==3){#county
          county_data = data%>%subset(
            Measure.Name == input$indicator_selector &
              State == input$states_selector_4&
              County.Name == input$county_selector_3&
              Score != "Not Available",
            select = c(Facility.Name, Score))%>%transform(Score = as.numeric(Score))
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
          
          bar_chart$comp <-ifelse(bar_chart$values<mean_state,"Better Than the State Rate", 
                                  ifelse(bar_chart$values==mean_state,"No Different Than the State Rate",
                                         ifelse(bar_chart$values>mean_state,"Worse Than the State Rate","")))
          
        }else if(input$comparation_selector ==3){#county
          county_data = data%>%subset(
            Measure.Name == input$indicator_selector &
              State == input$states_selector_4&
              County.Name == input$county_selector_3&
              Number.of.Patients.Returned != "Not Applicable",
            select = c(Facility.Name, Number.of.Patients.Returned))%>% transform(Number.of.Patients.Returned = as.numeric(Number.of.Patients.Returned))
          mean_county = mean(county_data$Number.of.Patients.Returned)
          bar_chart$comp <-ifelse(bar_chart$values<mean_county,"Better Than the County Rate", 
                                  ifelse(bar_chart$values==mean_county,"No Different Than the County Rate",
                                         ifelse(bar_chart$values>mean_county,"Worse Than the County Rate","")))
        } else if(input$comparation_selector == 4){#Facility
          facility<-ifelse(!is.null(input$facility_selector_2),input$facility_selector_2, "SOUTHEAST ALABAMA MEDICAL CENTER")
          facility_score <- data[data$Facility.Name==facility&data$Measure.Name==input$indicator_selector,"Number.of.Patients.Returned"]
          
          bar_chart$comp <-ifelse(bar_chart$values<facility_score,"Better Than the Facility Rate", 
                                  ifelse(bar_chart$values==facility_score,"No Different Than the Facility Rate",
                                         ifelse(bar_chart$values>facility_score,"Worse Than the Facility Rate","")))
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
      img(src='signif_nocalc2.png', height=12, style="padding-right: 2px; vertical-align:middle"), "Not different to indicator", 
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
  
}

# Run app ----
shinyApp(ui, server)