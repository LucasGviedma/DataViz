library(shiny)
library(maps)
library(mapproj)
library(dplyr)
library(usmap)
library(tidyverse)


source("helpers.R")


USED_VARS <- c("Hospital return days for heart attack patients", "Hospital return days for heart failure patients", "Hospital return days for pneumonia patients",
               "Rate of inpatient admissions for patients receiving outpatient chemotherapy", "Rate of unplanned hospital visits after colonoscopy (per 1,000 colonoscopies)", "Rate of emergency department (ED) visits for patients receiving outpatient chemotherapy",
               "Rate of readmission for chronic obstructive pulmonary disease (COPD) patients", "Rate of readmission for CABG",
               "Rate of readmission after discharge from hospital (hospital-wide)", "Rate of readmission after hip/knee replacement",
               "Acute Myocardial Infarction (AMI) 30-Day Readmission Rate", "Heart failure (HF) 30-Day Readmission Rate", "Pneumonia (PN) 30-Day Readmission Rate",
               "Ratio of unplanned hospital visits after hospital outpatient surgery")


cleaning          <- function(df){
  df<-df[!(df$State=="AS" | df$State=="GU" | df$State=="MP" | df$State=="AK" | df$State=="PR" | df$State=="VI" | df$State=="HI"),]
  df$County.Name <- sapply(df$County.Name, function(x){ gsub("Saint", "St", x)})
  df$County.Name <- sapply(df$County.Name, function(x){ gsub(" City", "", x)})
  df$County.Name[df$State == "AL" & df$County.Name == "New York"] <- "Chambers"
  df$County.Name[df$State == "AZ" & df$County.Name == "DuPage"]  <- "Maricopa"
  df$County.Name[df$State == "VA" & df$County.Name == "Alexandria"] <- "District of Columbia"
  df$County.Name[df$State == "VA" & df$County.Name == "Charlottesville"] <- "Albemarle"
  df$County.Name[df$State == "VA" & df$County.Name == "Chesapeake"] <- "Norfolk"
  df$County.Name[df$State == "VA" & df$County.Name == "Danville"] <- "Pittsylvania"
  df$County.Name[df$State == "VA" & df$County.Name == "Emporia"] <- "Greensville"
  df$County.Name[df$State == "VA" & df$County.Name == "Fredericksburg"] <- "Spotsylvania"
  df$County.Name[df$State == "VA" & df$County.Name == "Galax"] <- "Carroll"
  df$County.Name[df$State == "VA" & df$County.Name == "Harrisonburg"] <- "Rockingham"
  df$County.Name[df$State == "VA" & df$County.Name == "Hopewell"] <- "Prince George"
  df$County.Name[df$State == "VA" & df$County.Name == "Lexington"] <- "Rockbridge"
  df$County.Name[df$State == "VA" & df$County.Name == "Lynchburg"] <- "Campbell"
  df$County.Name[df$State == "VA" & df$County.Name == "Manassas"] <- "Prince William"
  df$County.Name[df$State == "VA" & df$County.Name == "Norton"] <- "Wise"
  df$County.Name[df$State == "VA" & df$County.Name == "Petersburg"] <- "Prince George"
  df$County.Name[df$State == "VA" & df$County.Name == "Portsmouth"] <- "Norfolk"
  df$County.Name[df$State == "VA" & df$County.Name == "Salem"] <- "Roanoke"
  df$County.Name[df$State == "VA" & df$County.Name == "Winchester"] <- "Frederick"
  df$State[df$State == "VA" & df$County.Name == "District of Columbia"] <- "DC"
  return(df)
}
state_to_name     <- function(state){
  if (state == "AL")return("Alabama")
  if (state == "AK")return("Alaska")
  if (state == "AZ")return("Arizona")
  if (state == "AR")return("Arkansas")
  if (state == "CA")return("California")
  if (state == "CO")return("Colorado")
  if (state == "CT")return("Connecticut")
  if (state == "DE")return("Delaware")
  if (state == "FL")return("Florida")
  if (state == "GA")return("Georgia")
  if (state == "HI")return("Hawaii")
  if (state == "ID")return("Idaho")
  if (state == "IL")return("Illinois")
  if (state == "IN")return("Indiana")
  if (state == "IA")return("Iowa")
  if (state == "KS")return("Kansas")
  if (state == "KY")return("Kentucky")
  if (state == "LA")return("Louisiana")
  if (state == "ME")return("Maine")
  if (state == "MD")return("Maryland")
  if (state == "MA")return("Massachusetts")
  if (state == "MI")return("Michigan")
  if (state == "MN")return("Minnesota")
  if (state == "MS")return("Mississippi")
  if (state == "MO")return("Missouri")
  if (state == "MT")return("Montana")
  if (state == "NE")return("Nebraska")
  if (state == "NV")return("Nevada")
  if (state == "NH")return("New Hampshire")
  if (state == "NJ")return("New Jersey")
  if (state == "NM")return("New Mexico")
  if (state == "NY")return("New York")
  if (state == "NC")return("North Carolina")
  if (state == "ND")return("North Dakota")
  if (state == "OH")return("Ohio")
  if (state == "OK")return("Oklahoma")
  if (state == "OR")return("Oregon")
  if (state == "PA")return("Pennsylvania")
  if (state == "PR")return("Puerto Rico")
  if (state == "RI")return("Rhode Island")
  if (state == "SC")return("South Carolina")
  if (state == "SD")return("South Dakota")
  if (state == "TN")return("Tennessee")
  if (state == "TX")return("Texas")
  if (state == "UT")return("Utah")
  if (state == "VT")return("Vermont")
  if (state == "VI")return("Virgin Islands")
  if (state == "VA")return("Virginia")
  if (state == "WA")return("Washington")
  if (state == "WV")return("West Virginia")
  if (state == "WI")return("Wisconsin")
  if (state == "WY")return("Wyoming")
  if (state == "AS")return("American Samoa")
  if (state == "GU")return("Guam")
  if (state == "MP")return("Saipan")
  if (state == "DC")return("District of Columbia")
  
}
mean_score_county <- function(df, var_name, sel){
  
  data  <- subset(df, Measure.Name==var_name, select=c(Location, Compared.to.National, Score, Denominator))
  
  n_hosp_in_county <- count(group_by(data, Location))
  
  n_hosp_wo_var_in_county <- aggregate(data$Score, by = list(data$Location), function(x) {sum(is.na(x))})
  
  if (sel == "General overview"){
    
    data$Score[grepl("Fewer",   data$Compared.to.National, fixed = TRUE) | grepl("Better",  data$Compared.to.National, fixed = TRUE)]      <- 3
    data$Score[grepl("Average Days", data$Compared.to.National, fixed = TRUE) | grepl("No Diff", data$Compared.to.National, fixed = TRUE)] <- 2
    data$Score[grepl("More",    data$Compared.to.National, fixed = TRUE) | grepl("Worse",   data$Compared.to.National, fixed = TRUE)]      <- 1
  }
  
  data[is.na(data)] <- 0
  
  data$WeightedScore <- data$Score * data$Denominator
  
  added_score_hosp_county <- aggregate(data$WeightedScore, by = list(data$Location), FUN=sum, na.rm = TRUE)
  added_denom_hosp_county <- aggregate(data$Denominator,   by = list(data$Location), FUN=sum, na.rm = TRUE)
  
  names(n_hosp_in_county)[names(n_hosp_in_county) == "n"] <- "n_hosp"
  
  n_hosp_in_county$n_hosp_with_var <- n_hosp_in_county$n_hosp - n_hosp_wo_var_in_county$x
  n_hosp_in_county$added_score     <- added_score_hosp_county$x
  n_hosp_in_county$added_denom     <- added_denom_hosp_county$x
  
  score_manager <- function(n_hosp_with_var, added_score, added_denom){
    
    if (n_hosp_with_var > 0){ 
      return((added_score / added_denom)) }
    else{ return(NA) }
  } 
  
  n_hosp_in_county$mean_score <- mapply(score_manager, n_hosp_in_county$n_hosp_with_var, n_hosp_in_county$added_score, n_hosp_in_county$added_denom)
  n_hosp_in_county <- subset(n_hosp_in_county, select=c(Location, mean_score)) 
  n_hosp_in_county <- rbind(n_hosp_in_county, missing_counties)
  n_hosp_in_county <- n_hosp_in_county[with(n_hosp_in_county, order(Location)), ]
  print(n_hosp_in_county)
  return(n_hosp_in_county$mean_score)
}


df <- read.csv("data/Unplanned_Hospital_Visits-Hospital.csv")
df <- subset(df, select=c(State, County.Name, Measure.Name, Compared.to.National, Score, Denominator))
df <- cleaning(df)

df$Score       <- as.numeric(df$Score)
df$Denominator <- as.numeric(df$Denominator)

missing_counties <- subset(read.csv("aux_missing_counties.csv"), select=c(Location, mean_score))  
missing_counties <- transform(missing_counties, mean_score = as.numeric(mean_score))

df$State    <- sapply(df$State, state_to_name)
df$Location <- paste(df$State, df$County.Name, sep=",")

df <- subset(df, select=c(Location, Measure.Name, Compared.to.National, Score, Denominator))
df <- df[with(df, order(Location)), ]

counties     <- map_data("county")
counties$loc <- paste(counties$region, counties$subregion, sep=",")
counties     <- counties[with(counties, order(loc)), ]
fill_pattern <- count(group_by(counties, loc))$n

# USER INTERFACE
ui <- fluidPage(
  
  fluidRow(
    column(12, titlePanel("Unplanned hospital visits geographical visualization"), br(),
           
           fluidRow(
             column(6,
                    wellPanel(
                      HTML(paste("<b>Available variables:</b>
                        <br/><br/><i>These variables have been determined by the U.S.A government to compare their hospitals' performance with the national mean performance. More information about these values can be obtained in the following link: 
                        <br/><br/><a href=\"https://data.cms.gov/provider-data/dataset/632h-zaca\">Unplanned hospital visits</a></i><br/>")),
                      
                      selectInput(inputId  = "variable_selection_input",   
                                  label="",
                                  choices  = USED_VARS,             
                                  selected = USED_VARS[1]),
                    )),
             
             column(6,
                    wellPanel(
                      HTML(paste("<b>Available views:</b> 
                     <br/><br/>- General overview: <i>displays the counties' hospitals' mean final value asigned by the government taking into account the performance, size of hospitals and number of patients treated.</i>
                     <br/><br/>- Specific values: <i>displays specifically the mean performance of the counties' hospitals without taking into account other elements.</i>")),
                      
                      selectInput(inputId  = "format_selection_input",
                                  label="",
                                  choices  = c("General overview","Specific values"),
                                  selected = "General overview"),
                    ))),
           
           fluidRow(column(12, align="center", plotOutput("map")))))
)

# SERVER LOGIC
server <- function(input, output) {
  
  output$map <- renderPlot({
    
    data <- mean_score_county(df, input$variable_selection_input, input$format_selection_input)
    
    hospital_scores_distribution_map(input$variable_selection_input, input$format_selection_input, data, counties, fill_pattern)
    
  }, width = 800, height = 550)
}

# Run app ----
shinyApp(ui, server)