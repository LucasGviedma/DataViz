
USED_VARS <- c("Hospital return days for heart attack patients", "Hospital return days for heart failure patients", "Hospital return days for pneumonia patients",
               "Rate of inpatient admissions for patients receiving outpatient chemotherapy", "Rate of unplanned hospital visits after colonoscopy (per 1,000 colonoscopies)", "Rate of emergency department (ED) visits for patients receiving outpatient chemotherapy",
               "Rate of readmission for chronic obstructive pulmonary disease (COPD) patients", "Rate of readmission for CABG",
               "Rate of readmission after discharge from hospital (hospital-wide)", "Rate of readmission after hip/knee replacement",
               "Acute Myocardial Infarction (AMI) 30-Day Readmission Rate", "Heart failure (HF) 30-Day Readmission Rate", "Pneumonia (PN) 30-Day Readmission Rate",
               "Ratio of unplanned hospital visits after hospital outpatient surgery")

#Datasert cleaning function
id1_cleaning          <- function(df){
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

#Turn AL for Alabama and so on
id1_state_to_name     <- function(state){
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

#Get the mean score for each county
id1_mean_score_county <- function(df, var_name, sel, missing_counties){

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

  return(n_hosp_in_county$mean_score)
}

#Plot the map
id1_hospital_scores_distribution_map <- function(selected.var.title, selected.format, selected.var.data, counties, fill_pattern) {
  
  data  <- selected.var.data
  
  if(selected.format == 'Specific values'){
    
    mean_value <- 0
    
    if(grepl("Ratio", selected.var.title, fixed = TRUE)){
      mean_value <- 1 
      legend_add <- ""
    }
    else if(grepl("Rate", selected.var.title, fixed = TRUE)){
      mean_value <- mean(data, na.rm = TRUE)
      legend_add <- ""
    }
    
    data <- data - mean_value
    
    max_value <- max(data, na.rm = TRUE)
    min_value <- min(data, na.rm = TRUE)
    
    map_fills <- data

    map <- function(x,y,dataset,fill_column){
        ggplot(data = dataset, mapping = aes(x = x, y = y, group = group, fill = map_fills, colour="")) +
            geom_polygon() + 
            borders("state") +
            scale_fill_gradient2(name = paste("Counties' ",tolower(selected.var.title),"\nin comparison with the national mean.", sep= ""), mid="cornsilk1", high="brown3", low="dodgerblue4", limits=c(min_value, max_value), na.value="grey98") + 
            scale_color_manual(values=NA, na.value="grey88") +              
            guides(colour=guide_legend("Data not available / Not enough data", override.aes=list(fill="grey98"))) + 
            theme(legend.position="bottom", legend.direction="vertical", legend.title = element_text(size = 13), legend.text = element_text(size = 13))
        
    }
  }
  else{
    
    color_assignement <- function(value){
      if(is.na(value)) return("grey98")
      if(value>2.5)    return("dodgerblue4")
      if(value<1.5)    return("brown3")
      else return("cornsilk1")
    }
    
    if(grepl("Ratio", selected.var.title, fixed = TRUE)){
      better <- "Better than expected" 
      avg    <- "No Different than expected"
      worse  <- "Worse than expected"
    }
    else if(grepl("Rate", selected.var.title, fixed = TRUE)){
      better <- "Better Than the National Rate" 
      avg    <- "No Different Than the National Rate"
      worse  <- "Worse Than the National Rate" 
    }
    else{
      better <- "Fewer Days Than Average per 100 Discharges"
      avg    <- "Average Days per 100 Discharges"
      worse  <- "More Days Than Average per 100 Discharges"
    }
    
    map_fills <- sapply(data, color_assignement, simplify = TRUE, USE.NAMES = FALSE)
    map_fills <- factor(map_fills, levels = c("dodgerblue4", "cornsilk1", "brown3", "grey98"))
      
    map <- function(x,y,dataset,fill_column){
        ggplot(data = dataset, mapping = aes(x = x, y = y, group = group)) +
             geom_polygon(aes(fill = map_fills), colour="grey88") + 
             scale_fill_identity(name = selected.var.title, guide = "legend",  labels = c(better, worse, avg, "Data not available / Not enough data")) + 
             borders("state") + theme(legend.position="bottom", legend.direction="vertical", legend.title = element_text(size = 13), legend.text = element_text(size = 13))
    }    
  }
    
  filling_index <- rep(1:3076, fill_pattern)
  map_fills <- map_fills[filling_index]
  
  # plotting USA states 
  map(counties$long,counties$lat,counties,map_fills)

}


