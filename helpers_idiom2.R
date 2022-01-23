

id2_get_state_abb <- function(state){
  if (state == "AL") {return ("alabama")}
  if (state == "AZ") {return ("arizona")}
  if (state == "AR") {return ("arkansas")}
  if (state == "CA") {return ("california")}
  if (state == "CO") {return ("colorado")}
  if (state == "CT") {return ("connecticut")}
  if (state == "DE") {return ("delaware")}
  if (state == "FL") {return ("florida")}
  if (state == "GA") {return ("georgia")}
  if (state == "ID") {return ("idaho")}
  if (state == "IL") {return ("illinois")}
  if (state == "IN") {return ("indiana")}
  if (state == "IA") {return ("iowa")}
  if (state == "KS") {return ("kansas")}
  if (state == "KY") {return ("kentucky")}
  if (state == "LA") {return ("louisiana")}
  if (state == "ME") {return ("maine")}
  if (state == "MD") {return ("maryland")}
  if (state == "MA") {return ("massachusetts")}
  if (state == "MI") {return ("michigan")}
  if (state == "MN") {return ("minnesota")}
  if (state == "MS") {return ("mississippi")}
  if (state == "MO") {return ("missouri")}
  if (state == "MT") {return ("montana")}
  if (state == "NE") {return ("nebraska")}
  if (state == "NV") {return ("nevada")}
  if (state == "NH") {return ("new hampshire")}
  if (state == "NJ") {return ("new jersey")}
  if (state == "NM") {return ("new mexico")}
  if (state == "NY") {return ("new york")}
  if (state == "NC") {return ("north carolina")}
  if (state == "ND") {return ("north dakota")}
  if (state == "OH") {return ("ohio")}
  if (state == "OK") {return ("oklahoma")}
  if (state == "OR") {return ("oregon")}
  if (state == "PA") {return ("pennsylvania")}
  if (state == "RI") {return ("rhode island")}
  if (state == "SC") {return ("south carolina")}
  if (state == "SD") {return ("south dakota")}
  if (state == "TN") {return ("tennessee")}
  if (state == "TX") {return ("texas")}
  if (state == "UT") {return ("utah")}
  if (state == "VT") {return ("vermont")}
  if (state == "VA") {return ("virginia")}
  if (state == "WA") {return ("washington")}
  if (state == "WV") {return ("west virginia")}
  if (state == "WI") {return ("wisconsin")}
  if (state == "WY") {return ("wyoming")}
  else {return (NULL)}
}


id2_getmode <- function(v, filter_options = NULL) {
  if (!is.null(filter_options)){
    for (filter_option in filter_options) {v <- v[!(tolower(v) %in% tolower(filter_option))]}
  }
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


id2_get_color <- function(value){
  color <- switch(tolower(as.character(value)),
                  "average days per 100 discharges" = "blue",
                  "no different than expected" = "blue",
                  "do different than the national rate" = "blue",
                  
                  "more days than average per 100 discharges" = "red",
                  "worse than expected" = "red",
                  "worse than the national rate" = "red",
                  
                  "fewer days than average per 100 discharges" = "green",
                  "better than expected" = "green",
                  "better than the national rate" = "green",
                  
                  "not available" = "black",
                  "<na>" = "black",
                  "number of cases too small" = "grey"
  )
  return (color)
}


id2_make_scatterplot <- function(data, measure, selected_states){
  states <- c()
  idx <- 1
  while (idx < 6) {
    if (selected_states[idx] != "--") {states <- c(states, selected_states[idx])}
    idx <- idx + 1
  }
  
  data <- data[data$Measure.Name == measure, ]
  data <- data[data$State %in% selected_states,]
  data <- data[!(data$Score %in% c("Not Applicable", "Not Available")), ]
  
  ggplot(data, aes(x = State, y = Score)) +
    geom_point(aes(color = factor(State)))
}

