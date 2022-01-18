hospital_scores_distribution_map <- function(selected.var.title, selected.format, selected.var.data, counties, fill_pattern) {
  
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
            scale_fill_gradient2(name = paste("Counties' ",tolower(selected.var.title),"\nin comparison with the national mean.", sep= ""), mid="cornsilk1", high="brown3", low="turquoise4", limits=c(min_value, max_value), na.value="grey98") + 
            scale_color_manual(values=NA, na.value="grey88") +              
            guides(colour=guide_legend("Data not available / Not enough data", override.aes=list(fill="grey98"))) + 
            theme(legend.position="bottom", legend.direction="vertical", legend.title = element_text(size = 13), legend.text = element_text(size = 13))
        
    }
  }
  else{
    
    color_assignement <- function(value){
      if(is.na(value)) return("grey98")
      if(value>2.5)    return("turquoise4")
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

    map <- function(x,y,dataset,fill_column){
        ggplot(data = dataset, mapping = aes(x = x, y = y, group = group)) +
             geom_polygon(aes(fill = map_fills), colour="grey88") + 
             scale_fill_identity(name = selected.var.title, guide = "legend",  labels = c("Data not available / Not enough data",worse,avg,better)) + 
             borders("state") + theme(legend.position="bottom", legend.direction="vertical", legend.title = element_text(size = 13), legend.text = element_text(size = 13))
    }    
  }
    
  filling_index <- rep(1:3076, fill_pattern)
  map_fills <- map_fills[filling_index]
  
  # plotting USA states 
  map(counties$long,counties$lat,counties,map_fills)

}


