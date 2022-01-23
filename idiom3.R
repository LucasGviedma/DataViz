library(shiny)
library(plotly)
library("stringr")

data <- read.csv("data/Unplanned_Hospital_Visits-Hospital.csv", header=TRUE) # Load Data
selected <- unique(data[,c(2, 10, 13)]) # select columns of interest

as.numeric(as.character(selected$Score))
selected <- selected[!is.na(as.numeric(as.character(selected$Score))),]

facility.Names <- unique(selected$Facility.Name)

measure.Names <- unique(selected$Measure.Name)

library(ggplot2)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "cerulean", font_scale = 0.8),
  titlePanel("Dot Chart Comparing Measure scores for different hospitals"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("hospital1", "Hospital Name", NULL, options = list(maxItems = 10, placeholder = 'Choose one or more hospitals'), selected = NULL),
      # selectizeInput("hospital2", "Hospital Name", NULL, options = list(maxItems = 1, placeholder = 'Choose a hospital 2'), selected = NULL),
      # selectizeInput("hospital3", "Hospital Name", NULL, options = list(maxItems = 1, placeholder = 'Choose a hospital 3'), selected = NULL),
      # selectizeInput("hospital4", "Hospital Name", NULL, options = list(maxItems = 1, placeholder = 'Choose a hospital 4'), selected = NULL),
      # selectizeInput("hospital5", "Hospital Name", NULL, options = list(maxItems = 1, placeholder = 'Choose a hospital 5'), selected = NULL),
      # selectizeInput("hospital6", "Hospital Name", NULL, options = list(maxItems = 1, placeholder = 'Choose a hospital 6'), selected = NULL)
      em("put the window in full screen for better vizualisation")  
    ),
    mainPanel(

      h3('Choose one or more hospitals on the side panel', style="color:grey"),

      plotlyOutput("plot", height = 700),
        
      textInput("filename", "File name:", placeholder = "My Plot"),
      radioButtons("format", "Choose format", c("png", "bmp", "pdf"), inline = TRUE),
      downloadButton("download", "Download Chart")
    )
  ),
)

server <- function(input, output, session) {
  updateSelectizeInput(session, 'hospital1', choices = facility.Names, server = TRUE)
  # updateSelectizeInput(session, 'hospital2', choices = facility.Names, server = TRUE, selected=NULL)
  # updateSelectizeInput(session, 'hospital3', choices = facility.Names, server = TRUE, selected=NULL)
  # updateSelectizeInput(session, 'hospital4', choices = facility.Names, server = TRUE, selected=NULL)
  # updateSelectizeInput(session, 'hospital5', choices = facility.Names, server = TRUE, selected=NULL)
  # updateSelectizeInput(session, 'hospital6', choices = facility.Names, server = TRUE, selected=NULL)
  
  
    
  output$value <- renderText({ input$hospital })
  hospital_name <- reactive({ input$hospital })

  output$plot <- renderPlotly({
    # data <- selected[selected$Facility.Name == input$hospital1 | selected$Facility.Name == input$hospital2 |
    #                    selected$Facility.Name == input$hospital3 |
    #                    selected$Facility.Name == input$hospital4 |
    #                    selected$Facility.Name == input$hospital5 |
    #                    selected$Facility.Name == input$hospital6,]
    
    if(length(input$hospital1) > 0){
      removeUI(
        "h3",
        multiple = FALSE,
        immediate = FALSE,
        session)
      
      data <- selected[selected$Facility.Name %in% input$hospital1,]
  
        ggplotly(ggplot(data, aes(x =Measure.Name, y = Score, col=Facility.Name)) + geom_point() +
                   labs(x='Measure Name', fill='Hospital Name') + theme(legend.position=c(0,0))+ 
                   scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) ) %>% 
                   layout(legend = list(x = 0, y = -0.3, title=list(text='<b> Hospital Name </b>'), orientation="h"))
    }

  })
  
      plotInput = function() {
        data <- selected[selected$Facility.Name %in% input$hospital1,]
        
        ggplot(data, aes(x =Measure.Name, y = Score, col=Facility.Name)) + geom_point() +
          labs(x='Measure Name', fill='Hospital Name') + theme(legend.position="bottom")+ 
          scale_x_discrete(labels = function(x) str_wrap(x, width = 15))
      }
      output$download = downloadHandler(
        filename = function() {
          paste0(input$filename, ".", input$format)
        },
        content = function(file) {
          if(input$format == 'png'){
            ggsave(file, plot = plotInput(), device = 'png', width=30, height=20, units='cm', limitsize = TRUE)
          }
          
          if(input$format == 'bmp'){
            ggsave(file, plot = plotInput(), device = 'bmp', width=30, height=20, units='cm', limitsize = TRUE)
          }
          
          if(input$format == 'pdf'){
            ggsave(file, plot = plotInput(), device = 'pdf', width=30, height=20, units='cm', limitsize = TRUE)
          }
        })
}

shinyApp(ui, server)