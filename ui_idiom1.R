id1_ui <- fluidRow(
     column(12, titlePanel("Unplanned hospital visits geographical visualization"), br(),
           
           fluidRow(
             column(6,
                    wellPanel(
                      HTML(paste("<b>Available variables:</b>
                        <br/><br/><i>These variables have been determined by the U.S.A government to compare their hospitals' performance with the national mean performance. More information about these values can be obtained in the following link: 
                        <br/><br/><a href=\"https://data.cms.gov/provider-data/dataset/632h-zaca\">Unplanned hospital visits</a></i><br/>")),
                      
                      selectInput(inputId  = "id1_variable_selection_input",   
                                  label="",
                                  choices  = USED_VARS,             
                                  selected = USED_VARS[1]),
                    )),
             
             column(6,
                    wellPanel(
                      HTML(paste("<b>Available views:</b> 
                     <br/><br/>- General overview: <i>displays the counties' hospitals' mean final value asigned by the government taking into account the performance, size of hospitals and number of patients treated.</i>
                     <br/><br/>- Specific values: <i>displays specifically the mean performance of the counties' hospitals without taking into account other elements.</i>")),
                      
                      selectInput(inputId  = "id1_format_selection_input",
                                  label="",
                                  choices  = c("General overview","Specific values"),
                                  selected = "General overview"),
                    ))),
           
           fluidRow(column(12, align="center", plotOutput("id1_map")))))

