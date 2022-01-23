
id2_ui <- fluidRow(
  headerPanel('Ranks'),
  sidebarPanel(
    #filter options
    width = 4,
    helpText("Unplanned hospital visits values comparison"),
    fluidRow(
      column(
        12,
        div(
          title = "Use this option to select an indicator",
          selectInput(
            "indicator_selector",
            "Step 1. Select an indicator:",
            choices = USED_VARS,
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
        conditionalPanel(condition = "input.comp_area == 1",
                         uiOutput("states_selector")),
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