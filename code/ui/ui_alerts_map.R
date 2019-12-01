####--UI SOLUTION--------------------------------------------------------------------------------------------

#
# SOLU
#


tabItem_maps <-
  tabItem(tabName = "alerts_map",
    fluidRow(
      column(width = 5,
             h2(icon("globe-asia"), HTML("&nbsp;"),"Alerts map")
             )
    ),
    tabsetPanel(
      type = "tabs",
      id = "solution_tabset",
      
####--Weather predictions-------------------------------------------------------------------------------------------
      # tabPanel(
      #   'Weather predictions', br(),
      #   dygraphOutput("solu_plot"), br(),
      #   fluidRow(
      #     box(
      #       title = "Weather prediction data",
      #       width = 12,
      #       collapsible = TRUE,
      #       collapsed = FALSE,
      #       status = "success",
      #       solidHeader = TRUE,
      #       dataTableOutput("solu_dt")
      #     )
      #   )
      # ),

####--Geography-------------------------------------------------------------------------------------------
      tabPanel(
        'Geography', br(),
        fluidRow(
          box(
            title = NULL, width = 12, background = NULL,
            leafletOutput(outputId = "alert_map") 
            %>% withSpinner(color="#0dc5c1")
            ,
            br(),
            fluidRow(
              column(width = 6,
                     actionBttn(
                       inputId = "zoom_australia_button",
                       label = "Australia", 
                       style = "gradient",
                       color = "primary",
                       icon = icon("search-location")
                       )
              ),
              column(width = 4,
                     fluidRow(
                       textInput(
                         inputId = "pulse_icon_text",
                         label = "Enter address"
                       ),
                       textOutput("pulse_icon_message")
                     )
              ),
              column(width = 2,
                     actionBttn(
                       inputId = "pulse_icon_button",
                       label = "Go",
                       style = "gradient",
                       color = "primary"
                     ))
            ),
            br(),
            dataTableOutput("geo_testmap_dt"),
            br()
          )
        )
      )
    )
  )
