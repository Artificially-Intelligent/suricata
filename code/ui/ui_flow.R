####--UI SOLUTION--------------------------------------------------------------------------------------------


tabItem_flow_dashboard <-tabItem(tabName = "flow_dash",
                                 tabPanel(
                                   fluidRow(width = "100%"),
                                   fluidRow(width = "100%",
                                            column(
                                              width = 6,h2(icon("dashboard"), HTML("&nbsp;"),"Traffic Flow Overview")
                                            )
                                            # ,
                                            # column(
                                            #   width = 3,
                                            #   offset = 3,
                                            #   numericInput(
                                            #     inputId = "maxrows",
                                            #     label = "Rows to show", 
                                            #     value =  25,
                                            #     min = 0
                                            #   )
                                            #)
                                   ),
                                   fluidRow(width = "100%",
                                            valueBoxOutput(width = 3,"flow.destinations"),
                                            valueBoxOutput(width = 3,"flow.rate"),
                                            valueBoxOutput(width = 3,"flow.requests"),
                                            valueBoxOutput(width = 3,"flow.bytes")
                                   ),
                                   fluidRow(
                                     width = "100%",
                                      column(width = 8, 
                                       box(
                                         width = 12, status = "info", solidHeader = TRUE,
                                         title = "Requests to flow destinations (last 30 min)",
                                         bubblesOutput("flow.dest_ip.bubbleplot", width = "100%", height = 220)
                                        ),
                                     ),
                                      column(
                                         width = 4,
                                          box(
                                           width = 12,
                                           status = "info",
                                           solidHeader = TRUE,
                                           title = "Top Destinations (last 5 min)",
                                           div(style = 'height:220px;overflow-y: scroll', 
                                               tableOutput('flow.dest_ip.table'))
                                          )
                                       )
                                     )
                                   )
                                 
)

####--UI SOLUTION--------------------------------------------------------------------------------------------


tabItem_flow_table <-tabItem(tabName = "flow_table",
                             fluidRow(width = "100%",
                                      column(
                                        width = 6,h2(icon("table"), HTML("&nbsp;"),"Traffic Flow Details")
                                      ),
                                      column(
                                        width = 3,
                                        offset = 3,
                                        numericInput(
                                          inputId = "maxrows",
                                          label = "Rows to show", 
                                          value =  25,
                                          min = 0
                                        )
                                      )
                             ),
      fluidRow(
        box(
          
          width = 12, status = "info", solidHeader = TRUE,
          title = "flow request details (last 30 min)",
          div(style = 'overflow-y: scroll;overflow-x: scroll', 
              tableOutput('flow.table')
              #%>% withSpinner(color="#0dc5c1")
          )
        )
      ),
      fluidRow(
        column(
          width = 3,
          #offset = 6,
          downloadButton("flow.download_csv", "Download as CSV")
        )
      )
#    verbatimTextOutput("flow.raw"),
    
  )



####--UI SOLUTION--------------------------------------------------------------------------------------------

#
# SOLU
#


tabItem_flow_map <-
  tabItem(tabName = "flow_map",
          fluidRow(
            column(width = 6,
                   h2(icon("globe-asia"), HTML("&nbsp;"),"Traffic Flow Origin Map")
            )
          ),
          tabPanel(
            'Map', br(),
            fluidRow(
              box(
                title = NULL, width = 12, background = NULL,
                leafletOutput(outputId = "flow_map") 
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

