####--UI SOLUTION--------------------------------------------------------------------------------------------


tabItem_netflow_dashboard <-tabItem(tabName = "netflow_dash",
                                 tabPanel(
                                   fluidRow(width = "100%"),
                                   fluidRow(width = "100%",
                                            column(
                                              width = 6,h2(icon("dashboard"), HTML("&nbsp;"),"Traffic netflow Overview")
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
                                            valueBoxOutput(width = 3,"netflow.destinations"),
                                            valueBoxOutput(width = 3,"netflow.rate"),
                                            valueBoxOutput(width = 3,"netflow.requests"),
                                            valueBoxOutput(width = 3,"netflow.bytes")
                                   ),
                                   fluidRow(
                                     width = "100%",
                                      column(width = 8, 
                                       box(
                                         width = 12, status = "info", solidHeader = TRUE,
                                         title = "Requests to netflow destinations (last 30 min)",
                                         bubblesOutput("netflow.dest_ip.bubbleplot", width = "100%", height = 220)
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
                                               tableOutput('netflow.dest_ip.table'))
                                          )
                                       )
                                     )
                                   )
                                 
)

####--UI SOLUTION--------------------------------------------------------------------------------------------


tabItem_netflow_table <-tabItem(tabName = "netflow_table",
                             fluidRow(width = "100%",
                                      column(
                                        width = 6,h2(icon("table"), HTML("&nbsp;"),"Traffic netflow Details")
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
          title = "netflow request details (last 30 min)",
          div(style = 'overflow-y: scroll;overflow-x: scroll', 
              tableOutput('netflow.table')
              #%>% withSpinner(color="#0dc5c1")
          )
        )
      ),
      fluidRow(
        column(
          width = 3,
          #offset = 6,
          downloadButton("netflow.download_csv", "Download as CSV")
        )
      )
#    verbatimTextOutput("netflow.raw"),
    
  )



####--UI SOLUTION--------------------------------------------------------------------------------------------

#
# SOLU
#


tabItem_netflow_map <-
  tabItem(tabName = "netflow_map",
          fluidRow(
            column(width = 6,
                   h2(icon("globe-asia"), HTML("&nbsp;"),"Traffic netflow Origin Map")
            )
          ),
          tabPanel(
            'Map', br(),
            fluidRow(
              box(
                title = NULL, width = 12, background = NULL,
                leafletOutput(outputId = "netflow_map") 
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

