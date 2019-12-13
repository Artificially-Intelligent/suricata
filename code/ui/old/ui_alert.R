####--UI SOLUTION--------------------------------------------------------------------------------------------


tabItem_alert_dashboard <-tabItem(tabName = "alert_dash",
                                 tabPanel(
                                   fluidRow(width = "100%"),
                                   fluidRow(width = "100%",
                                            column(
                                              width = 6,h2(icon("dashboard"), HTML("&nbsp;"),"Traffic Alerts Overview")
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
                                   # fluidRow(width = "100%",
                                   #          valueBoxOutput(width = 3,"http.destinations"),
                                   #          valueBoxOutput(width = 3,"http.rate"),
                                   #          valueBoxOutput(width = 3,"http.requests"),
                                   #          valueBoxOutput(width = 3,"http.status.pct")
                                   # ),
                                   # fluidRow(
                                   #   width = "100%",
                                   #    column(width = 8, 
                                   #     box(
                                   #       width = 12, status = "info", solidHeader = TRUE,
                                   #       title = "Requests to http hosts (last 30 min)",
                                   #       bubblesOutput("http.hostname.bubbleplot", width = "100%", height = 220)
                                   #          #%>% withSpinner(color="#0dc5c1")
                                   #       ),
                                   #   ),
                                   #    column(
                                   #       width = 4,
                                   #        box(
                                   #         width = 12,
                                   #         status = "info",
                                   #         solidHeader = TRUE,
                                   #         title = "Top Host (last 5 min)",
                                   #         div(style = 'height:220px;overflow-y: scroll', 
                                   #             tableOutput('http.hostname.table')
                                   #              #%>% withSpinner(color="#0dc5c1")
                                   #             )
                                   #        )
                                   #     )
                                   #  )
                                   )
                                 
)

####--UI SOLUTION--------------------------------------------------------------------------------------------


tabItem_alert_table <-tabItem(tabName = "alert_table",
      fluidRow(
        box(
          
          width = 12, status = "info", solidHeader = TRUE,
          title = "Traffic Alert Details",
          div(style = 'overflow-y: scroll;overflow-x: scroll', 
              DTOutput('alert.table') 
              #%>% withSpinner(color="#0dc5c1")
          )
        )
      ),
      fluidRow(
        column(
          width = 3,
          #offset = 9,
          downloadButton("alert.download_csv", "Download as CSV")
        )
      )
      #    verbatimTextOutput("alert.raw"),
      
)



####--UI SOLUTION--------------------------------------------------------------------------------------------

#
# SOLU
#


tabItem_alert_map <-
  tabItem(tabName = "alert_map",
          fluidRow(
            column(width = 6,
                   h2(icon("globe-asia"), HTML("&nbsp;"),"Traffic Alert Map")
            )
          ),
           tabPanel(
             'Alert Map', br(),
            fluidRow(
              box(
                title = NULL, width = 12, background = NULL,
                leafletOutput(outputId = "alert_map_leaflet") 
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
                )
              )
             )
          )
  )

