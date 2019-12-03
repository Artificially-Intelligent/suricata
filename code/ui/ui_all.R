####--UI SOLUTION--------------------------------------------------------------------------------------------


tabItem_all_dashboard <-tabItem(tabName = "all_dash",
                                 tabPanel(
                                   fluidRow(width = "100%"),
                                   fluidRow(width = "100%",
                                            column(
                                              width = 6,h2(icon("dashboard"), HTML("&nbsp;"),"Event Overview")
                                            )
                                   ),
                                   fluidRow(width = "100%",
                                            valueBoxOutput(width = 3,"all.destinations"),
                                            valueBoxOutput(width = 3,"all.rate"),
                                            valueBoxOutput(width = 3,"all.requests"),
                                            valueBoxOutput(width = 3,"all.bytes")
                                   ),
                                   fluidRow(
                                     width = "100%",
                                      column(width = 8, 
                                       box(
                                         width = 12, status = "info", solidHeader = TRUE,
                                         title = "Requests to http hosts (last 30 min)",
                                         bubblesOutput("all.event_count.bubbleplot", width = "100%", height = 220)
                                            #%>% withSpinner(color="#0dc5c1")
                                         ),
                                     ),
                                      column(
                                         width = 4,
                                          box(
                                           width = 12,
                                           status = "info",
                                           solidHeader = TRUE,
                                           title = "Top Event Types (last 5 min)",
                                           div(style = 'height:220px;overflow-y: scroll', 
                                               tableOutput('all.event_count.table')
                                                #%>% withSpinner(color="#0dc5c1")
                                               )
                                          )
                                       )
                                     )
                                   )
                                 
)

####--UI SOLUTION--------------------------------------------------------------------------------------------


tabItem_all_table <-tabItem(tabName = "all_table",
                             fluidRow(width = "100%",
                                      column(
                                        width = 6,h2(icon("table"), HTML("&nbsp;"),"Event Details")
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
          title = "All Event Details (last 30 min)",
          div(style = 'overflow-y: scroll;overflow-x: scroll', 
              tableOutput('all.table') 
              #%>% withSpinner(color="#0dc5c1")
          )
        )
      ),
      fluidRow(
        column(
          width = 3,
          #offset = 6,
          downloadButton("all.download_csv", "Download as CSV")
        )
      )
    
  )


