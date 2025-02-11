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
                                            valueBoxOutput(width = 3,"all.report_period")
                                   ),
                                   fluidRow(
                                     width = "100%",
                                      column(width = 8, 
                                       box(
                                         width = 12, status = "info", solidHeader = TRUE,
                                         title = "Event Types (last 30 min)",
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
                                           title = paste("Top Event Types", sep = "" ),
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
       # fluidRow(width = "100%",
       #          column(
       #            width = 6,h2(icon("table"), HTML("&nbsp;"),"Event Details")
       #          )
       # ),
      fluidRow(
        box(
          width = 12, status = "info", solidHeader = TRUE,
          title = "Event Details",
          DTOutput('all.table') 
        )
      ),
      fluidRow(
        column(
          width = 3,
          #offset = 9,
          downloadButton("all.download_csv", "Download as CSV")
        )
      )
    
  )


