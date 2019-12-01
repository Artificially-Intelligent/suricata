####--UI SOLUTION--------------------------------------------------------------------------------------------


tabItem_dashboard <-tabItem(tabName = "dashboard",
                            fluidRow(
                              column(width = 5,
                                     h2(icon("globe-asia"), HTML("&nbsp;"),"Traffic Overview")
                              )
                            ),
                            tabsetPanel(
                              type = "tabs",
                              id = "solution_tabset",
                                
                           fluidRow(
                             valueBoxOutput("rate"),
                             valueBoxOutput("requests"),
                             valueBoxOutput("destinations"),
                             valueBoxOutput("wwwBytes")
                           ),
                           fluidRow(
                             box(
                               width = 8, status = "info", solidHeader = TRUE,
                               title = "Requests to http hosts (last 30 min)",
                               bubblesOutput("packagePlot", width = "100%", height = 600)
                             ),
                             box(
                               width = 4, status = "info",
                               title = "Top packages (last 5 min)",
                               tableOutput("packageTable")
                             )
                           )
                        )
                    )
  