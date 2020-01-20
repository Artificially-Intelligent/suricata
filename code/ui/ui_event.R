####--UI SOLUTION--------------------------------------------------------------------------------------------


tabItem_dashboard <- function(event = "all") {
  tab_name <- paste(event,"_dash",sep="")
  if( event == "all"){
    event_type <- "Event"  
  }else{
    if( event == "http"){
      event_type <- "HTTP"  
    }else{
      event_type <- simple_cap(event)
    }
  } 
  tab_title <- paste(event_type ,  'Overview')
  value_box_1_outputId <- paste(event,".destinations",sep="")
  value_box_2_outputId <- paste(event,".rate",sep="")
  value_box_3_outputId <- paste(event,".requests",sep="")
  value_box_4_outputId <- paste(event,".report_period",sep="")
  bubble_plot_title    <- paste(event_type ,  'Breakdown')
  if(event == "all"){
    bubble_plot_outputId <- paste(event,".event_count.bubbleplot",sep="")
    table_outputId      <- paste(event,".event_count.table",sep="")
    table_title   <- paste('Top',event_type ,'Types')  
  }else{
    bubble_plot_outputId <- paste(event,".destination.bubbleplot",sep="")
    table_outputId      <- paste(event,".destination.table",sep="")
    table_title   <- paste('Top',event_type ,'Destinations')
  }
  
  
  tabItem(tabName = tab_name,
   tabPanel(
     fluidRow(width = "100%"),
     fluidRow(width = "100%",
              column(
                width = 6,h2(icon("dashboard"), HTML("&nbsp;"),tab_title)
              )
     ),
     fluidRow(width = "100%",
              valueBoxOutput(width = 3,value_box_1_outputId)
              ,valueBoxOutput(width = 3,value_box_2_outputId)
              ,valueBoxOutput(width = 3,value_box_3_outputId)
              ,valueBoxOutput(width = 3,value_box_4_outputId)
     ),
     fluidRow(
       width = "100%",
        column(width = 8, 
         box(
           width = 12, status = "info", solidHeader = TRUE,
           title = bubble_plot_title,
           bubblesOutput(bubble_plot_outputId, width = "100%", height = 220)
              #%>% withSpinner(color="#0dc5c1")
           ),
       ),
        column(
           width = 4,
            box(
             width = 12,
             status = "info",
             solidHeader = TRUE,
             title = paste(table_title, sep = "" ),
             div(style = 'height:220px;overflow-y: scroll', 
                 tableOutput(table_outputId)
                 )
            )
         )
       )
     )
   
  )
}

####--UI SOLUTION--------------------------------------------------------------------------------------------


tabItem_table <- function(event = "all") {
  tab_name <- paste(event,"_table",sep="")
  if( event == "all"){
    event_type <- "event"  
  }else{
    event_type <- event
  } 
  tab_title <- simple_cap(paste(event_type ,  'Details'))
  DT_outputId <- paste(event,".table",sep="")
  download_csv_outputId <- paste(event,".download_csv",sep="")
  
  tabItem(tabName = tab_name,
       # fluidRow(width = "100%",
       #          column(
       #            width = 6,h2(icon("table"), HTML("&nbsp;"),tab_title)
       #          )
       # ),
      fluidRow(
        box(
          width = 12, status = "info", solidHeader = TRUE,
          title = tab_title,
          DTOutput(DT_outputId) 
        )
      ),
      fluidRow(
        column(
          width = 3,
          #offset = 9,
          downloadButton(download_csv_outputId, "Download as CSV")
        )
      )
    
  )
}


####--UI Timeseries--------------------------------------------------------------------------------------------


tabItem_timeseries <- function(event = "all") {
  tab_name <- paste(event,"_timeseries",sep="")
  if( event == "all"){
    event_type <- "event"  
  }else{
    event_type <- event
  } 
  tab_title <- simple_cap(paste(event_type ,  'Timeseries'))
  plotly_outputId <- paste(event,".plotly",sep="")
  measure_picker_inputId <- paste(event,".measure_picker",sep="")
  value_picker_inputId <- paste(event,".value_picker",sep="")
  display_picker_inputId <- paste(event,".display_picker",sep="")
  download_csv_outputId <- paste(event,".download_timeseries_csv",sep="")
  
  tabItem(tabName = tab_name,
          # fluidRow(width = "100%",
          #          column(
          #            width = 6,h2(icon("table"), HTML("&nbsp;"),tab_title)
          #          )
          # ),
          fluidRow(
            column(
              width = 4,
              pickerInput(
                inputId = value_picker_inputId,
                label = "Value Column", 
                
                choices = c(colnames(
                  data_row_template[,!unlist(lapply(data_row_template, is.numeric))]
                                     )[4:15],
                            colnames(select(
                              data_row_template[unlist(lapply(data_row_template,is.factor))]
                              , matches(paste0("^(", paste(event, collapse="|"), ")"))))
                ),
                options = list(
                  `live-search` = TRUE)
              )
            )
            ,
            column(
              width = 4,
              pickerInput(
                inputId = measure_picker_inputId,
                label = "Measure",
                choices = c('count',
                  colnames(select(
                  data_row_template[unlist(lapply(data_row_template,is.numeric))]
                  , matches(paste0("^(", paste(event, collapse="|"), ")")))
                )),
                options = list(
                  `live-search` = TRUE)
              )
            )
            ,
            column(
              width = 4,
              pickerInput(
                inputId = display_picker_inputId,
                label = "Measure",
                choices = c('line','bar'),
                options = list(
                  `live-search` = TRUE)
              )
            )
            ,
            box(
              width = 12, status = "info", solidHeader = TRUE,
              title = tab_title,
              fluidRow(
                plotlyOutput(
                  plotly_outputId, 
                  # width = "100%", height = 350
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              #offset = 9,
              downloadButton(download_csv_outputId, "Download as CSV")
            )
          )
          
  )
}

####--UI MAP--------------------------------------------------------------------------------------------


tabItem_map <- function(event = "all") {
  tab_name <- paste(event,"_map",sep="")
  leaflet_outputId <- paste(tab_name, "_leaflet",sep="")
  zoom_all_button_inputId = paste(leaflet_outputId, "_zoom_all_button",sep="")
  table_summary_outputId <- paste(tab_name,"_table_summary",sep="")
  DT_detail_outputId <- paste(tab_name,"_table_detail",sep="")
  
  value_box_1_outputId <- paste(tab_name,".value.1",sep="")
  value_box_2_outputId <- paste(tab_name,".value.2",sep="")
  value_box_3_outputId <- paste(tab_name,".value.3",sep="")
  value_box_4_outputId <- paste(tab_name,".value.4",sep="")
  
  if( event == "all"){
    event_type <- ""  
  }else{
    if( event == "http"){
      event_type <- "HTTP"  
    }else{
      event_type <- simple_cap(event)
    }
  } 
  tab_title <- paste(event_type ,  'Map')
  summary_table_title <- paste(event_type, 'Summary')
  detail_table_title <- paste(event_type, 'Detail')
  
  
  tabItem(tabName = tab_name,
          fluidRow(
            column(width = 6,
                   h2(icon("globe-asia"), HTML("&nbsp;"),tab_title)
            ),
            column(width = 3,offset = 3,
                   actionBttn(
                                inputId = zoom_all_button_inputId,
                                label = "Zoom All Data",
                                style = "gradient",
                                color = "primary",
                                icon = icon("search-location")
                              )
            )
          )
          # ,box(
          #   br()
            ,fluidRow(
              # box(
                title = NULL, 
                width = 12, 
                background = NULL,
                column(
                  width = 8, 
                  leafletOutput(outputId = leaflet_outputId)
                  %>% withSpinner(color="#0dc5c1")
                  
                ),
                column(
                  width = 4, 
                  valueBoxOutput( width = 6, value_box_1_outputId),
                  valueBoxOutput( width = 6, value_box_2_outputId),
                  valueBoxOutput( width = 6, value_box_3_outputId),
                  valueBoxOutput( width = 6, value_box_4_outputId),
                  box( 
                    width = 12,
                    status = "info",
                    solidHeader = FALSE,
                    # title = paste(summary_table_title, sep = "" ),
                    div(style = 'height:120px;overflow-y: scroll', 
                      tableOutput(outputId = table_summary_outputId)
                        %>% withSpinner(color="#0dc5c1")
                    )
                  )
                  
                )
                
                # ,br()
                # ,fluidRow(
                #   column(width = 6,
                #          actionBttn(
                #            inputId = "zoom_australia_button",
                #            label = "Australia", 
                #            style = "gradient",
                #            color = "primary",
                #            icon = icon("search-location")
                #          )
                #   ),
                #   column(width = 4,
                #          fluidRow(
                #            textInput(
                #              inputId = "pulse_icon_text",
                #              label = "Enter address"
                #            ),
                #            textOutput("pulse_icon_message")
                #          )
                #   ),
                #   column(width = 2,
                #          actionBttn(
                #            inputId = "pulse_icon_button",
                #            label = "Go",
                #            style = "gradient",
                #            color = "primary"
                #          ))
                # )
              # )
            # )
          ),fluidRow(
            width = 12,
            DTOutput(outputId = DT_detail_outputId)
            %>% withSpinner(color="#0dc5c1")
            
          )
  )
}


