####--UI--------------------------------------------------------------------------------------------

#  
# The UI isn't really here but in server.R
# This allows dynamic control over UI elements
#


ui <- dashboardPagePlus(
                        skin = 'blue',
                        md = TRUE,
                        sidebar_fullCollapse = TRUE,
                        header = dashboardHeaderPlus(
                          #fixed = TRUE,
                          #title = "Modular Mapping",
                          title = tagList(
                            tags$span(class = "logo-lg", project_name), 
                            tags$img(src = "./img/artificially-intelligent.png"),
                            use_waiter(),
                            show_waiter_on_load( tagList(
                              spin_fading_circles(),
                              div(class='loading-text',"Loading Recent Traffic Data...")
                            ))
                          ),
                          enable_rightsidebar = FALSE,
                          rightSidebarIcon = "gears",
                          userOutput("user")
                        ),
                        dashboardSidebar(
                          sidebarMenuOutput("menu"),
                          shinyWidgets::sliderTextInput("data_refresh_rate","Data refresh rate (seconds)",
                                                        choices=c(0, 1, 3, 5, 10, 15, 30, 60, 120, 180,300,600,900,1800,3600,86400,"disabled"),
                                                        selected=60, grid = T),
                          textOutput("res")
                        ),
                        
              # uiOutput("ui_sidebar"),
              rightsidebar = uiOutput("ui_rightsidebar"),
              dashboardBody(
                includeCSS("www/ui.css"),
                use_waiter(),
                uiOutput("ui_body")
              ),
              title = project_name,
              footer = dashboardFooter(
                left_text = "By Artificially-Intelligent",
                right_text = "Melbourne, 2019"
              )
)

# ui <- fluidPage(
#   
#     title = NULL, width = 12, background = NULL,
#     leafletOutput(outputId = "alert_map") 
#     %>% withSpinner(color="#0dc5c1")
# )

if(! using_auth){
  print(paste("Warning: Starting Shiny app with user authentication module deactivated"))
  ui
}else{
  auth0_ui(ui, info = a0_info)
}


