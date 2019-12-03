####--UI--------------------------------------------------------------------------------------------

#  
# The UI isn't really here but in server.R
# This allows dynamic control over UI elements
#


ui <- dashboardPagePlus(
                        skin = 'yellow',
                        md = TRUE,
                        sidebar_fullCollapse = TRUE,
                        header = dashboardHeaderPlus(
                          fixed = TRUE,
                          #title = "Modular Mapping",
                          title = tagList(
                            tags$span(class = "logo-lg", project_name), 
                            tags$img(src = "./img/artificially-intelligent.png"),
                            use_waiter(),
                            show_waiter_on_load(spin_fading_circles())
                          ),
                          enable_rightsidebar = FALSE,
                          rightSidebarIcon = "gears",
                          userOutput("user")
                        ),
              uiOutput("ui_sidebar"),
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


