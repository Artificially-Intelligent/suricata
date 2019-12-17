u <- reactiveValues(
  # User settings accessible to s_data functions
  user_id =  NULL,
  user_roles = NULL
)

output$user <- renderUser({
  if(length(session$userData$auth0_info)>0){
    user_roles <- auth0_user_roles(user_id = session$userData$auth0_info$sub)
    u$user_id  <- session$userData$auth0_info$sub
    u$user_roles <- user_roles
    
    dashboardUser(
      name = session$userData$auth0_info$name, 
      src = session$userData$auth0_info$picture, 
      title = "User",
      subtitle = paste("Roles:", paste(user_roles$name ,collapse = ", ")), 
      footer = p("The footer", class = "text-center"),
      fluidRow(
        
        column(width = 12, align="center", offset=0, 
          logoutButton(color = "success", style = "simple") 
        )
      )
    )
  }else{
    dashboardUser(
      name = "Guest", 
      src =  "./img/guestuser.svg", 
      title = "User",
      subtitle = "", 
      footer = p("The footer", class = "text-center"),
      if(using_auth){
        fluidRow(
          column(width=12, align="center",
             socialButton(url = paste(session$clientData$url_protocol,"//",session$clientData$url_hostname,":",session$clientData$url_port,sep=''),
                        type = 'google')
          )
        )
      }
    )
  }
})
