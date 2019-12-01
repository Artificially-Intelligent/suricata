#####
# Functions for setting up shiny user authentication using auth0, and querying auth0 API for user info and roles.
#####

# Activate package dependencies
my_packages <- c('auth0','httr')
invisible(lapply(my_packages, library, character.only = TRUE))

print(paste("Loading Auth0 Config"))


required_auth0_env_variables <-   c(
  'OAUTH_API_KEY',
  'OAUTH_API_SECRET',
  'OAUTH_API_AUDIENCE',
  'OAUTH_KEY',
  'OAUTH_SECRET',
  'OAUTH_REDIRECT_URL',
  'OAUTH_PROVIDER','OAUTH_USER'
)
missing_auth0_env_variable <- FALSE
for (env_variable in required_auth0_env_variables){
  if(nchar(Sys.getenv(env_variable))==0){
    print(paste('Warning: Missing ENV variable or no value assigned:',env_variable ))
    missing_auth0_env_variable <- TRUE
  }
}



# Setup development bypass of oauth

if ((interactive() && ! enable_oauth_dev) || missing_auth0_env_variable) {
  using_auth <- FALSE
  print(paste("Warning: Running with user auth module deactivated"))
}else{
  using_auth <- TRUE
}

# Setup Auth0 Dependencies

a0_info <- auth0::auth0_info()

options(auth0_config_file = file.path('_auth0.yml')
        ,shiny.port = shiny_port
        ,auth0_disable = (! using_auth))

auth0_api_token <- function(auth0_account=Sys.getenv("OAUTH_USER"),
                            api_client_id=Sys.getenv("OAUTH_API_KEY"),
                            api_client_secret=Sys.getenv("OAUTH_API_SECRET"),
                            api_audience=Sys.getenv("OAUTH_API_AUDIENCE")) {
  api_query_url <- paste("https://", auth0_account  , ".au.auth0.com", "/oauth/token/", sep = "")
  headers <- add_headers("Content-Type" = "application/json")
  body <- paste('{"client_id":"', api_client_id , '","client_secret":"', api_client_secret, '","audience":"', api_audience ,'","grant_type":"client_credentials"}',sep = "")
  
  response <-POST(api_query_url, config = headers, body=body,  verbose())
  if(response$status_code == 200){
    response_content <- content(response)
    access_token <- response_content$access_token  
  }else{
    access_token <- "access_token_error"
    print(paste('Error retriving auth0 accesstoken: "',paste(content(response),collapse = ", "), '"', sep = ''))
  }
  return(access_token)
}

auth0_api_query <- function(api_query_url,
                            access_token=auth0_api_token()
                            ) {
  headers <- add_headers(
    "Authorization" = paste("Bearer", access_token)
  )
  response <-GET(api_query_url, config = headers, verbose())
  
  response$status_code
  response_content <- content(response)
  if(response$status_code == 200){
    if(length(response_content) > 0){
      top_names <- names(response_content[[1]])
      top_names <- top_names[top_names != 'sources']
      names <- c( top_names, names(response_content[[1]]$sources[[1]]))
      
      query_result <- as.data.frame(t(matrix(unlist(response_content), nrow=length(unlist(response_content[1])))))
      colnames(query_result) <- names
    }else{
      print(paste('Warning: No data returened for API query: ' , api_query_url, sep = ''))
      query_result <- data.frame()  
    }
  }else{
    print(paste('Error: Unable to retrive requested info from auth0 api ', api_query_url ,'. API query response: "',paste(content(response),collapse = ", "), '"', sep = ''))
    query_result <- data.frame()
  }
  return(query_result)
}

auth0_user_roles <- function(user_id,
                             api_audience=Sys.getenv("OAUTH_API_AUDIENCE"),
                             access_token=auth0_api_token()
                             ) {
  if(length(user_id)>0 && nchar(user_id) > 0){
    api_query_url <- paste(api_audience, "users/", user_id, "/roles", sep="")
    user_roles <- auth0_api_query(api_query_url = api_query_url,
                                  access_token = access_token )
  }else{
    print('Error: Unable to retrive user role from auth0 api: " No user_id was provided to api query')
    user_roles <- data.frame()
  }
  return(user_roles)
}

logout_url <- function() {
  config <- auth0_config()
  
  app_url_enc <- utils::URLencode(redirect_uri, reserved = TRUE)
  logout_url <- sprintf("%s/v2/logout?client_id=%s&returnTo=%s",
                        config$auth0_config$api_url,
                        config$auth0_config$credentials$key,
                        app_url_enc)
  logout_url
}


logoutButton <- function(label = "Log out", ..., id = "._auth0logout_") {
  shiny::actionButton(id, label, ...)
}