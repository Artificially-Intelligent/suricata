#### Installed Library Dependencies ####

########################################
# Add packages missing from docker image artificiallyintelligent/shiny
# Current package list can be found here:
# https://hub.docker.com/r/artificiallyintelligent/shiny
# 
# eg. missing_package <- c('auth0','shiny')

missing_package <- c()
if(length(missing_package) >0)
  install.packages(missing_package, repos='http://cran.rstudio.com/', dependencies = TRUE)
