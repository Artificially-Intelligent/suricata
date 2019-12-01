library(shiny)
library(shinydashboard)

fluidPage(
  tags$head(tags$script(src="js.cookie.js")),
  # a shiny element to display unformatted text
  box(title ="click the gray square to view cookies!",    verbatimTextOutput("results"),actionButton("go","click me")),
  
  # javascript code to send data to shiny server
  tags$script('
          document.getElementById("go").onclick = function() {
          var number = Math.random();

          Cookies.set(\'name\', \'value\', { expires: 7 });
          Cookies.set(\'cookie_2\', \'value\', { expires: 7 });

          var my_cookie = Cookies.get(); 

          Shiny.onInputChange("mydata", my_cookie);
          };
          ')