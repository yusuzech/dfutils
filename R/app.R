library(shiny)
library(shinydashboard)


dashboard <- list()

dashboard$header <- dashboardHeader()
dashboard$sidebar <- dashboardSidebar()
dashboard$body <- dashboardBody()

ui <- dashboardPage(
    dashboard$header,
    dashboard$sidebar,
    dashboard$body
)

server <- function(input,ouput,session){

}

runApp(ui,server)
