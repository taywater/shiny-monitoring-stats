#This file, app.R, works with the sourced inlet_conveyance.R file to create the PWD GSI MARS Monitoring Stats app
#See Fieldwork App R script for more details.

# SET UP
#0.0: load libraries --------------
#shiny
library(shiny)
#pool for database connections
library(pool)
#odbc for database connections
library(odbc)
#tidyverse for data manipulations
library(tidyverse)
#shinythemes for colors
library(shinythemes)
#lubridate to work with dates
library(lubridate)
#shinyjs() to use easy java script functions
library(shinyjs)
#DT for datatables
library(DT)
#reactable for reactable tables
library(reactable)

#annual report stuff
library(reactablefmtr)
library(shinydashboard)
library(openxlsx)


#0.1: database connection and global options --------

#set default page length for datatables
options(DT.options = list(pageLength = 15))

#set db connection
#using a pool connection so separate connnections are unified
#gets environmental variables saved in local or pwdrstudio environment
poolConn <- dbPool(odbc(), dsn = "mars14_datav2", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"))
#disconnect from db on stop 
onStop(function(){
  poolClose(poolConn)
})

#js warning about leaving page
jscode <- 'window.onbeforeunload = function() { return "Please use the button on the webpage"; };'

#0.2 source scripts.  ----
#each script contains a module, which includes UI and server code
source("monitoring_stats.R")
source("quarterly_report.R")
source("annual_report.R")



#1: UI FUNCTION -----
#initialize variables for UI and call all UI functions
#call all the UI functions

ui <- function(req){
  
  #1.1: load required variables -----
  #define global variables that will be required each time the UI runs
  
  #monitoring stats
  #get the current fiscal year and list of all years from start of data (2012) to now
  current_fy <- lubridate::today() %m+% months(6) %>% year()
  start_fy <- 2012
  years <- start_fy:current_fy %>% sort(decreasing = TRUE)
  
  # 1.2: actual UI------------------------
  
  #use tagList so tags and shinyjs can be called without being inside of the navbarPage. When they're inside navbarpage, they create small invisible fake tabs that take up space and act weird when clicked on
  tagList(
    #call jscode to warn when leaving page
    tags$head(tags$script(jscode)),
    #must call useShinyjs() for shinyjs() functionality to work in app
    useShinyjs(),
    navbarPage("Monitoring Stats",  id = "inTabset", theme = shinytheme("cerulean"),
      #Stats
      m_statsUI("stats", current_fy = current_fy, years = years),
      #Quarterly Report
      q_reportUI("q_report", current_fy = current_fy, years = years),
      #Annual Report
      a_reportUI("a_report", current_fy = current_fy, years = years)
      
      
    )
  )
  
}

# 2: server function ----
#call modules, referencing the UI names above. These are functions, so any data originating outside the function needs to be named as an argument, whether it is lookup data, or from another tab. Modules need to be assigned to variables so they can be used in other module functions. 
server <- function(input, output, session) {
  
  # 2.1: required variables -----
  #define global variables that will be defined each time server runs
  
  #monitoring stats
  current_fy <- lubridate::today() %m+% months(6) %>% year()
  
  # 2.2: Server Module functions ---------------------------
  #Stats
  stats <- m_statsServer("stats", parent_session = session, current_fy = current_fy, poolConn = poolConn)
  
  #Quarterly Report
  q_report <- q_reportServer("q_report", parent_session = session, current_fy = current_fy, poolConn = poolConn)
  
  #Annual report
  a_report <- a_reportServer("a_report", parent_session = session, current_fy = current_fy, poolConn = poolConn)
  
  
}

#Run this function to run the app!
shinyApp(ui, server)