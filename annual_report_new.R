#Quarterly Report
#Select Quarter Only 
#Table will match what is in the quarterly report

#1.0 UI --------
a_reportUI <- function(id, label = "a_report", current_fy, years){
  ns <- NS(id)
  tabPanel(title = "Annual Report", value = "a_report",
           fluidPage(#theme = shinytheme("cerulean"),
             titlePanel("Annual Report Counts"), 
             #1.1 General Inputs
             sidebarPanel(
               fluidRow(column(12, selectInput(ns("fy"), "Fiscal Year (FY)", choices = years))
               ),
               #1.2 Buttons
               fluidRow(column(6,
                               actionButton(ns("table_button"), "Generate Table")),
                        column(6, 
                               shinyjs::disabled(downloadButton(ns("download_table"), "Download xlsx")))
                        
               ), width = 3),
             #1.3 Main Panel (Outputs) -----
             mainPanel(
               
               
               strong("Table 5-1: Summary of Post-Construction CWL Monitoring of Public SMPs"),
               reactableOutput(ns("Summary of Post-Construction CWL Monitoring of Public SMPs"))
               
               
             )
           )
  )
  
}

#2.0 Server -----
a_reportServer <- function(id, parent_session, current_fy, poolConn){
  moduleServer(
    id, 
    function(input, output, session){
      
      #reactive FY start and END
      FYSTART_reactive <- reactive({
        fystart_string <-"%s-07-01 00:00:00"
        FYSTART <- paste(sprintf(fystart_string, as.character(as.numeric(input$fy)-1)),collapse="")
        return(FYSTART)
      })
      
      FYEND_reactive <- reactive({
        fyend_string <-"%s-06-30 11:59:59"
        FYEND <- paste(sprintf(fyend_string, input$fy),collapse="")
        return(FYEND)
      })
      
      
      
      #2.2 observe event --------
      observeEvent(input$table_button, {
        
        #enable downloading after table in generated
        enable("download_table")
        
        #Reactive table poplutions here-all outputs must be reactive dataframes
        
        table_5_1 <- reactive({
          
          #Public sensors deployed this FY
          fy_public_sensors_deployed <- "select count(*) from fieldwork.viw_deployment_full_cwl
                            where (collection_dtime > '%s' OR collection_dtime is null)
                            and deployment_dtime between '%s' and '%s'
                            and public =  TRUE"
          
          fy_public_sensors_deployed_poolConn <- dbGetQuery(poolConn, paste(sprintf(fy_public_sensors_deployed, 
                                                                            FYSTART_reactive(),
                                                                            FYSTART_reactive(), 
                                                                            FYEND_reactive()),
                                                                    collapse="")) 
          
          #Public systems monitored this FY
          fy_public_systems_monitored <- "select count(distinct admin.fun_smp_to_system(d.smp_id)) 
		                                from fieldwork.viw_deployment_full_cwl d
		                                where d.public = true and
		                                (deployment_dtime between '%s' and '%s'
		                                or collection_dtime between '%s' and '%s'
		                                or (deployment_dtime < '%s' and collection_dtime is null))"
          fy_public_systems_monitored_poolConn <- dbGetQuery(poolConn, 
                                                         paste(sprintf(fy_public_systems_monitored, 
                                                                       FYSTART_reactive(), 
                                                                       FYEND_reactive(), 
                                                                       FYSTART_reactive(), 
                                                                       FYEND_reactive(), 
                                                                       FYSTART_reactive()), 
                                                               collapse="")) 
          
          #Public systems newly monitored this FY
          fy_public_systems_newly_monitored <- "select count(*) from 
		                                  fieldwork.viw_first_deployment_cwl f where
		                                  public = true and
		                                  first_deployment between '%s' and '%s'"
          
          fy_public_systems_newly_monitored_poolConn <- dbGetQuery(poolConn, 
                                                               paste(sprintf(fy_public_systems_newly_monitored, 
                                                                             FYSTART_reactive(), 
                                                                             FYEND_reactive()),
                                                                     collapse=""))
          
          #Sensors deployed to date
          todate_public_sensors_deployed <- "select count(*) from fieldwork.viw_deployment_full_cwl
	                                                  where deployment_dtime <= '%s'
	                                                  and public = TRUE"
          
          todate_public_sensors_deployed_poolConn <- dbGetQuery(poolConn, paste(sprintf(todate_public_sensors_deployed,
                                                                                FYEND_reactive()),
                                                                        collapse=""))
          
          #Public systems monitored to date
          todate_public_systems_monitored <- "select count(distinct admin.fun_smp_to_system(d.smp_id)) 
	                                          from fieldwork.viw_deployment_full_cwl d
	                                          where deployment_dtime <= '%s'
	                                          and d.public = true"
          
          todate_public_systems_monitored_poolConn <- dbGetQuery(poolConn, 
                                                             paste(sprintf(todate_public_systems_monitored,
                                                                           FYEND_reactive()),
                                                                   collapse=""))
          
          #Assembling output table
          public_postcon_cwl <- data.frame("fy" = rep(NA, 3), "todate" = rep(NA, 3))
          public_postcon_cwl$fy <- c(fy_public_sensors_deployed_poolConn$count, #Public sensors deployed
                                     fy_public_systems_monitored_poolConn$count, #Public systems monitored
                                     fy_public_systems_newly_monitored_poolConn$count) #Public systems newly monitored
          
          public_postcon_cwl$todate <- c(todate_public_sensors_deployed_poolConn$count, #Public sensors deployed
                                         todate_public_systems_monitored_poolConn$count, #Public systems monitored
                                         NA) #Public systems newly monitored is only defined for the FY
          
          colnames(public_postcon_cwl)<- c("This Fiscal Year","To Date")
          rownames(public_postcon_cwl)<-c("Sensors Deployed","Systems Monitored","Systems Newly Monitored")
          
          return(public_postcon_cwl)
          
          
        })
        
        
        #ractable table outputs
        output$`Summary of Post-Construction CWL Monitoring of Public SMPs` <- renderReactable(reactable(table_5_1(), striped = TRUE))
        output$help_text <- renderText({
          paste("A Shiny App to Populate the Annual Report Stats" , 
                "First Version Published on 08/05/2022 by Farshad Ebrahimi",
                sep="\n")
        })
        
        output$download_table <- downloadHandler(
          
          filename = function() {
            paste("FY",input$fy,"_","AnnualReport","_",Sys.Date(),".xlsx", sep = "")
          },
          content = function(filename){
            
            df_list <- list(Table_3_1=table_5_1())
            write.xlsx(x = df_list , file = filename, rowNames = TRUE)
          }
        ) 
        
        
      })
      
      
    }
  )
}






