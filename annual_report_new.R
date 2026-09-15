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
                reactableOutput(ns("Summary of Post-Construction CWL Monitoring of Public SMPs")),
                
              strong("Table 5-2: Post-Construction CWL Monitoring of Public SMPs Listed by Type"),
                reactableOutput(ns("Post-Construction CWL Monitoring of Public SMPs Listed by Type")) ,
                
              strong("Table 5-3: Post-Construction SRTs performed on Public Systems"),
                reactableOutput(ns("Post-Construction SRTs performed on Public Systems")),
                
              strong("Table 5-4: Public Systems with Post-Construction SRTs Performed"),
                reactableOutput(ns("Public Systems with Post-Construction SRTs Performed")),
                
              strong("Table 5-5: Construction-Phase SRTs Performed on Public Systems"),
                reactableOutput(ns("Construction-Phase SRTs Performed on Public Systems")) #,
                
              # strong("Table 5-6: Public Systems with Construction-Phase SRTs Performed"),
              #   reactableOutput(ns("Public Systems with Construction-Phase SRTs Performed")),
                
              # strong("Table 5-7: Public Systems with CETs Administered"),
              #   reactableOutput(ns("Public Systems with CETs Administered")),
                
              # strong("Table 5-8: Public Systems with Infiltration Testing Administered"),
              #   reactableOutput(ns("Public Systems with Infiltration Testing Administered")),
                
              # strong("Table 5-9: Public Systems with Inlet Leakage Tests Administered"),
              #   reactableOutput(ns("Public Systems with Inlet Leakage Tests Administered")),
                
              # strong("Table 5-10: Inlet Conveyance Tests Performed on Public Systems"),
              #   reactableOutput(ns("Inlet Conveyance Tests Performed on Public Systems")),
                
              # strong("Table 5-11: Groundwater Monitoring for Public GSI"),
              #   reactableOutput(ns("Groundwater Monitoring for Public GSI")),
                
              # strong("Table 6-1: Summary of Post-Construction CWL Monitoring of Private Systems"),
              #   reactableOutput(ns("Summary of Post-Construction CWL Monitoring of Private Systems")),
                
              # strong("Table 6-2: Post-Construction CWL Monitoring of Private Systems Listed by Type"),
              #   reactableOutput(ns("Post-Construction CWL Monitoring of Private Systems Listed by Type")),
                
              # strong("Table 6-3: Post-Construction SRTs performed on Private Systems"),
              #   reactableOutput(ns("Post-Construction SRTs performed on Private Systems")),
                
              # strong("Table 6-4: Private SMPs with Post-Construction SRTs Performed"),
              #   reactableOutput(ns("Private SMPs with Post-Construction SRTs Performed")),
                
              # strong("Table 6-5: Private Systems with CETs Administered"),
              #   reactableOutput(ns("Private Systems with CETs Administered")),
                
              # strong("Table 6-6: Private Systems with ICTs Administered"),
              #   reactableOutput(ns("Private Systems with ICTs Administered")),
                
              # strong("Table 6-7: Private Systems with WWIs Administered"),
              #   reactableOutput(ns("Private Systems with WWIs Administered"))
               
               
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

        table_5_2 <- reactive({
          #Public systems monitored by type todate
          todate_public_systems_monitored_bytype <- "select sfc.asset_type, count(distinct(d.smp_id)), d.public from
                                                            fieldwork.viw_deployment_full_cwl d
                                                            left join external.mat_assets sfc on d.smp_id = sfc.smp_id
                                                            where sfc.component_id is null
                                                            and d.smp_id is not null
                                                            and d.deployment_dtime < '%s'
                                                            and d.public = true
                                                            group by sfc.asset_type, d.public"

          #Query monitored systems and recode MARS name to GreenIT name
          todate_public_systems_monitored_bytype_prod <- dbGetQuery(poolConn, 
                                                                    paste(sprintf(todate_public_systems_monitored_bytype,
                                                                                  FYEND_reactive()),
                                                                          collapse="")) |>
            mutate(asset_type = fct_recode(asset_type, "Infiltration/Storage Trench" = "Trench"))


          #Public systems constructed by type to date
          #cipit statuses indicating constructed systems are Jillian Simmons's best recommendation
          todate_public_systems_constructed_bytype <- "select count(*), smp_smptype from external.tbl_smpbdv g 
                                                        where g.smp_notbuiltretired is null 
                                                        and (g.cipit_status = 'Closed' 
                                                        or g.cipit_status = 'Construction-Substantially Complete' 
                                                        or g.cipit_status = 'Construction-Contract Closed') 
                                                        group by smp_smptype"

          #Query constructed systems and recode GreenIT name to MARS name
          todate_public_systems_constructed_bytype_prod <- dbGetQuery(poolConn,
                                                                      todate_public_systems_constructed_bytype) |>
            mutate(smp_smptype = fct_recode(smp_smptype, "Permeable Pavement" = "Pervious Paving")) 
          

          #Join and assemble table
          todate_public_prod <- todate_public_systems_constructed_bytype_prod |> 
            left_join(todate_public_systems_monitored_bytype_prod, 
                      by=c("smp_smptype" = "asset_type"), 
                      suffix = c(".constructed", ".monitored")) |>
            transmute(`SMP Type` = smp_smptype, 
                      Description = NA,
                      `Monitored SMPs` = replace_na(count.monitored, 0),
                      `Total Constructed Public SMPs` = count.constructed)

          #Add descriptions
          todate_public_prod$Description[todate_public_prod$`SMP Type` == "Infiltration/Storage Trench"] <- "Also listed as Trench"
          todate_public_prod$Description[todate_public_prod$`SMP Type` == "Permeable Pavement"] <- "Also listed as Pervious Paving"

          return(todate_public_prod)

        })

        table_5_3 <- reactive({
          #Post-construction public SRTs this FY
          fy_public_postcon_srt <- "select count(*), type from fieldwork.viw_srt_full 
                                    where test_date >= '%s'
                                    and test_date <= '%s'
                                    and phase = 'Post-Construction'
                                    and public = TRUE
                                    group by type"

          fy_public_postcon_srt_prod <-dbGetQuery(poolConn, 
                                                  paste(sprintf(fy_public_postcon_srt, 
                                                                FYSTART_reactive(), 
                                                                FYEND_reactive()),
                                                        collapse=""))

          #Post-construction public SRTsto date
          todate_public_postcon_srt <-"select count(*), type from fieldwork.viw_srt_full 
                                                                    where test_date <= '%s'
                                                                    and phase = 'Post-Construction'
                                                                    and public = TRUE
                                                                    group by type"

          todate_public_postcon_srt_prod <- dbGetQuery(poolConn, 
                                                       paste(sprintf(todate_public_postcon_srt,
                                                                     FYEND_reactive()),
                                                             collapse=""))
          
          #Assembling output table
          public_postcon_srt <- left_join(todate_public_postcon_srt_prod,
                                          fy_public_postcon_srt_prod,
                                          by = "type",
                                          suffix = c(".todate", ".fy"))

          rownames(public_postcon_srt)<- public_postcon_srt$type
          public_postcon_srt <- transmute(public_postcon_srt,
                                       "This Fiscal Year" = replace_na(count.fy, 0),
                                       "To Date" = count.todate)

          return(public_postcon_srt)

        })

        table_5_4 <- reactive({
          #Public systems with post-construction srts this FY
          fy_public_postcon_srt_systems <-"select sfc.asset_type, count(distinct(srt.system_id))
                                              from fieldwork.viw_srt_full srt
                                              left join external.mat_assets sfc on srt.system_id = sfc.system_id
                                              where sfc.component_id is null
                                              and test_date >= '%s'
                                              and test_date <= '%s'
                                              and phase = 'Post-Construction'
                                              and public = TRUE
                                              group by sfc.asset_type"

          fy_public_postcon_srt_systems_prod <-dbGetQuery(poolConn, 
                                                          paste(sprintf(fy_public_postcon_srt_systems, 
                                                                        FYSTART_reactive(), 
                                                                        FYEND_reactive()),
                                                                collapse=""))

          #Public Systems with Post-Construction SRTs Performed TO DATE
          todate_public_postcon_srt_systems <-"select sfc.asset_type, count(distinct(srt.system_id))
                                                            from fieldwork.viw_srt_full srt
                                                            left join external.mat_assets sfc on srt.system_id = sfc.system_id
                                                            where sfc.component_id is null
                                                            and test_date <= '%s'
                                                            and phase = 'Post-Construction'
                                                            and public = TRUE
                                                            group by sfc.asset_type"

          todate_public_postcon_srt_systems_prod <-dbGetQuery(poolConn, 
                                                              paste(sprintf(todate_public_postcon_srt_systems, 
                                                                            FYEND_reactive()),
                                                                    collapse=""))

          #Assembling output table
          public_postcon_srt_bysystem <- left_join(todate_public_postcon_srt_systems_prod,
                                          fy_public_postcon_srt_systems_prod,
                                          by = "asset_type",
                                          suffix = c(".todate", ".fy"))

          rownames(public_postcon_srt_bysystem)<- public_postcon_srt_bysystem$asset_type
          public_postcon_srt_bysystem <- transmute(public_postcon_srt_bysystem,
                                       "This Fiscal Year" = replace_na(count.fy, 0),
                                       "To Date" = count.todate)

          return(public_postcon_srt_bysystem)
        })


        table_5_5 <- reactive({

          #Mid-construction SRTs performed on Public Systems this FY
          fy_public_midcon_srt <- "select count(*), type
                                      from fieldwork.viw_srt_full 
                                      where test_date >= '%s'
                                      and test_date <= '%s'
                                      and phase = 'Construction'
                                      and public = TRUE
                                      group by type"
          fy_public_midcon_srt_prod <- dbGetQuery(poolConn, 
                                                  paste(sprintf(fy_public_midcon_srt, 
                                                                FYSTART_reactive(), 
                                                                FYEND_reactive()),
                                                        collapse=""))



          #Mid-construction SRTs performed on Public Systems to date
          todate_public_midcon_srt <- "select count(*), type
                                    from fieldwork.viw_srt_full 
                                    where test_date <= '%s'
                                    and phase = 'Construction'
                                    and public = TRUE
                                    group by type"

          todate_public_midcon_srt_prod <- dbGetQuery(poolConn, 
                                                      paste(sprintf(todate_public_midcon_srt, 
                                                                    FYEND_reactive()),
                                                            collapse=""))

          #Assembling output table
          public_midcon_srt <- left_join(todate_public_midcon_srt_prod,
                                          fy_public_midcon_srt_prod,
                                          by = "type",
                                          suffix = c(".todate", ".fy"))

          rownames(public_midcon_srt)<- public_midcon_srt$type
          public_midcon_srt <- transmute(public_midcon_srt,
                                       "This Fiscal Year" = replace_na(count.fy, 0),
                                       "To Date" = count.todate)

          return(public_midcon_srt)
        })

        
        #reactable table outputs
        output$`Summary of Post-Construction CWL Monitoring of Public SMPs` <- renderReactable(reactable(table_5_1(), striped = TRUE, pagination = FALSE))
        output$`Post-Construction CWL Monitoring of Public SMPs Listed by Type` <- renderReactable(reactable(table_5_2(), striped = TRUE, pagination = FALSE))
        output$`Post-Construction SRTs performed on Public Systems` <- renderReactable(reactable(table_5_3(), striped = TRUE, pagination = FALSE))
        output$`Public Systems with Post-Construction SRTs Performed` <- renderReactable(reactable(table_5_4(), striped = TRUE, pagination = FALSE))
        output$`Construction-Phase SRTs Performed on Public Systems` <- renderReactable(reactable(table_5_5(), striped = TRUE, pagination = FALSE))
        # output$`Public Systems with Construction-Phase SRTs Performed` <- renderReactable(reactable(table_5_6(), striped = TRUE, pagination = FALSE))
        # output$`Public Systems with CETs Administered` <- renderReactable(reactable(table_5_7(), striped = TRUE, pagination = FALSE))
        # output$`Public Systems with Infiltration Testing Administered` <- renderReactable(reactable(table_5_8(), striped = TRUE, pagination = FALSE))
        # output$`Public Systems with Inlet Leakage Tests Administered` <- renderReactable(reactable(table_5_9(), striped = TRUE, pagination = FALSE))
        # output$`Inlet Conveyance Tests Performed on Public Systems` <- renderReactable(reactable(table_5_10(), striped = TRUE, pagination = FALSE))
        # output$`Groundwater Monitoring for Public GSI` <- renderReactable(reactable(table_5_11(), striped = TRUE, pagination = FALSE))
        # output$`Summary of Post-Construction CWL Monitoring of Private Systems` <- renderReactable(reactable(table_6_1(), striped = TRUE, pagination = FALSE))
        # output$`Post-Construction CWL Monitoring of Private Systems Listed by Type` <- renderReactable(reactable(table_6_2(), striped = TRUE, pagination = FALSE))
        # output$`Post-Construction SRTs performed on Private Systems` <- renderReactable(reactable(table_6_3(), striped = TRUE, pagination = FALSE))
        # output$`Private SMPs with Post-Construction SRTs Performed` <- renderReactable(reactable(table_6_4(), striped = TRUE, pagination = FALSE))
        # output$`Private Systems with CETs Administered` <- renderReactable(reactable(table_6_5(), striped = TRUE, pagination = FALSE))
        # output$`Private Systems with ICTs Administered` <- renderReactable(reactable(table_6_6(), striped = TRUE, pagination = FALSE))
        # output$`Private Systems with WWIs Administered` <- renderReactable(reactable(table_6_7(), striped = TRUE, pagination = FALSE))
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






