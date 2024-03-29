#Monitoring Stats
#Exports a csv for each table
#use reactive variables to create a new query for each combination

#1.0 UI ---------
  m_statsUI <- function(id, label = "stats", current_fy, years){
    ns <- NS(id)
    tabPanel(title = "Monitoring Stats", value = "m_stats",
    fluidPage(#theme = shinytheme("cerulean"),
               titlePanel("Monitoring Stats"),
             #1.1 General Inputs ----
                sidebarPanel(
                  selectInput(ns("date_range"), "Date Range", choices = c("To-Date", "Select Range")),
                  conditionalPanel(condition = "input.date_range == 'Select Range'", 
                                   ns = ns, 
                          fluidRow(column(6,
                          selectInput(ns("start_fy"), "Start Fiscal Year (FY)", choices = years)),
                          column(6,selectInput(ns("start_quarter"), "Start Fiscal Quarter", 
                                               choices = c("Q1" = "7/1", "Q2" = "10/1","Q3" = "1/1", "Q4" = "4/1")))),
                          fluidRow(column(6,
                            selectInput(ns("end_fy"), "End Fiscal Year (FY)", choices = years)),
                            column(6,selectInput(ns("end_quarter"), "End Fiscal Quarter", 
                                                 choices = c("Q1" = "9/30", "Q2" = "12/31","Q3" = "3/31", "Q4" = "6/30"))))
                          ), 
                  selectInput(ns("phase"), "Construction Phase", choices = c("Construction", "Post-Construction"), selected = "Post-Construction"),
                  selectInput(ns("public_private"), "Ownership (for field tests)", choices = c("Public and Private", "Public", "Private")),
                  checkboxInput(ns("cet_checkbox"), "Capture Efficiency Test Options"), 
                  #1.2 CET inputs ----
                    #show options for capture efficiency if the box is checked
                    conditionalPanel(condition = "input.cet_checkbox", 
                                     ns = ns, 
                                     selectInput(ns("inlet_type"), "Inlets", choices = c("All", "Inlets", "Curbcuts")), 
                                     selectInput(ns("unique_inlets"), "Unique Inlets", choices = c("Include All Tests", "Most Recent for Each Inlet")) 
                                     ),
                  #1.3 Buttons --------
                    fluidRow(column(8,
                            actionButton(ns("table_button"), "Generate System and SMP Stats")),
                            column(4, 
                            shinyjs::disabled(downloadButton(ns("download_table"), "Download")))),
                    fluidRow(column(8,
                              actionButton(ns("postcon_button"), "Generate Post-Construction Stats")),
                             column(4, 
                                    disabled(downloadButton(ns("download_postcon"), "Download")))),
                    #show options for capture efficiency if the box is checked
                    conditionalPanel(condition = "input.cet_checkbox", 
                                     ns = ns, 
                                     fluidRow(column(8, 
                                              actionButton(ns("cet_button"), "Generate Capture Efficiency Stats")), 
                                              column(4,
                                              disabled(downloadButton(ns("download_cet"), "Download")))))
                  
                ), 
             #1.4 Main Panel (Outputs) -----
               mainPanel(
                 h4(textOutput(ns("table_name"))), 
                    DTOutput(ns("table")), 
                 h4(textOutput(ns("postcon"))),
                    DTOutput(ns("postcon_table")),
                 h4(textOutput(ns("cet_name"))), 
                    DTOutput(ns("cet_table"))
                 )
             
    )
    )
  }
  
#2.0 Server ----
m_statsServer <- function(id, parent_session, current_fy, poolConn){
  moduleServer(
    id, 
    function(input, output, session){
  
  
      #define ns to use in modals
      ns <- session$ns
      
      #make sure end_fy is after start_fy
      observe(updateSelectInput(session, "end_fy", choices = current_fy:input$start_fy))
      #updates stats button based on con or post-con
      observe(updateActionButton(session, "postcon_button", label = paste("Generate", input$phase, "Stats")))
      
      #initialzie reactive values
      rv <- reactiveValues()
      
      #create a date style for headers
      sf <- lubridate::stamp("March 1, 1999", orders = "%B %d, %Y")
      
      #convert FY/Quarter to a real date
      rv$start_date <- reactive(lubridate::mdy(paste0(input$start_quarter, "/", ifelse(input$start_quarter == "7/1" | input$start_quarter == "10/1", as.numeric(input$start_fy)-1,input$start_fy))))
      rv$end_date <- reactive(lubridate::mdy(paste0(input$end_quarter, "/", ifelse(input$end_quarter == "9/30" | input$end_quarter == "12/31", as.numeric(input$end_fy)-1,input$end_fy))))
      
      #system and smp
      observeEvent(input$table_button, {
        #enable downloading after table is generated
        enable("download_table")
        #if "To-Date"
        if(input$date_range == "To-Date"){
          #name table in text output
          rv$table_name <- "CWL To Date"
          output$table_name <- renderText(rv$table_name)
          #bind rows for public and private systems to date
          rv$cwl_to_date_table <- bind_rows(
            rv$public_systems_monitored_to_date(),
            rv$private_systems_monitored_to_date()
          )
          #output this table
          output$table <- renderDT(
            rv$cwl_to_date_table,
            options = list(dom = 't')
          )
        }else{
          #if a range is given
          #name table in text output
          rv$table_name <- paste("CWL", sf(rv$start_date()), "to", sf(rv$end_date()))
          output$table_name <- renderText(rv$table_name)
          #bind public systems, smps, private systems, long term, short term, new systems, new smps, hobos deployed
          rv$cwl_table <- bind_rows(
            rv$public_systems_monitored(),
            rv$public_smps_monitored(),
            rv$private_systems_monitored(),
            rv$long_term_systems_monitored(),
            rv$short_term_systems_monitored(),
            rv$new_systems_monitored(),
            rv$new_smps_monitored(),
            rv$hobos_deployed(),
            rv$hobos_deployed_private()
          )
          #output this table
          output$table <- renderDT(
            rv$cwl_table,
            options = list(dom = 't')
          )
           }
        }
      )
      
      #get dange range to text
      rv$select_range_tests_title_text <- reactive(if(input$date_range == "Select Range"){
        paste(sf(rv$start_date()), "to", sf(rv$end_date()))
      }else{
        paste("To Date")
      })
      
      #post con
      #create postcon table and header when postcon button is clicked
      observeEvent(input$postcon_button, {
        enable("download_postcon")
        rv$postcon_name <- paste(input$phase, input$public_private, "Tests", rv$select_range_tests_title_text())
          output$postcon <- renderText(rv$postcon_name)
          rv$postcon_table <- bind_rows(
            rv$pre_inspection(), 
            rv$systems_tested_pre_inspection_srt(),
            rv$performance_srts(),
            rv$systems_tested_performance_srt(),
            rv$cctv_dye_test(),
            rv$systems_tested_cctv_dye_test(),
            rv$systems_tested_srt(),
            rv$systems_tested_ppt(), 
            rv$systems_tested_cet()
          )
          output$postcon_table <- renderDT(
            rv$postcon_table, 
            options = list(dom = 't')
          )
      })
    
      #header for inlets
      rv$unique_inlet_title_text <- reactive(if(input$unique_inlets == "Include All Tests"){ 
        paste("All")
      }else if(input$unique_inlets == "Most Recent for Each Inlet"){
        paste("Unique")
      })
      
      #create cet header and table when button is clicked
      observeEvent(input$cet_button, {
        #print("starting_cet")
        enable("download_cet")
        rv$cet_name <- paste(rv$unique_inlet_title_text(), input$phase, input$inlet_type, input$public_private, "Capture Efficiency Stats", rv$select_range_tests_title_text())
        output$cet_name <- renderText(rv$cet_name)
        rv$cet_table <- bind_rows(
          rv$cet_lf(),
          rv$cet_hf(),
          rv$cet_no_lf_bypass(),
          rv$cet_no_hf_bypass(),
          rv$cet_10pct_lf_bypass(),
          rv$cet_10pct_hf_bypass()
        )
        output$cet_table <- renderDT(
          rv$cet_table,
          options = list(dom = 't')
        )
      } )
      
      #download button for system/ SMP table
      output$download_table <- downloadHandler(
        filename = function(){
          paste(rv$table_name, "_", Sys.Date(), ".csv", sep = "")
        }, 
        content = function(file){
          write.csv(rv$cwl_table, file, row.names = FALSE)
        }
      )
      
      #download button for postcon table
      output$download_postcon <- downloadHandler(
        filename = function(){
          paste(rv$postcon_name, "_", Sys.Date(), ".csv", sep = "")
        }, 
        content = function(file){
          write.csv(rv$postcon_table, file, row.names = FALSE)
        }
      )
      
      #download button for CET table
      output$download_cet <- downloadHandler(
        filename = function(){
          paste(rv$cet_name, "_", Sys.Date(), ".csv", sep = "")
        }, 
        content = function(file){
          write.csv(rv$cet_table, file, row.names = FALSE)
        }
      )
      
      # System ----------------------------------------------------------
      
      #systems
      # No. of public systems monitored
      # rv$public_systems_monitored_q <- reactive(paste0("SELECT COUNT(DISTINCT admin.fun_smp_to_system(d.smp_id)) FROM fieldwork.viw_deployment_full_cwl d 
      #                                               WHERE ((d.deployment_dtime_est >= '", rv$start_date(), "' AND d.deployment_dtime_est <= '", rv$end_date(), "')
      #                                               OR (d.collection_dtime_est <= '", rv$end_date(), "' AND d.collection_dtime_est >= '", rv$start_date(), "')
      #                                               OR (d.deployment_dtime_est <= '", rv$end_date(), "' AND d.collection_dtime_est is NULL))
      #                                               AND d.public = TRUE"))
      
      rv$public_systems_monitored_q <- reactive(paste0("SELECT COUNT(DISTINCT admin.fun_smp_to_system(d.smp_id)) FROM fieldwork.viw_deployment_full_cwl d
                                                       WHERE deployment_dtime_est <= '", rv$end_date(), "'
                                                    AND (collection_dtime_est >= '", rv$start_date(), "' 
                                             OR collection_dtime_est IS NULL) 
                                                       AND d.public = TRUE"))
      
      
      rv$public_systems_monitored_value <- reactive(dbGetQuery(poolConn, rv$public_systems_monitored_q()))
      rv$public_systems_monitored <- reactive(data.frame(Metric = as.character("Public Systems Monitored"), Count = rv$public_systems_monitored_value()))
      
      
      
      #No. of longterm systems monitored
      
      rv$long_term_systems_monitored_q <- reactive(paste0("SELECT COUNT(DISTINCT admin.fun_smp_to_system(d.smp_id)) FROM fieldwork.viw_deployment_full_cwl d
                                                       WHERE deployment_dtime_est <= '", rv$end_date(), "'
                                                    AND (collection_dtime_est >= '", rv$start_date(), "' 
                                             OR collection_dtime_est IS NULL) 
                                             AND d.term = 'Long'
                                                       AND d.public = TRUE"))
      
      
      rv$long_term_systems_monitored_value <- reactive(dbGetQuery(poolConn, rv$long_term_systems_monitored_q()))
      rv$long_term_systems_monitored <- reactive(data.frame(Metric = as.character("Public Long Term Systems Monitored"), Count = rv$long_term_systems_monitored_value()))
      
      
      #No. of short term systems monitored
      rv$short_term_systems_monitored_q <- reactive(paste0("SELECT COUNT(DISTINCT admin.fun_smp_to_system(d.smp_id)) FROM fieldwork.viw_deployment_full_cwl d
                                                       WHERE deployment_dtime_est <= '", rv$end_date(), "'
                                                    AND (collection_dtime_est >= '", rv$start_date(), "' 
                                             OR collection_dtime_est IS NULL) 
                                             AND d.term = 'Short'
                                                       AND d.public = TRUE"))
      
      rv$short_term_systems_monitored_value <- reactive(dbGetQuery(poolConn, rv$short_term_systems_monitored_q()))
      rv$short_term_systems_monitored <- reactive(data.frame(Metric = as.character("Public Short Term Systems Monitored"), Count = rv$short_term_systems_monitored_value()))
      
      #No. of new systems monitored
      rv$new_systems_monitored_q <- reactive(paste0("SELECT count(distinct admin.fun_smp_to_system(dada.smp_id)) FROM 
                                                  (SELECT d.smp_id FROM fieldwork.viw_deployment_full_cwl d 
                                                    GROUP BY d.smp_id
                                                    HAVING min(d.deployment_dtime_est) >= '", rv$start_date(), "'
                                                    AND min(d.deployment_dtime_est) <= '", rv$end_date(), "') dada"))
      
      rv$new_systems_monitored_value <- reactive(dbGetQuery(poolConn, rv$new_systems_monitored_q()))
      rv$new_systems_monitored <- reactive(data.frame(Metric = as.character("New Systems Monitored"), Count = rv$new_systems_monitored_value()))
      
      #No. of public systems monitored to date
      rv$public_systems_monitored_to_date_q <- reactive(paste0("SELECT COUNT(DISTINCT admin.fun_smp_to_system(d.smp_id)) FROM fieldwork.viw_deployment_full_cwl d 
                                                    WHERE d.public = TRUE"))
      
      rv$public_systems_monitored_to_date_value <- reactive(dbGetQuery(poolConn, rv$public_systems_monitored_to_date_q()))
      rv$public_systems_monitored_to_date <- reactive(data.frame(Metric = as.character("Public Systems Monitored"), 
                                                                 to_date_count = rv$public_systems_monitored_to_date_value()))
      
      #private systems
      # No. of private systems monitored
      rv$private_systems_monitored_q <- reactive(paste0("SELECT COUNT(DISTINCT admin.fun_smp_to_system(d.smp_id)) FROM fieldwork.viw_deployment_full_cwl d
                                                       WHERE deployment_dtime_est <= '", rv$end_date(), "'
                                                    AND (collection_dtime_est >= '", rv$start_date(), "' 
                                             OR collection_dtime_est IS NULL) 
                                                       AND d.public = FALSE"))
      
      rv$private_systems_monitored_value <- reactive(dbGetQuery(poolConn, rv$private_systems_monitored_q()))
      rv$private_systems_monitored <- reactive(data.frame(Metric = as.character("Private Systems Monitored"), Count = rv$private_systems_monitored_value()))
      
      #No. of private systems monitored to date
      rv$private_systems_monitored_to_date_q <- reactive(paste0("SELECT COUNT(DISTINCT admin.fun_smp_to_system(d.smp_id)) FROM fieldwork.viw_deployment_full_cwl d 
                                                    WHERE d.public = FALSE"))
      
      rv$private_systems_monitored_to_date_value <- reactive(dbGetQuery(poolConn, rv$private_systems_monitored_to_date_q()))
      rv$private_systems_monitored_to_date <- reactive(data.frame(Metric = as.character("Private Systems Monitored"), 
                                                                  to_date_count = rv$private_systems_monitored_to_date_value()))
      #smps -----
      
      # No. of public smps monitored
      rv$public_smps_monitored_q <- reactive(paste0("SELECT COUNT(DISTINCT d.smp_id) FROM fieldwork.viw_deployment_full_cwl d
                                                       WHERE deployment_dtime_est <= '", rv$end_date(), "'
                                                    AND (collection_dtime_est >= '", rv$start_date(), "' 
                                             OR collection_dtime_est IS NULL) 
                                                       AND d.public = TRUE"))
      
      rv$public_smps_monitored_value <- reactive(dbGetQuery(poolConn, rv$public_smps_monitored_q()))
      rv$public_smps_monitored <- reactive(data.frame(Metric = as.character("Public SMPs Monitored"), Count = rv$public_smps_monitored_value()))
      
      #No. of new smps monitored
      rv$new_smps_monitored_q <- reactive(paste0("SELECT count(distinct(dada.smp_id)) FROM 
                                                  (SELECT d.smp_id FROM fieldwork.viw_deployment_full_cwl d 
                                                    GROUP BY d.smp_id, d.term 
                                                    HAVING min(d.deployment_dtime_est) >= '", rv$start_date(), "'
                                                    AND min(d.deployment_dtime_est) <= '", rv$end_date(), "') dada"))
      
      rv$new_smps_monitored_value <- reactive(dbGetQuery(poolConn, rv$new_smps_monitored_q()))
      rv$new_smps_monitored <- reactive(data.frame(Metric = as.character("New SMPs Monitored"), Count = rv$new_smps_monitored_value()))
    # sensors -----------------------------------------------------------------
      
      #hobos deployed (public)
      rv$hobos_deployed_q <- reactive(paste0("SELECT COUNT(distinct(sensor_serial)) FROM fieldwork.viw_deployment_full_cwl 
                                             WHERE deployment_dtime_est <= '", rv$end_date(), "'
                                                    AND (collection_dtime_est >= '", rv$start_date(), "' 
                                             OR collection_dtime_est IS NULL) AND 
                                             public = TRUE"))
      
      rv$hobos_deployed_value <- reactive(dbGetQuery(poolConn, rv$hobos_deployed_q()))
      rv$hobos_deployed <- reactive(data.frame(Metric = "Sensors Deployed at Public Sites", 
                                               Count = rv$hobos_deployed_value()))
      
      #hobos deployed (private)
      rv$hobos_deployed_private_q <- reactive(paste0("SELECT COUNT(distinct(sensor_serial)) FROM fieldwork.viw_deployment_full_cwl 
                                             WHERE deployment_dtime_est <= '", rv$end_date(), "'
                                                    AND (collection_dtime_est >= '", rv$start_date(), "' 
                                             OR collection_dtime_est IS NULL) AND 
                                             public = FALSE"))
      
      rv$hobos_deployed_private_value <- reactive(dbGetQuery(poolConn, rv$hobos_deployed_private_q()))
      rv$hobos_deployed_private <- reactive(data.frame(Metric = "Sensors Deployed at Private Sites", 
                                               Count = rv$hobos_deployed_private_value()))
      
      
    
      # post-con / con testing stats ----------------------------------------------------------------
      
      rv$select_range_tests_query_text <- reactive(if(input$date_range == "Select Range"){
        paste("AND test_date >= '", rv$start_date(), "' AND
                                             test_date <= '", rv$end_date(), "'")
      }else{
        paste("")
      })
      
      rv$public_query_text <- reactive(if(input$public_private == "Public and Private"){
        paste("")
      }else if(input$public_private == "Public"){
        paste("AND public = TRUE")
      }else if(input$public_private == "Private"){
        paste("AND public = FALSE")
      })
      
      #SRTs (by selected date)
      #Pre-Inspection SRTs
      rv$pre_inspection_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.viw_srt_full 
                                             WHERE phase = '", input$phase, "' AND
                                             type = 'Pre-Inspection Dye Test'", rv$select_range_tests_query_text(), rv$public_query_text()))
      
      rv$pre_inspection_value <- reactive(dbGetQuery(poolConn, rv$pre_inspection_q()))
      
      rv$pre_inspection <- reactive(data.frame(Metric = "Pre-Inspection Dye Tests", 
                                               Count = rv$pre_inspection_value()))
      
      #systems tested: Pre-Inspection SRT
      rv$systems_tested_pre_inspection_srt_q <- reactive(paste0("SELECT COUNT(distinct system_id) FROM fieldwork.viw_srt_full 
            WHERE phase = '", input$phase, "' AND
                    type = 'Pre-Inspection Dye Test'", rv$select_range_tests_query_text(), rv$public_query_text()))
      
      rv$systems_tested_pre_inspection_srt_value <- reactive(dbGetQuery(poolConn, rv$systems_tested_pre_inspection_srt_q()))
      
      rv$systems_tested_pre_inspection_srt <- reactive(data.frame(Metric = "Systems Tested - Pre-Inspection Dye Test", 
                                                   Count = rv$systems_tested_pre_inspection_srt_value()))
      
      #Performance SRTs
      rv$performance_srts_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.viw_srt_full 
                                             WHERE phase = '", input$phase, "' AND
                                             type = 'Performance Test'", rv$select_range_tests_query_text(), rv$public_query_text()))
      
      rv$performance_srts_value <- reactive(dbGetQuery(poolConn, rv$performance_srts_q()))
      
      rv$performance_srts <- reactive(data.frame(Metric = "Performance SRTs", 
                                                 Count = rv$performance_srts_value()))
      
      #systems tested: Performance SRT
      rv$systems_tested_performance_srt_q <- reactive(paste0("SELECT COUNT(distinct system_id) FROM fieldwork.viw_srt_full 
            WHERE phase = '", input$phase, "' AND
                    type = 'Performance Test'", rv$select_range_tests_query_text(), rv$public_query_text()))
      
      rv$systems_tested_performance_srt_value <- reactive(dbGetQuery(poolConn, rv$systems_tested_performance_srt_q()))
      
      rv$systems_tested_performance_srt <- reactive(data.frame(Metric = "Systems Tested - Performance SRT", 
                                                                  Count = rv$systems_tested_performance_srt_value()))
      
      #CCTV/Dye Tests
      rv$cctv_dye_test_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.viw_srt_full 
                                             WHERE phase = '", input$phase, "' AND
                                             type = 'CCTV Dye Test'", rv$select_range_tests_query_text(), rv$public_query_text()))
      
      rv$cctv_dye_test_value <- reactive(dbGetQuery(poolConn, rv$cctv_dye_test_q()))
      
      rv$cctv_dye_test <- reactive(data.frame(Metric = "CCTV Dye Tests", 
                                              Count = rv$cctv_dye_test_value()))
      
      #systems tested: CCTV/Dye Tests
      rv$systems_tested_cctv_dye_test_q <- reactive(paste0("SELECT COUNT(distinct system_id) FROM fieldwork.viw_srt_full 
            WHERE phase = '", input$phase, "' AND
                    type = 'CCTV Dye Test'", rv$select_range_tests_query_text(), rv$public_query_text()))
      
      rv$systems_tested_cctv_dye_test_value <- reactive(dbGetQuery(poolConn, rv$systems_tested_cctv_dye_test_q()))
      
      rv$systems_tested_cctv_dye_test <- reactive(data.frame(Metric = "Systems Tested - CCTV Dye Test", 
                                                             Count = rv$systems_tested_cctv_dye_test_value()))
      
      #systems tested: SRT
      rv$systems_tested_srt_q <- reactive(paste0("SELECT COUNT(distinct system_id) FROM fieldwork.viw_srt_full 
            WHERE phase = '", input$phase, "'", rv$select_range_tests_query_text(), rv$public_query_text()))
      
      rv$systems_tested_srt_value <- reactive(dbGetQuery(poolConn, rv$systems_tested_srt_q()))
      
      rv$systems_tested_srt <- reactive(data.frame(Metric = "Systems Tested - SRT", 
                                                   Count = rv$systems_tested_srt_value()))
      
      #systems tested: porous pavement
      rv$systems_tested_ppt_q <- reactive(paste0("SELECT COUNT(distinct admin.fun_smp_to_system(smp_id)) FROM fieldwork.viw_porous_pavement_full 
            WHERE phase = '", input$phase, "'", rv$select_range_tests_query_text(), rv$public_query_text()))
      
      rv$systems_tested_ppt_value <- reactive(dbGetQuery(poolConn, rv$systems_tested_ppt_q()))
      
      rv$systems_tested_ppt <- reactive(data.frame(Metric = "Systems Tested - Porous Pavement", 
                                                   Count = rv$systems_tested_ppt_value()))
      
      #systems tested: CET
      rv$systems_tested_cet_q <- reactive(paste0("SELECT COUNT(distinct system_id) FROM fieldwork.viw_capture_efficiency_full 
            WHERE phase = '", input$phase, "'", rv$select_range_tests_query_text(), rv$public_query_text()))
      
      rv$systems_tested_cet_value <- reactive(dbGetQuery(poolConn, rv$systems_tested_cet_q()))
      
      rv$systems_tested_cet <- reactive(data.frame(Metric = "Systems Tested - Capture Efficiency", 
                                                   Count = rv$systems_tested_cet_value()))
    
      # CET ------------------------------------------------------------
      
      rv$cet_inlet_query_test <- reactive(if(input$inlet_type == "All"){
        paste("")
      }else if(input$inlet_type == "Inlets"){
        paste("AND 
            asset_type IN ('Inlet', 'Fitting')")
      }else if(input$inlet_type == "Curbcuts"){
        paste("AND 
            asset_type IN ('CURBCUT', 'TrenchDrain')")
      }
      )
      
      rv$cet_unique_query_text <- reactive(if(input$unique_inlets == "Include All Tests"){ 
        paste("fieldwork.viw_capture_efficiency_full")
      }else if(input$unique_inlets == "Most Recent for Each Inlet"){
        paste("fieldwork.viw_capture_efficiency_full_unique_inlets")
      })
      
      #capture efficiency tests (pc)
      rv$cet_lf_q <- reactive(paste0("SELECT COUNT(*) FROM ", rv$cet_unique_query_text(), " 
            WHERE phase = '", input$phase, "'", rv$select_range_tests_query_text(), rv$cet_inlet_query_test(), rv$public_query_text(), "
            AND low_flow_bypass_observed IS NOT NULL"))
      
      rv$cet_lf_value <- reactive(dbGetQuery(poolConn, rv$cet_lf_q()))
      
      rv$cet_lf <- reactive(data.frame(Metric = "Low Flow Capture Efficiency Tests", 
                                       Count = rv$cet_lf_value()))
      
      #capture efficiency tests (pc)
      rv$cet_hf_q <- reactive(paste0("SELECT COUNT(*) FROM ", rv$cet_unique_query_text(), " 
            WHERE phase = '", input$phase, "'", rv$select_range_tests_query_text(), rv$cet_inlet_query_test(), rv$public_query_text(), "
            AND high_flow_efficiency_pct IS NOT NULL"))
      
      rv$cet_hf_value <- reactive(dbGetQuery(poolConn, rv$cet_hf_q()))
      
      rv$cet_hf <- reactive(data.frame(Metric = "High Flow Capture Efficiency Tests", 
                                       Count = rv$cet_hf_value()))
      
      #capture efficiency tests with no low flow bypass (pc)
      rv$cet_no_lf_bypass_q <- reactive(paste0("SELECT COUNT(*) FROM ", rv$cet_unique_query_text(), " 
            WHERE phase = '", input$phase, "' AND 
            low_flow_efficiency_pct = 100 ", rv$select_range_tests_query_text(), rv$cet_inlet_query_test(), rv$public_query_text()))
      
      rv$cet_no_lf_bypass_value <- reactive(dbGetQuery(poolConn, rv$cet_no_lf_bypass_q()))
      
      rv$cet_no_lf_bypass <- reactive(data.frame(Metric = "Capture Efficiency Tests with No Low Flow Bypass", 
                                                 Count = rv$cet_no_lf_bypass_value()))
      
      #capture efficiency tests with no high flow bypass (pc)
      rv$cet_no_hf_bypass_q <- reactive(paste0("SELECT COUNT(*) FROM ", rv$cet_unique_query_text(), " 
            WHERE phase = '", input$phase, "' AND 
            high_flow_efficiency_pct = 100 ", rv$select_range_tests_query_text(), rv$cet_inlet_query_test(), rv$public_query_text()))
      
      rv$cet_no_hf_bypass_value <- reactive(dbGetQuery(poolConn, rv$cet_no_hf_bypass_q()))
      
      rv$cet_no_hf_bypass <- reactive(data.frame(Metric = "Capture Efficiency Tests with No High Flow Bypass", 
                                                 Count = rv$cet_no_hf_bypass_value()))
      
      #capture efficiency tests with > 10% low flow bypass (pc)
      rv$cet_10pct_lf_bypass_q <- reactive(paste0("SELECT COUNT(*) FROM ", rv$cet_unique_query_text(), " 
            WHERE phase = '", input$phase, "' AND 
            low_flow_efficiency_pct < 90 ", rv$select_range_tests_query_text(), rv$cet_inlet_query_test(), rv$public_query_text()))
      
      rv$cet_10pct_lf_bypass_value <- reactive(dbGetQuery(poolConn, rv$cet_10pct_lf_bypass_q()))
      
      rv$cet_10pct_lf_bypass <- reactive(data.frame(Metric = "Capture Efficiency Tests with > 10% Low Flow Bypass", 
                                                    Count = rv$cet_10pct_lf_bypass_value()))
      
      #capture efficiency tests with > 10% high flow bypass (pc)
      rv$cet_10pct_hf_bypass_q <- reactive(paste0("SELECT COUNT(*) FROM ", rv$cet_unique_query_text(), " 
            WHERE phase = '", input$phase, "' AND 
            high_flow_efficiency_pct < 90 ", rv$select_range_tests_query_text(), rv$cet_inlet_query_test(), rv$public_query_text()))
      
      rv$cet_10pct_hf_bypass_value <- reactive(dbGetQuery(poolConn, rv$cet_10pct_hf_bypass_q()))
      
      rv$cet_10pct_hf_bypass <- reactive(data.frame(Metric = "Capture Efficiency Tests with > 10% High Flow Bypass", 
                                                    Count = rv$cet_10pct_hf_bypass_value()))
    }
  )
}