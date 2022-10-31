#Quarterly Report
#Select Quarter Only 
#Table will match what is in the quarterly report

#1.0 UI --------
  q_reportUI <- function(id, label = "q_report", current_fy, years){
    ns <- NS(id)
    tabPanel(title = "Quarterly Report", value = "q_report",
    fluidPage(theme = shinytheme("cerulean"),
              titlePanel("Quarterly Report Counts"), 
              #1.1 General Inputs
              sidebarPanel(
                fluidRow(column(6, selectInput(ns("fy"), "Fiscal Year (FY)", choices = years)),
                         column(6, selectInput(ns("quarter"), "Quarter", choices = c("Q1", "Q2", "Q3", "Q4")))
                ),
              #1.2 Buttons
              # fluidRow(column(6,
                              actionButton(ns("table_button"), "Generate Table"),
                       # column(6, 
                       #        shinyjs::disabled(downloadButton(ns("download_table"), "Download"))))
                         
              ),
              #1.3 Main Panel (Outputs) -----
                mainPanel(
                  h4(textOutput(ns("header"))),
                  disabled(h4("Public Post-Construction")),
                  DTOutput(ns("pupc_table")), 
                  disabled(h4("Public Construction")), 
                  DTOutput(ns("puc_table")), 
                  disabled(h4("Private Post-Construction")), 
                  DTOutput(ns("prpc_table"))
                )
    )
    )
    
  }
  
#2.0 Server -----
  q_reportServer <- function(id, parent_session, current_fy, poolConn){
    moduleServer(
      id, 
      function(input, output, session){
        
        #2.1 set up----
        #define ns to use in modals
        ns <- session$ns
        
        #initialize reactive values
        rv <- reactiveValues()
        
        #create a date style for headers
        sf <- lubridate::stamp("March 1, 1999", orders = "%B %d, %Y")
        
        #get quarters as dates
        rv$start_quarter <- reactive(case_when(input$quarter == "Q1" ~ "1/1", 
                                               input$quarter == "Q2" ~ "4/1", 
                                               input$quarter == "Q3" ~ "7/1", 
                                               input$quarter == "Q4" ~ "10/1"))
        
        rv$end_quarter <- reactive(case_when(input$quarter == "Q1" ~ "3/31", 
                                             input$quarter == "Q2" ~ "6/30", 
                                             input$quarter == "Q3" ~ "9/30", 
                                             input$quarter == "Q4" ~ "12/31"))
        
        #convert FY/Quarter to a real date
        rv$start_date <- reactive(lubridate::mdy(paste0(rv$start_quarter(), "/", input$fy))%m-% months(6))
        rv$end_date <- reactive(lubridate::mdy(paste0(rv$end_quarter(), "/", input$fy))%m-% months(6))
        
        #2.2 observe event --------
        observeEvent(input$table_button, {
          
          #enable downloading after table in generated
          enable("download_table")
          
          #name table in text output
          rv$header <- paste0("FY", input$fy, " ", input$quarter, " Quarterly Report Counts (", sf(rv$start_date()), " to ", sf(rv$end_date()), ")")
          output$header <- renderText(rv$header)
          
          #public (pu) post-construction (pc) (pupc) table
          rv$pupc_table <- bind_rows(
            pupc$sensors_deployed(), 
            pupc$systems_monitored(),
            pupc$systems_newly_monitored_qtr(),
            pupc$systems_monitored_to_date(),
            pupc$new_systems_QA_data_qtr(),
            pupc$systems_QA_data_to_date(),
            pupc$pre_inspection_qtr(), 
            pupc$pre_inspection_to_date(),
            pupc$cctv_dye_test_qtr(), 
            pupc$cctv_dye_test_to_date(),
            pupc$performance_srts_qtr(), 
            pupc$performance_srts_to_date(), 
            pupc$systems_tested_cet(),
            pupc$systems_tested_cet_to_date(),
            pupc$infiltration_system_tests_qtr(),
            pupc$infiltration_system_tests_to_date()
          )
          
          output$pupc_table <- renderDT(
            rv$pupc_table, 
            options = list(dom = 't', pageLength = 20)
          )
          
          #public construction table
          rv$puc_table <- bind_rows(
            puc$pre_inspection_qtr(), 
            puc$pre_inspection_to_date(),
            puc$cctv_dye_test_qtr(), 
            puc$cctv_dye_test_to_date(),
            puc$performance_srts_qtr(), 
            puc$performance_srts_to_date()
          )
          
          output$puc_table <- renderDT(
            rv$puc_table, 
            options = list(dom = 't')
          )
          
          rv$prpc_table <- bind_rows(
            prpc$sensors_deployed(), 
            prpc$systems_monitored(),
            prpc$systems_newly_monitored_qtr(),
            prpc$systems_monitored_to_date(),
            prpc$pre_inspection_qtr(), 
            prpc$pre_inspection_to_date(),
            prpc$cctv_dye_test_qtr(), 
            prpc$cctv_dye_test_to_date(),
            prpc$performance_srts_qtr(), 
            prpc$performance_srts_to_date(), 
            prpc$systems_tested_cet(),
            prpc$systems_tested_cet_to_date()
          )
          
          output$prpc_table <- renderDT(
            rv$prpc_table, 
            options = list(dom = 't')
          )
          
          #browser() #Check the state of variables post-button click
          
        })
        
        
        #2.3 get values ---------
        #help with queries
        #this is like this because it makes it easier to copy from the other module 
        rv$quarter_range <- reactive(
          paste("AND test_date >= '", rv$start_date(), "' AND
                                             test_date <= '", rv$end_date(), "'")
        )
        
        rv$to_date_range <- reactive(
          paste("AND test_date <= '", rv$end_date(), "' ")
        )
        
        #from table located here: 
        #"O:\Watershed Sciences\GSI Monitoring\03 Reports and Presentations\08 Quarterly Reports\SOP\MARS_SOP\Template_[FYXXQX]_PostCon_MARS_Inputs.docx"
        
        #2.3.1 Section 2: Public Post-Construction (pupc) ----
        
        #reactive values for this section
        
        pupc <- reactiveValues()
        
        #help with queries
        pupc$public_query_text <- "AND public = TRUE"
        
        pupc$phase <- "Post-Construction"
        
        #Sensors deployed this quarter
        #hobos deployed (public)
        pupc$sensors_deployed_q <- reactive(paste0("SELECT COUNT(distinct(sensor_serial)) FROM fieldwork.deployment_full_cwl 
                                             WHERE deployment_dtime_est <= '", rv$end_date(), "'
                                                    AND (collection_dtime_est >= '", rv$start_date(), "' 
                                             OR collection_dtime_est IS NULL) AND 
                                             public = TRUE"))
        
        pupc$sensors_deployed_value <- reactive(dbGetQuery(poolConn, pupc$sensors_deployed_q()))
        pupc$sensors_deployed <- reactive(data.frame(Metric = "Sensors Deployed", 
                                                 Description = "Total HOBOs deployed this quarter",
                                                 Count = pupc$sensors_deployed_value()))
        
        #systems with CWL monitoring this quarter
        pupc$systems_monitored_q <- reactive(paste0("SELECT COUNT(DISTINCT smp_to_system(d.smp_id)) FROM fieldwork.deployment_full_cwl d
                                                       WHERE deployment_dtime_est <= '", rv$end_date(), "'
                                                    AND (collection_dtime_est >= '", rv$start_date(), "'
                                             OR collection_dtime_est IS NULL)
                                                       AND d.public = TRUE"))

        pupc$systems_monitored_value <- reactive(dbGetQuery(poolConn, pupc$systems_monitored_q()))
        pupc$systems_monitored <- reactive(data.frame(Metric = as.character("Systems Monitored this Quarter"), 
                                                      Description = "Systems at which HOBOs were deployed this quarter",
                                                      Count = pupc$systems_monitored_value()))

        #Systems newly monitored this quarter
        #Systems deployed at for the first time this quarter
        pupc$systems_newly_monitored_qtr_q <- reactive(paste0("with last_quarter as (SELECT DISTINCT smp_to_system(d.smp_id) as system_id FROM fieldwork.deployment_full_cwl d
          WHERE d.public = TRUE and deployment_dtime_est < '", rv$start_date(), "' and smp_to_system(d.smp_id) is not null)
                
          SELECT count(DISTINCT(smp_to_system(d.smp_id))) FROM fieldwork.deployment_full_cwl d
            WHERE d.public = TRUE and deployment_dtime_est <= '", rv$end_date(), "' 
            and smp_to_system(d.smp_id) is not null and smp_to_system(d.smp_id) not in (select * from last_quarter)"))
        pupc$systems_newly_monitored_qtr_value <- reactive(dbGetQuery(poolConn, pupc$systems_newly_monitored_qtr_q()))
        pupc$systems_newly_monitored_qtr <- reactive(data.frame(Metric = as.character("Systems Newly Monitored this Quarter"), 
                                                                Description = "Systems at which HOBOs were deployed for the first time this quarter",
                                                                Count = pupc$systems_newly_monitored_qtr_value()))

        
        #systems with CWL monitoring to-date
        #No. of public systems monitored to date
        pupc$systems_monitored_to_date_q <- reactive(paste0("SELECT COUNT(DISTINCT smp_to_system(d.smp_id)) FROM fieldwork.deployment_full_cwl d
                                                    WHERE d.public = TRUE and deployment_dtime_est <= '", rv$end_date(), "'"))

        pupc$systems_monitored_to_date_value <- reactive(dbGetQuery(poolConn, pupc$systems_monitored_to_date_q()))
        pupc$systems_monitored_to_date <- reactive(data.frame(Metric = as.character("Systems Monitored to-Date"),
                                                                   Description = "All systems at which HOBOs have been deployed",
                                                                   Count = pupc$systems_monitored_to_date_value()))

        #systems with QA'd CWL Data to-date
        pupc$new_systems_QA_data_qtr_q <- reactive(paste0("with fq_input as (select fiscal_quarter_lookup_uid from public.fiscal_quarter_lookup WHERE fiscal_quarter = 'FY",
                                                              str_sub(input$fy,-2),input$quarter,"') ,
                                                              
                                                              newest_smps as (select distinct system_id from ow_leveldata_entrydates 
                                                              where fiscal_quarter_lookup_uid = (select max(fiscal_quarter_lookup_uid) from fq_input)),
                                                              
                                                              previous_date as (select max(fiscal_quarter_lookup_uid) as previous_date from ow_leveldata_entrydates 
                                                              where fiscal_quarter_lookup_uid < (select max(fiscal_quarter_lookup_uid)from fq_input)),
                                                              
                                                              previous_smps as (select distinct system_id from ow_leveldata_entrydates 
                                                              where fiscal_quarter_lookup_uid <= (select previous_date from previous_date))
                                                              
                                                              select COUNT(*) from newest_smps 
                                                              where system_id not in (select system_id from previous_smps) -- System never given CWL before
                                                              and system_id like '%-%' -- Public SMPs only"))
        
        pupc$new_systems_QA_data_qtr_value <- reactive(dbGetQuery(poolConn, pupc$new_systems_QA_data_qtr_q()))
        pupc$new_systems_QA_data_qtr <- reactive(data.frame(Metric = "New Systems With QA data this quarter",
                                                            Description = "Systems that have QA data for the first time this quarter",
                                                              Count = pupc$new_systems_QA_data_qtr_value()))

        pupc$systems_QA_data_to_date_q <- reactive(paste0("with fq_input as (select fiscal_quarter_lookup_uid from public.fiscal_quarter_lookup WHERE fiscal_quarter = 'FY",
                                                          str_sub(input$fy,-2),input$quarter,"') ,
                                                              
                                                              current_systems as (select distinct system_id from ow_leveldata_entrydates 
                                                              where fiscal_quarter_lookup_uid <= (select max(fiscal_quarter_lookup_uid) from fq_input))
                                                              
                                                              select COUNT(*) from current_systems 
                                                              where system_id like '%-%' -- Public Systems only"))
        
        pupc$systems_QA_data_to_date_value <- reactive(dbGetQuery(poolConn, pupc$systems_QA_data_to_date_q()))
        pupc$systems_QA_data_to_date <- reactive(data.frame(Metric = "Systems With QA data to-Date",
                                                            Description = "All systems with QA data in the Postgres database",
                                                            Count = pupc$systems_QA_data_to_date_value()))
        
        
        #SRT pre-inspection tests this quarter
        pupc$pre_inspection_qtr_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full
                                             WHERE phase = '", pupc$phase, "' AND
                                             type = 'Pre-Inspection Dye Test'", rv$quarter_range(), pupc$public_query_text))

        pupc$pre_inspection_qtr_value <- reactive(dbGetQuery(poolConn, pupc$pre_inspection_qtr_q()))

        pupc$pre_inspection_qtr <- reactive(data.frame(Metric = "Pre-Inspection Dye Tests this Quarter",
                                                 Count = pupc$pre_inspection_qtr_value()))
        
        #SRT pre-inspection tests to-date
        pupc$pre_inspection_to_date_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full
                                             WHERE phase = '", pupc$phase, "' AND
                                             type = 'Pre-Inspection Dye Test'", rv$to_date_range(), pupc$public_query_text))
        
        pupc$pre_inspection_to_date_value <- reactive(dbGetQuery(poolConn, pupc$pre_inspection_to_date_q()))
        
        pupc$pre_inspection_to_date <- reactive(data.frame(Metric = "Pre-Inspection Dye Tests to-Date",
                                                       Count = pupc$pre_inspection_to_date_value()))
        
        
        #SRT CCTV/ Dye Tests this quarter
        pupc$cctv_dye_test_qtr_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full
                                             WHERE phase = '", pupc$phase, "' AND
                                             type = 'CCTV Dye Test'", rv$quarter_range(), pupc$public_query_text))
        
        pupc$cctv_dye_test_qtr_value <- reactive(dbGetQuery(poolConn, pupc$cctv_dye_test_qtr_q()))
        
        pupc$cctv_dye_test_qtr <- reactive(data.frame(Metric = "CCTV Dye Tests this Quarter",
                                                       Count = pupc$cctv_dye_test_qtr_value()))
        
        #SRT CCTV/ Dye Tests this to-date
        pupc$cctv_dye_test_to_date_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full
                                             WHERE phase = '", pupc$phase, "' AND
                                             type = 'CCTV Dye Test'", rv$to_date_range(), pupc$public_query_text))
        
        pupc$cctv_dye_test_to_date_value <- reactive(dbGetQuery(poolConn, pupc$cctv_dye_test_to_date_q()))
        
        pupc$cctv_dye_test_to_date <- reactive(data.frame(Metric = "CCTV Dye Tests to-Date",
                                                           Count = pupc$cctv_dye_test_to_date_value()))
        
        #SRT Performance Tests this quarter
        pupc$performance_srts_qtr_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full
                                             WHERE phase = '", pupc$phase, "' AND
                                             type = 'Performance Test'", rv$quarter_range(), pupc$public_query_text))
        
        pupc$performance_srts_qtr_value <- reactive(dbGetQuery(poolConn, pupc$performance_srts_qtr_q()))
        
        pupc$performance_srts_qtr <- reactive(data.frame(Metric = "Performance Tests this Quarter",
                                                       Count = pupc$performance_srts_qtr_value()))
        
        #SRT Performance Tests to-date
        pupc$performance_srts_to_date_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full
                                             WHERE phase = '", pupc$phase, "' AND
                                             type = 'Performance Test'", rv$to_date_range(), pupc$public_query_text))
        
        pupc$performance_srts_to_date_value <- reactive(dbGetQuery(poolConn, pupc$performance_srts_to_date_q()))
        
        pupc$performance_srts_to_date <- reactive(data.frame(Metric = "Performance Tests to-Date",
                                                           Count = pupc$performance_srts_to_date_value()))
        
        #systems with capture efficiency testing administered this quarter
        pupc$systems_tested_cet_q <- reactive(paste0("with old_systems as (select system_id, min(test_date) as earliest_test 
            from fieldwork.capture_efficiency_full where phase = '", 
                                                     pupc$phase, "' AND test_date < '", 
                                                     rv$start_date(), "' ", pupc$public_query_text, 
                                                     " AND low_flow_bypass_observed IS NOT NULL group by system_id)
  
            SELECT count(distinct(system_id)) FROM fieldwork.capture_efficiency_full 
            WHERE phase = '", pupc$phase, "'", rv$quarter_range(), 
                                                     pupc$public_query_text, " AND low_flow_bypass_observed IS NOT NULL 
            and system_id not in (select system_id from old_systems)"))        
        pupc$systems_tested_cet_value <- reactive(dbGetQuery(poolConn, pupc$systems_tested_cet_q()))
        
        pupc$systems_tested_cet <- reactive(data.frame(Metric = "New Systems with Capture Efficiency Tests this Quarter", 
                                                     Count = pupc$systems_tested_cet_value()))
        
        
        #systems with capture efficiency testing administered to date
        pupc$systems_tested_cet_to_date_q <- reactive(paste0("SELECT COUNT(distinct system_id) FROM fieldwork.capture_efficiency_full 
            WHERE phase = '", pupc$phase, "'", rv$to_date_range(), pupc$public_query_text, " AND low_flow_bypass_observed IS NOT NULL"))
        
        pupc$systems_tested_cet_to_date_value <- reactive(dbGetQuery(poolConn, pupc$systems_tested_cet_to_date_q()))
        
        pupc$systems_tested_cet_to_date <- reactive(data.frame(Metric = "Systems with Capture Efficiency Tests to-Date", 
                                                       Count = pupc$systems_tested_cet_to_date_value()))
        

        #infiltration system-tests this quarter (subtracting ten because of some tests that took part on multiple)
        pupc$infiltration_system_tests_qtr_q <- reactive(paste0("SELECT count(*) FROM fieldwork.porous_pavement_smp_averages
                                                          WHERE test_date >= '", rv$start_date(), "' AND
                                                               test_date <= '", rv$end_date(), "'"))

        pupc$infiltration_system_tests_qtr_value <- reactive(dbGetQuery(poolConn, pupc$infiltration_system_tests_qtr_q()))

        pupc$infiltration_system_tests_qtr <- reactive(data.frame(Metric = "Porous Pavement Infiltration System-Tests this Quarter",
                                                       Count = pupc$infiltration_system_tests_qtr_value()))
        
        #infiltration system-tests to-date
        pupc$infiltration_system_tests_to_date_q <- reactive(paste0("SELECT (count(*) - 10) as count FROM fieldwork.porous_pavement_smp_averages
                                                          WHERE test_date <= '", rv$end_date(), "'"))
        
        pupc$infiltration_system_tests_to_date_value <- reactive(dbGetQuery(poolConn, pupc$infiltration_system_tests_to_date_q()))
        
        pupc$infiltration_system_tests_to_date <- reactive(data.frame(Metric = "Porous Pavement Infiltration System-Tests to-Date",
                                                       Count = pupc$infiltration_system_tests_to_date_value()))
        
        #2.3.2 Section 5: Public Construction  ----
        
        puc <- reactiveValues()
        
        #help with queries
        puc$public_query_text <- "AND public = TRUE"
        
        puc$phase <- "Construction"
        
        #SRT pre-inspection tests this quarter
        puc$pre_inspection_qtr_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full
                                             WHERE phase = '", puc$phase, "' AND
                                             type = 'Pre-Inspection Dye Test'", rv$quarter_range(), puc$public_query_text))
        
        puc$pre_inspection_qtr_value <- reactive(dbGetQuery(poolConn, puc$pre_inspection_qtr_q()))
        
        puc$pre_inspection_qtr <- reactive(data.frame(Metric = "Pre-Inspection Dye Tests this Quarter",
                                                      Count = puc$pre_inspection_qtr_value()))
        
        #SRT pre-inspection tests to-date
        puc$pre_inspection_to_date_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full
                                             WHERE phase = '", puc$phase, "' AND
                                             type = 'Pre-Inspection Dye Test'", rv$to_date_range(), puc$public_query_text))
        
        puc$pre_inspection_to_date_value <- reactive(dbGetQuery(poolConn, puc$pre_inspection_to_date_q()))
        
        puc$pre_inspection_to_date <- reactive(data.frame(Metric = "Pre-Inspection Dye Tests to-Date",
                                                          Count = puc$pre_inspection_to_date_value()))
        
        
        #SRT CCTV/ Dye Tests this quarter
        puc$cctv_dye_test_qtr_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full
                                             WHERE phase = '", puc$phase, "' AND
                                             type = 'CCTV Dye Test'", rv$quarter_range(), puc$public_query_text))
        
        puc$cctv_dye_test_qtr_value <- reactive(dbGetQuery(poolConn, puc$cctv_dye_test_qtr_q()))
        
        puc$cctv_dye_test_qtr <- reactive(data.frame(Metric = "CCTV Dye Tests this Quarter",
                                                     Count = puc$cctv_dye_test_qtr_value()))
        
        #SRT CCTV/ Dye Tests this to-date
        puc$cctv_dye_test_to_date_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full
                                             WHERE phase = '", puc$phase, "' AND
                                             type = 'CCTV Dye Test'", rv$to_date_range(), puc$public_query_text))
        
        puc$cctv_dye_test_to_date_value <- reactive(dbGetQuery(poolConn, puc$cctv_dye_test_to_date_q()))
        
        puc$cctv_dye_test_to_date <- reactive(data.frame(Metric = "CCTV Dye Tests to-Date",
                                                         Count = puc$cctv_dye_test_to_date_value()))
        
        #SRT Performance Tests this quarter
        puc$performance_srts_qtr_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full
                                             WHERE phase = '", puc$phase, "' AND
                                             type = 'Performance Test'", rv$quarter_range(), puc$public_query_text))
        
        puc$performance_srts_qtr_value <- reactive(dbGetQuery(poolConn, puc$performance_srts_qtr_q()))
        
        puc$performance_srts_qtr <- reactive(data.frame(Metric = "Performance Tests this Quarter",
                                                        Count = puc$performance_srts_qtr_value()))
        
        #SRT Performance Tests to-date
        puc$performance_srts_to_date_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full
                                             WHERE phase = '", puc$phase, "' AND
                                             type = 'Performance Test'", rv$to_date_range(), puc$public_query_text))
        
        puc$performance_srts_to_date_value <- reactive(dbGetQuery(poolConn, puc$performance_srts_to_date_q()))
        
        puc$performance_srts_to_date <- reactive(data.frame(Metric = "Performance Tests to-Date",
                                                            Count = puc$performance_srts_to_date_value()))
        
        
        #2.3.3 Section 6: Private Post-Construction -----
        
        #reactive values for this section
        
        prpc <- reactiveValues()
        
        #help with queries
        prpc$public_query_text <- "AND public = FALSE"
        
        prpc$phase <- "Post-Construction"
        
        #Sensors deployed this quarter
        #hobos deployed (public)
        prpc$sensors_deployed_q <- reactive(paste0("SELECT COUNT(distinct(sensor_serial)) FROM fieldwork.deployment_full_cwl 
                                             WHERE deployment_dtime_est <= '", rv$end_date(), "'
                                                    AND (collection_dtime_est >= '", rv$start_date(), "' 
                                             OR collection_dtime_est IS NULL) AND 
                                             public = FALSE"))
        
        prpc$sensors_deployed_value <- reactive(dbGetQuery(poolConn, prpc$sensors_deployed_q()))
        prpc$sensors_deployed <- reactive(data.frame(Metric = "Sensors Deployed", 
                                                     Description = "Total HOBOs deployed this quarter",
                                                     Count = prpc$sensors_deployed_value()))
        
        
        #systems with CWL monitoring this quarter
        prpc$systems_monitored_q <- reactive(paste0("SELECT COUNT(DISTINCT smp_to_system(d.smp_id)) FROM fieldwork.deployment_full_cwl d
                                                       WHERE deployment_dtime_est <= '", rv$end_date(), "'
                                                    AND (collection_dtime_est >= '", rv$start_date(), "'
                                             OR collection_dtime_est IS NULL)
                                                       AND d.public = FALSE"))
        
        prpc$systems_monitored_value <- reactive(dbGetQuery(poolConn, prpc$systems_monitored_q()))
        prpc$systems_monitored <- reactive(data.frame(Metric = as.character("Systems Monitored this Quarter"), 
                                                      Description = "Systems at which HOBOs were deployed this quarter",
                                                      Count = prpc$systems_monitored_value()))
        
        #Systems newly monitored this quarter
        #Systems deployed at for the first time this quarter
        prpc$systems_newly_monitored_qtr_q <- reactive(paste0("with last_quarter as (SELECT DISTINCT smp_to_system(d.smp_id) as system_id FROM fieldwork.deployment_full_cwl d
          WHERE d.public = FALSE and deployment_dtime_est < '", rv$start_date(), "' and smp_to_system(d.smp_id) is not null)
                
          SELECT count(DISTINCT(smp_to_system(d.smp_id))) FROM fieldwork.deployment_full_cwl d
            WHERE d.public = FALSE and deployment_dtime_est <= '", rv$end_date(), "' 
            and smp_to_system(d.smp_id) is not null and smp_to_system(d.smp_id) not in (select * from last_quarter)"))
        prpc$systems_newly_monitored_qtr_value <- reactive(dbGetQuery(poolConn, prpc$systems_newly_monitored_qtr_q()))
        prpc$systems_newly_monitored_qtr <- reactive(data.frame(Metric = as.character("Systems Newly Monitored this Quarter"), 
                                                                Description = "Systems at which HOBOs were deployed for the first time this quarter",
                                                                Count = prpc$systems_newly_monitored_qtr_value()))
        
        #systems with CWL monitoring to-date
        #No. of public systems monitored to date
        prpc$systems_monitored_to_date_q <- reactive(paste0("SELECT COUNT(DISTINCT smp_to_system(d.smp_id)) FROM fieldwork.deployment_full_cwl d
                                                    WHERE d.public = FALSE and deployment_dtime_est <= '", rv$end_date(), "'"))
        
        prpc$systems_monitored_to_date_value <- reactive(dbGetQuery(poolConn, prpc$systems_monitored_to_date_q()))
        prpc$systems_monitored_to_date <- reactive(data.frame(Metric = as.character("Systems Monitored to-Date"),
                                                              Description = "All systems at which HOBOs have been deployed",
                                                              to_date_count = prpc$systems_monitored_to_date_value()))
        
        #SRT pre-inspection tests this quarter
        prpc$pre_inspection_qtr_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full
                                             WHERE phase = '", prpc$phase, "' AND
                                             type = 'Pre-Inspection Dye Test'", rv$quarter_range(), prpc$public_query_text))
        
        prpc$pre_inspection_qtr_value <- reactive(dbGetQuery(poolConn, prpc$pre_inspection_qtr_q()))
        
        prpc$pre_inspection_qtr <- reactive(data.frame(Metric = "Pre-Inspection Dye Tests this Quarter",
                                                       Count = prpc$pre_inspection_qtr_value()))
        
        #SRT pre-inspection tests to-date
        prpc$pre_inspection_to_date_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full
                                             WHERE phase = '", prpc$phase, "' AND
                                             type = 'Pre-Inspection Dye Test'", rv$to_date_range(), prpc$public_query_text))
        
        prpc$pre_inspection_to_date_value <- reactive(dbGetQuery(poolConn, prpc$pre_inspection_to_date_q()))
        
        prpc$pre_inspection_to_date <- reactive(data.frame(Metric = "Pre-Inspection Dye Tests to-Date",
                                                           Count = prpc$pre_inspection_to_date_value()))
        
        
        #SRT CCTV/ Dye Tests this quarter
        prpc$cctv_dye_test_qtr_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full
                                             WHERE phase = '", prpc$phase, "' AND
                                             type = 'CCTV Dye Test'", rv$quarter_range(), prpc$public_query_text))
        
        prpc$cctv_dye_test_qtr_value <- reactive(dbGetQuery(poolConn, prpc$cctv_dye_test_qtr_q()))
        
        prpc$cctv_dye_test_qtr <- reactive(data.frame(Metric = "CCTV Dye Tests this Quarter",
                                                      Count = prpc$cctv_dye_test_qtr_value()))
        
        #SRT CCTV/ Dye Tests this to-date
        prpc$cctv_dye_test_to_date_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full
                                             WHERE phase = '", prpc$phase, "' AND
                                             type = 'CCTV Dye Test'", rv$to_date_range(), prpc$public_query_text))
        
        prpc$cctv_dye_test_to_date_value <- reactive(dbGetQuery(poolConn, prpc$cctv_dye_test_to_date_q()))
        
        prpc$cctv_dye_test_to_date <- reactive(data.frame(Metric = "CCTV Dye Tests to-Date",
                                                          Count = prpc$cctv_dye_test_to_date_value()))
        
        #SRT Performance Tests this quarter
        prpc$performance_srts_qtr_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full
                                             WHERE phase = '", prpc$phase, "' AND
                                             type = 'Performance Test'", rv$quarter_range(), prpc$public_query_text))
        
        prpc$performance_srts_qtr_value <- reactive(dbGetQuery(poolConn, prpc$performance_srts_qtr_q()))
        
        prpc$performance_srts_qtr <- reactive(data.frame(Metric = "Performance Tests this Quarter",
                                                         Count = prpc$performance_srts_qtr_value()))
        
        #SRT Performance Tests to-date
        prpc$performance_srts_to_date_q <- reactive(paste0("SELECT COUNT(*) FROM fieldwork.srt_full
                                             WHERE phase = '", prpc$phase, "' AND
                                             type = 'Performance Test'", rv$to_date_range(), prpc$public_query_text))
        
        prpc$performance_srts_to_date_value <- reactive(dbGetQuery(poolConn, prpc$performance_srts_to_date_q()))
        
        prpc$performance_srts_to_date <- reactive(data.frame(Metric = "Performance Tests to-Date",
                                                             Count = prpc$performance_srts_to_date_value()))
        
        #systems with capture efficiency testing administered this quarter
        prpc$systems_tested_cet_q <- reactive(paste0("SELECT COUNT(distinct system_id) FROM fieldwork.capture_efficiency_full 
            WHERE phase = '", prpc$phase, "'", rv$quarter_range(), prpc$public_query_text))
        
        prpc$systems_tested_cet_value <- reactive(dbGetQuery(poolConn, prpc$systems_tested_cet_q()))
        
        prpc$systems_tested_cet <- reactive(data.frame(Metric = "Systems with Capture Efficiency Tests this Quarter", 
                                                       Count = prpc$systems_tested_cet_value()))
        
        
        #systems with capture efficiency testing administered to date
        prpc$systems_tested_cet_to_date_q <- reactive(paste0("SELECT COUNT(distinct system_id) FROM fieldwork.capture_efficiency_full 
            WHERE phase = '", prpc$phase, "'", rv$to_date_range(), prpc$public_query_text))
        
        prpc$systems_tested_cet_to_date_value <- reactive(dbGetQuery(poolConn, prpc$systems_tested_cet_to_date_q()))
        
        prpc$systems_tested_cet_to_date <- reactive(data.frame(Metric = "Systems with Capture Efficiency Tests to-Date", 
                                                               Count = prpc$systems_tested_cet_to_date_value()))
      }
    )
  }
    
        
        
  
  
  