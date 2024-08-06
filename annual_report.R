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
                 
                  
                  strong("Table 3-1: Summary of Post-Construction CWL Monitoring of Public SMPs"),
                  reactableOutput(ns("Table 3-1")),
                  strong("Table 3-2: Post-Construction CWL Monitoring of Public SMPs Listed by Type "),
                  reactableOutput(ns("Table 3-2")),
                  strong("Table 3-3: Post-Construction SRTs performed on Public Systems"),
                  reactableOutput(ns("Table 3-3")),
                  strong("Table 3-4: Public Systems with Post-Construction SRTs Performed"),
                  reactableOutput(ns("Table 3-4")),
                  strong("Table 3-5: Public Systems with CETs Administered"),
                  reactableOutput(ns("Table 3-5")),
                  strong("Table 3-6: Public Systems with Infiltration Testing Administered"),
                  reactableOutput(ns("Table 3-6")),
                  strong("Table 3-7: Construction-Phase SRTs Performed on Public Systems"),
                  reactableOutput(ns("Table 3-7")),
                  strong("Table 3-8: Public Systems with Construction-Phase SRTs Performed"),
                  reactableOutput(ns("Table 3-8")),
                  strong("Table 3-9: Groundwater Monitoring for Public GSI"),
                  reactableOutput(ns("Table 3-9")),
                  strong("Table 3-10: Summary of Post-Construction CWL Monitoring of Private SMPs"),
                  reactableOutput(ns("Table 3-10")),
                  strong("Table 3-11: Post-Construction CWL-Monitoring of Private SMPs Listed by Type "),
                  reactableOutput(ns("Table 3-11")),
                  strong("Table 3-12: Post-Construction SRTs performed on Private Systems"),
                  reactableOutput(ns("Table 3-12")),
                  strong("Table 3-13: Private SMPs with Post-Construction SRTs Performed"),
                  reactableOutput(ns("Table 3-13")),
                  strong("Table 3-14: Private Systems with CETs Administered"),
                  reactableOutput(ns("Table 3-14"))
          
                  
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
          
          table_31 <- reactive({
            
            ## queries to gets stats
            #Section 3.1: Post-Construction GSI Monitoring + Testing
            #Section 3.1.1: CWL Monitoring
            #Sensors deployed this fiscal year
            #In the text, and in table 3.1
            
            sql_string_1 <- "select count(*) from fieldwork.viw_deployment_full_cwl
                                  where (collection_dtime_est > '%s' OR collection_dtime_est is null)
                                  and deployment_dtime_est between '%s' and '%s'
                                  and public =  TRUE"
            table_3_1_public_sensors_deployed_postcon <- dbGetQuery(poolConn, paste(sprintf(sql_string_1, FYSTART_reactive(),FYSTART_reactive(), FYEND_reactive()),collapse="")) 
            
            ##Public systems monitored this fiscal year
            #In the text, and in table 3.1
    
            sql_string_2 <- "select count(distinct admin.fun_smp_to_system(d.smp_id)) from fieldwork.viw_deployment_full_cwl d
                                            where d.public = true and
                                            (deployment_dtime_est between '%s' and '%s'
                                            or collection_dtime_est between '%s' and '%s'
                                            or (deployment_dtime_est < '%s' and collection_dtime_est is null))"
            
            table_3_1_public_systems_monitored <- dbGetQuery(poolConn, paste(sprintf(sql_string_2, FYSTART_reactive(), FYEND_reactive(), FYSTART_reactive(), FYEND_reactive(), FYSTART_reactive()), collapse="")) 
            
            
            #Newly monitored systems this fiscal year
            #In the text, and in table 3.1
            sql_string_3 <- "select count(*) from fieldwork.viw_first_deployment_cwl f where
                  public = true and
                  first_deployment between '%s' and '%s'"
            table_3_1_public_new_systems_monitored<- dbGetQuery(poolConn, paste(sprintf(sql_string_3, FYSTART_reactive(), FYEND_reactive()),collapse=""))
            
            
            #Sensors deployed to date
            sql_string_4 <- "select count(*) from fieldwork.viw_deployment_full_cwl
                                                where deployment_dtime_est <= '%s'
                                                and public = TRUE"
            
            table_3_1_public_sensors_deployed_todate <- dbGetQuery(poolConn, paste(sprintf(sql_string_4,FYEND_reactive()),collapse=""))
            
            #Public systems monitored to date
            #In the text, and in table 3.1
            sql_string_5 <- "select count(distinct admin.fun_smp_to_system(d.smp_id)) from fieldwork.viw_deployment_full_cwl d
                                                where deployment_dtime_est <= '%s'
                                                and d.public = true"
            table_3_1_public_systems_monitored_todate <- dbGetQuery(poolConn, paste(sprintf(sql_string_5,FYEND_reactive()),collapse=""))
            
            
            `This Fiscal Year`<- data.frame(c(pull(table_3_1_public_sensors_deployed_postcon),pull(table_3_1_public_systems_monitored),pull(table_3_1_public_new_systems_monitored)))
            `To Date`<-data.frame(c(pull(table_3_1_public_sensors_deployed_todate),pull(table_3_1_public_systems_monitored_todate),NA))
            table_3_1 <- bind_cols(`This Fiscal Year`,`To Date`)
            colnames(table_3_1)<- c("This Fiscal Year","To Date")
            rownames(table_3_1)<-c("Sensors Deployment","Systems","Systems Newly Monitored")
            table_3_1$Description <- c("Number of Sensor deployments, Doesn't include SRTs, Can Include duplicated sensor ids","Public systems with CWL data","Systems first deployed during this fiscal year")
            
            table_3_1 <- table_3_1 %>%
              select(Description, `This Fiscal Year`, `To Date`)
            
            return(table_3_1)
            
            
          })
          
          table_32 <- reactive({
            
            #Monitored public smps by type, to date
            #Table 3-2 first column
            sql_string_6 <- "select sfc.asset_type, count(distinct(d.smp_id)), d.public from
                                                fieldwork.viw_deployment_full_cwl d
                                                left join external.mat_assets sfc on d.smp_id = sfc.smp_id
                                                where sfc.component_id is null
                                                and d.smp_id is not null
                                                and d.deployment_dtime_est < '%s'
                                                and d.public = true
                                                group by sfc.asset_type, d.public"
            
            table_3_2_public_smp_bytype_todate <- dbGetQuery(poolConn, paste(sprintf(sql_string_6,FYEND_reactive()),collapse=""))
            
            #cipit statuses indicating constructed systems are Jillian Simmons's best recommendation
            #Table 3-2 second column
            sql_string_7 <- "select count(*), smp_smptype from external.tbl_smpbdv g where g.smp_notbuiltretired is null 
                                            and (g.cipit_status = 'Closed' 
                                            or g.cipit_status = 'Construction-Substantially Complete' 
                                            or g.cipit_status = 'Construction-Contract Closed') 
                                            group by smp_smptype"
            table_3_2_public_constructed_systems_total <- dbGetQuery(poolConn,sql_string_7)
            
            table_3_2_public_smp_bytype_todate <- table_3_2_public_smp_bytype_todate %>%
              select(`SMP Type` = asset_type,`Monitored SMPs`=count) 
            table_3_2_public_smp_bytype_todate[table_3_2_public_smp_bytype_todate[,"SMP Type"] == "Trench",1] <- "Infiltration/Storage Trench"
            
            
            table_3_2_public_constructed_systems_total <- table_3_2_public_constructed_systems_total %>%
              select(`SMP Type` = smp_smptype,`Total Constructed Public SMPs`=count) 
            table_3_2_public_constructed_systems_total[table_3_2_public_constructed_systems_total[,"SMP Type"] == "Pervious Paving",1] <- "Permeable Pavement"
            table_3_2 <- table_3_2_public_constructed_systems_total %>% 
              left_join(table_3_2_public_smp_bytype_todate, by="SMP Type")
            table_3_2<-table_3_2[,c(1,3,2)]
            
            #Replace NA with zero
            table_3_2[is.na(table_3_2)] <-  0
            
            table_3_2$Description <- NA
            table_3_2 <- table_3_2 %>%
              select(`SMP Type`, Description,`Total Constructed Public SMPs`, `Monitored SMPs`)
            
            table_3_2[table_3_2[,"SMP Type"] == "Infiltration/Storage Trench", 2] <- "Also listed as Trench"
            table_3_2[table_3_2[,"SMP Type"] == "Permeable Pavement", 2] <- "Also listed as Pervious Paving"
            
            return(table_3_2)
            
          })
          
          
          table_33 <- reactive({
            
            #Section 3.1.2: SRT Testing
            #Table 3-3
            #Post-construction SRTs performed on Public Systems
            #Also appears in the 3.1.2 opening paragraph
            #First column: This fiscal year
            
            sql_string_8 <- "select count(*), type from fieldwork.viw_srt_full 
                                      where test_date >= '%s'
                                      and test_date <= '%s'
                                      and phase = 'Post-Construction'
                                      and public = TRUE
                                      group by type"
            table_3_3_public_system_postcon_srt <-dbGetQuery(poolConn, paste(sprintf(sql_string_8, FYSTART_reactive(), FYEND_reactive()),collapse=""))
            
            
            #Post-construction SRTs performed on Public Systems TO DATE
            #Second column: The above, to date
            
            sql_string_9 <-"select count(*), type from fieldwork.viw_srt_full 
                                                        where test_date <= '%s'
                                                        and phase = 'Post-Construction'
                                                        and public = TRUE
                                                        group by type"
            
            table_3_3_public_system_postcon_srt_todate <- dbGetQuery(poolConn, paste(sprintf(sql_string_9,FYEND_reactive()),collapse=""))
            
            
            
            table_3_3 <- table_3_3_public_system_postcon_srt_todate %>%
              left_join(table_3_3_public_system_postcon_srt, by="type") %>%
              select(`SRT Type`=type,`This Fiscal Year`=count.y, `To Date`=count.x)
            table_3_3[is.na(table_3_3)] <-  0
            
            
            return(table_3_3)
            
          })
          
          table_34 <- reactive({
            
            #Table 3.4: Public Systems with Post-Construction SRTs Performed
            #Note: This is a count of systems, not SMPs. The table title and caption for the table in the FY21 annual report implies that it is counting SMPs.
            #The numbers in the old table are systems, not SMPs, and were generated by this query
            #I suggest we rename the table to be counting systems, and remove the caption
            #We could just as easily count SMPs, by replacing the count(distinct(srt.system_id)) with count(distinct(sfc.smp_id))
            
            #Column 1: Current fiscal year
            sql_string_10 <-"select sfc.asset_type, count(distinct(srt.system_id))
                                                  from fieldwork.viw_srt_full srt
                                                  left join external.mat_assets sfc on srt.system_id = sfc.system_id
                                                  where sfc.component_id is null
                                                  and test_date >= '%s'
                                                  and test_date <= '%s'
                                                  and phase = 'Post-Construction'
                                                  and public = TRUE
                                                  group by sfc.asset_type"
            
            table_3_4_public_systems_postcon_srt <-dbGetQuery(poolConn, paste(sprintf(sql_string_10, FYSTART_reactive(), FYEND_reactive()),collapse=""))
            
            #Public Systems with Post-Construction SRTs Performed TO DATE
            #Column 2: 
            sql_string_11 <-"select sfc.asset_type, count(distinct(srt.system_id))
                                                  from fieldwork.viw_srt_full srt
                                                  left join external.mat_assets sfc on srt.system_id = sfc.system_id
                                                  where sfc.component_id is null
                                                  and test_date <= '%s'
                                                  and phase = 'Post-Construction'
                                                  and public = TRUE
                                                  group by sfc.asset_type"
            table_3_4_public_systems_postcon_srt_todate <-dbGetQuery(poolConn, paste(sprintf(sql_string_11, FYEND_reactive()),collapse=""))
            
            
            table_3_4 <- table_3_4_public_systems_postcon_srt_todate %>%
              left_join(table_3_4_public_systems_postcon_srt, by="asset_type")
            table_3_4 <- table_3_4[,c(1,3,2)]
            colnames(table_3_4) <- c("SMP Type","This Fiscal Year","To Date")
            table_3_4[is.na(table_3_4)] <-  0
            
            return(table_3_4)
            
            
          })
          
          table_35 <- reactive({
            
            #Section 3.1.3: CET Testing
            #Table 3.5 and the 3.1.3 paragraph
            #Column 1: Public systems with CET this fiscal year
            
            sql_string_12 <-"select count(distinct system_id) 
                                      from fieldwork.viw_capture_efficiency_full 
                                      where phase = 'Post-Construction'
                                      and test_date >= '%s'
                                      and test_date <= '%s'
                                      and public = TRUE"
            table_3_5_public_systems_cet <- dbGetQuery(poolConn, paste(sprintf(sql_string_12, FYSTART_reactive(), FYEND_reactive()),collapse=""))
            
            #Public systems with CET  TO DATE
            #Column 2: 
            
            sql_string_13 <-"select count(distinct system_id) 
                                      from fieldwork.viw_capture_efficiency_full 
                                      where phase = 'Post-Construction'
                                      and test_date <= '%s'
                                      and public = TRUE"
            table_3_5_public_systems_cet_todate <- dbGetQuery(poolConn, paste(sprintf(sql_string_13,FYEND_reactive()),collapse=""))
            
            table_3_5_public_systems_cet["To Date"] <-table_3_5_public_systems_cet_todate
            table_3_5 <-table_3_5_public_systems_cet
            colnames(table_3_5) <- c("This Fiscal Year","To Date")
            rownames(table_3_5) <- "Number of Systems with CETs Administered"
            
            
            return(table_3_5) 
          })
          
          table_36 <- reactive({
            #Section 3.1.4: Porous Pavement and PPSIRT
            #Table 3.6 and in the 3.1.4 paragraph
            #Column 1: Systems with PP/PPSIRT in this fiscal year
            
            
            sql_string_14 <-"select count(distinct admin.fun_smp_to_system(smp_id))
                                      from fieldwork.viw_porous_pavement_full
                                      where test_date >= '%s'
                                      and test_date <= '%s'
                                      and public = TRUE"
            table_3_6_systems_pp_ppsirt <- dbGetQuery(poolConn, paste(sprintf(sql_string_14, FYSTART_reactive(), FYEND_reactive()),collapse=""))
            
            #Systems with PP/PPSIRT in this fiscal year TO DATE
            #column 2
            sql_string_15 <-"select count(distinct admin.fun_smp_to_system(smp_id))
                                          from fieldwork.viw_porous_pavement_full
                                          where test_date <= '%s'
                                          and public = TRUE"
            table_3_6_systems_pp_ppsirt_todate <- dbGetQuery(poolConn, paste(sprintf(sql_string_15, FYEND_reactive()),collapse=""))
            
            table_3_6_systems_pp_ppsirt["To Date"] <- table_3_6_systems_pp_ppsirt_todate
            table_3_6 <- table_3_6_systems_pp_ppsirt
            colnames(table_3_6)<- c("This Fiscal Year","To Date")
            rownames(table_3_6)<- "Number of Systems with Infiltration Testing Administered"
            
            return(table_3_6) 
            
          })
          
          
          table_37 <- reactive({
            
            
            #Section 3.2: Public GSI Monitoring during Construction
            #3.2.1 SRTs in Construction
            
            #Table 3.7, and also the opening paragraph
            #Mid-construction SRTs performed on Public Systems
            #First column: This fiscal year
            
            sql_string_16 <- "select count(*), type
                            from fieldwork.viw_srt_full 
                            where test_date >= '%s'
                            and test_date <= '%s'
                            and phase = 'Construction'
                            and public = TRUE
                            group by type"
            
            
            
            table_3_7_public_midcon_srt <- dbGetQuery(poolConn, paste(sprintf(sql_string_16, FYSTART_reactive(), FYEND_reactive()),collapse=""))
            
            #Second column: The above, to date
            sql_string_17 <- "select count(*), type
                          from fieldwork.viw_srt_full 
                          where test_date <= '%s'
                          and phase = 'Construction'
                          and public = TRUE
                          group by type"
            
            table_3_7_public_midcon_srt_todate <- dbGetQuery(poolConn, paste(sprintf(sql_string_17, FYEND_reactive()),collapse=""))
            
            table_3_7 <- table_3_7_public_midcon_srt_todate %>%
              left_join(table_3_7_public_midcon_srt, by="type") %>%
              select(`SRT Type`=type,`This Fiscal Year`=count.y, `To Date`=count.x)
            table_3_7[is.na(table_3_7)] <-  0
            
            
            return(table_3_7) 
            
          })
          
          table_38 <- reactive({
            #Table 3.8, and the opening paragraph
            #Note: This is a count of systems, not SMPs. The table title and caption for the table in the FY21 annual report implies that it is counting SMPs.
            #The numbers in the old table are systems, not SMPs, and were generated by this query
            #I suggest we rename the table to be counting systems, and remove the caption
            #We could just as easily count SMPs, by replacing the count(distinct(srt.system_id)) with count(distinct(sfc.smp_id))
            #Column 1: Current fiscal year
            
            
            sql_string_18 <-"select sfc.asset_type, count(distinct(srt.system_id))
                              from fieldwork.viw_srt_full srt
                              left join external.mat_assets sfc on srt.system_id = sfc.system_id
                              where sfc.component_id is null
                              and test_date >= '%s'
                              and test_date <= '%s'
                              and phase = 'Construction'
                              and public = TRUE
                              group by sfc.asset_type"
            table_3_8_public_systems_duringcon_srt <- dbGetQuery(poolConn, paste(sprintf(sql_string_18, FYSTART_reactive(), FYEND_reactive()),collapse=""))
            
            #SRTs in Construction (count of systems) TO DATE
            sql_string_19 <-"select sfc.asset_type, count(distinct(srt.system_id))
                              from fieldwork.viw_srt_full srt
                              left join external.mat_assets sfc on srt.system_id = sfc.system_id
                              where sfc.component_id is null
                              and test_date <= '%s'
                              and phase = 'Construction'
                              and public = TRUE
                              group by sfc.asset_type"
            table_3_8_public_systems_duringcon_srt_todate <- dbGetQuery(poolConn, paste(sprintf(sql_string_19,FYEND_reactive()),collapse=""))
            
            table_3_8 <- table_3_8_public_systems_duringcon_srt_todate %>%
              left_join(table_3_8_public_systems_duringcon_srt, by="asset_type") 
            table_3_8[is.na(table_3_8)] <-  0
            table_3_8 <- table_3_8[,c(1,3,2)]
            colnames(table_3_8) <- c("System Type","This Fiscal Year", "To Date")
            
            
            return(table_3_8) 
            
          })
          
          
          table_39 <- reactive({
            
            #Section 3.3 Groundwater Level Monitoring for Public GSI
            #Table 3.9 and the opening paragraph
            #First column, first row - GW monitoring prior to construction of GSI this fiscal year
            
            sql_string_20 <-"select count(distinct(site_name)) from fieldwork.viw_deployment_full where smp_id is null 
                                                and deployment_dtime_est <= '%s'
                                                and (collection_dtime_est >= '%s' OR collection_dtime_est is null)
                                                and (ow_suffix LIKE 'GW_' or ow_suffix LIKE 'CW_')"
            table_3_9_public_gw_monitoring_precon <- dbGetQuery(poolConn, paste(sprintf(sql_string_20, FYEND_reactive(), FYSTART_reactive()),collapse=""))
            
            #GW monitoring prior to construction of GSI TO THIS DATE 
            #Second column, first row - the same, to date
            sql_string_21 <-"select count(distinct(site_name)) from fieldwork.viw_deployment_full where smp_id is null 
                                                and (ow_suffix LIKE 'GW_' or ow_suffix LIKE 'CW_')"
            table_3_9_public_gw_monitoring_precon_todate <- dbGetQuery(poolConn, sql_string_21)
            
            #Post-construction GW monitoring at GSI this fiscal year
            #First column, second row 
            sql_string_22 <-"select count(distinct(site_name)) from fieldwork.viw_deployment_full where smp_id is not null 
                                                and deployment_dtime_est <= '%s'
                                                and (collection_dtime_est >= '%s' OR collection_dtime_est is null)
                                                and (ow_suffix LIKE 'GW_' or ow_suffix LIKE 'CW_')"
            table_3_9_public_gw_monitoring_postcon <- dbGetQuery(poolConn, paste(sprintf(sql_string_22, FYEND_reactive(), FYSTART_reactive()),collapse=""))
            
            
            #Post-construction GW monitoring at GSI TO THIS DATE
            #Second column, second row - the same, to date
            sql_string_23 <-"select count(distinct(smp_id)) from fieldwork.viw_deployment_full where smp_id is not null 
                                                                        and (ow_suffix LIKE 'GW_' or ow_suffix LIKE 'CW_')"
            table_3_9_public_gw_monitoring_postcon_todate <- dbGetQuery(poolConn, sql_string_23)
            
            table_3_9_public_gw_monitoring_precon["To Date"] <- table_3_9_public_gw_monitoring_precon_todate
            table_3_9_public_gw_monitoring_postcon["To Date"] <- table_3_9_public_gw_monitoring_postcon_todate
            table_3_9 <- bind_rows(table_3_9_public_gw_monitoring_precon,table_3_9_public_gw_monitoring_postcon)
            rownames(table_3_9) <- c("Prior to Construction of GSI (Systems)","Post-Construction (Active GSI)")
            colnames(table_3_9) <- c("This Fiscal Year","To Date")
            
            return(table_3_9)
          })
          
          table_310 <- reactive({
            
            #Section 3.4: Post-Construction Private GSI Monitoring and Testing
            #Section 3.4.1: Private CWL Monitoring
            #Table 3.10, and also the opening paragraph
            #First column, first row - Sensor deployments this fiscal year
            
            sql_string_24 <-  "select count(*) from fieldwork.viw_deployment_full_cwl
                                  where (collection_dtime_est > '%s' OR collection_dtime_est is null)
                                  and deployment_dtime_est between '%s' and '%s'
                                  and public = FALSE"
            
            table_3_10_private_sensors_deployed <- dbGetQuery(poolConn, paste(sprintf(sql_string_24, FYSTART_reactive(), FYSTART_reactive(), FYEND_reactive()),collapse="")) 
            
            #Second column, first row - Sensor deployments to date (private)
            sql_string <- "select count(*) from fieldwork.viw_deployment_full_cwl
                                  where deployment_dtime_est < '%s'
                                  and public = FALSE"
            
            table_3_10_private_sensors_deployed_todate <- dbGetQuery(poolConn, paste(sprintf(sql_string,FYEND_reactive()),collapse="")) 
            
            #Systems monitored this fiscal year (private)
            #First column, second row - Systems monitored this fiscal year
            
            sql_string_25 <- "select count(distinct admin.fun_smp_to_system(d.smp_id)) from fieldwork.viw_deployment_full_cwl d
                                  where deployment_dtime_est between '%s' and '%s'
                                  and (collection_dtime_est >= '%s'
                                      or collection_dtime_est is null) 
                                  and d.public = false"
            table_3_10_private_systems_monitored <- dbGetQuery(poolConn, paste(sprintf(sql_string_25, FYSTART_reactive(), FYEND_reactive(), FYSTART_reactive()),collapse="")) 
            
            #Systems monitored to date (private)
            sql_string_26 <- "select count(distinct admin.fun_smp_to_system(d.smp_id)) from fieldwork.viw_deployment_full_cwl d
                                  where deployment_dtime_est <= '%s'
                                  and d.public = false"
            table_3_10_private_systems_monitored_todate <- dbGetQuery(poolConn, paste(sprintf(sql_string_26, FYEND_reactive()),collapse="")) 
            
            #Newly monitored systems this fiscal year (private)
            sql_string_27 <-"select count(distinct admin.fun_smp_to_system(newdeployments.smp_id)) FROM 
                                          	(select d.smp_id FROM fieldwork.viw_deployment_full_cwl d 
                                               group BY d.smp_id, d.public
                                               having min(d.deployment_dtime_est) > '%s'
                                               and min(d.deployment_dtime_est) <= '%s'
                                               and d.public = false) newdeployments"
            table_3_10_private_new_systems_monitored<- dbGetQuery(poolConn, paste(sprintf(sql_string_27, FYSTART_reactive(), FYEND_reactive()),collapse=""))
            
            
            `This Fiscal Year`<- data.frame(c(pull(table_3_10_private_sensors_deployed),pull(table_3_10_private_systems_monitored ),pull( table_3_10_private_new_systems_monitored)))
            `To Date`<-data.frame(c(pull(table_3_10_private_sensors_deployed_todate),pull(table_3_10_private_systems_monitored_todate),NA))
            table_3_10 <- bind_cols(`This Fiscal Year`,`To Date`)
            colnames(table_3_10)<- c("This Fiscal Year","To Date")
            rownames(table_3_10)<-c("Sensors Deployments","Systems","Systems Newly Monitored")
            
            
            table_3_10$Description <- NA
            table_3_10 <- table_3_10 %>%
              select(Description,`This Fiscal Year`, `To Date`)
            
            table_3_10$Description <- c("Number of Sensor deployments, Doesn't include SRTs, Can Include duplicated sensor ids","Private systems with CWL data","Private systems first deployed during this fiscal year")
            
            
            return(table_3_10)
            
          })
          
          
          table_311 <- reactive({
            
            #Table 3.11: Post-Construction CWL-Monitoring of Private SMPs Listed by Type 
            #First column: Monitored private SMPs to date
            
            sql_string_29 <- "select cr.\"smp_type\", count(distinct(d.smp_id)), d.public from
                  fieldwork.viw_deployment_full_cwl d
                  left join external.tbl_planreview_crosstab cr on d.smp_id = cr.\"smp_id\"::text
                  where d.smp_id is not null
                  and d.deployment_dtime_est < '%s'
                  and d.public = false
                  group by cr.\"smp_type\", d.public;"
            
            table_3_11_private_monitored_smps_postcon<- dbGetQuery(poolConn, paste(sprintf(sql_string_29,FYEND_reactive()),collapse=""))
            
            
            #Second column: Total constructed private SMPs
            
            
            # sql_string_30 <- "select count(*), cr.\"smp_type\"
            #       from external.tbl_planreview_projectsmpconcat pl
            #       left join external.tbl_planreview_designation de on de.\"ProjectID\" = pl.project_id
            #       left join external.tbl_planreview_crosstab cr on de.\"smp_id\" = cr.\"smp_id\"
            #       left join external.mat_assets sfc on de.\"smp_id\"::text = sfc.smp_id
            #       where cr.\"DCIA\" is not null
            #       and sfc.smp_id is not null
            #       and sfc.component_id is null
            #       group by cr.\"smp_type\";"
            
            
            # updated query reflecting changes in database tables
            sql_string_30 <- "with sfc as (
                                        	select distinct smp_id from external.mat_assets 
                                        	where smp_id is not null
                                        	and component_id is null
                                        ), pl as (
                                        	select distinct \"SMPID\" from external.tbl_planreview_private
                                        ), cr as (
                                        	select distinct smp_id, dcia_ft2, smp_type from external.tbl_planreview_crosstab
                                        )
                                        
                                        select count(*), cr.smp_type from pl 
                                        left join cr on pl.\"SMPID\"::text = cr.smp_id
                                        inner join sfc on pl.\"SMPID\"::text = sfc.smp_id
                                        where cr.dcia_ft2 is not null
                                        group by cr.smp_type"
            
            
            
            table_3_11_private_total_constructed_smps<- dbGetQuery(poolConn, sql_string_30)
            
            table_3_11_private_total_constructed_smps <- table_3_11_private_total_constructed_smps %>%
              select(`SMP Type` = smp_type,`Total Constructed Private SMPs`=count)
            table_3_11_private_monitored_smps_postcon <- table_3_11_private_monitored_smps_postcon %>%
              select(`SMP Type` = smp_type,`Monitored SMPs`=count)
            table_3_11 <- table_3_11_private_total_constructed_smps %>% 
              left_join(table_3_11_private_monitored_smps_postcon, by="SMP Type")
            table_3_11<-table_3_11[,c(1,3,2)]
            table_3_11[is.na(table_3_11)] <-  0
            
            return(table_3_11)
          })
          
          table_312 <- reactive({
            
            #3.4.2: Private SRTs
            #Table 3.12: Post-construction private SRTs
            #First column: This fiscal year
            sql_string_31 <-"select count(*), type
                          from fieldwork.viw_srt_full 
                          where test_date >= '%s'
                          and test_date <= '%s'
                          and phase = 'Post-Construction'
                          and public = false
                          group by type"
            
            
            table_3_12_private_postcon_srt <- dbGetQuery(poolConn, paste(sprintf(sql_string_31, FYSTART_reactive(), FYEND_reactive()),collapse=""))
            
            
            #3.4.2: Private SRTs
            #Table 3.12: Post-construction private SRTs
            #2nd column: To date
            sql_string_32 <-"select count(*), type
                        from fieldwork.viw_srt_full 
                        where test_date <= '%s'
                        and phase = 'Post-Construction'
                        and public = false
                        group by type"
            
            
            table_3_12_private_postcon_srt_todate <- dbGetQuery(poolConn, paste(sprintf(sql_string_32,FYEND_reactive()),collapse=""))
            
            table_3_12 <- table_3_12_private_postcon_srt_todate %>%
              left_join(table_3_12_private_postcon_srt, by="type") %>%
              select(`SRT Type`=type,`This Fiscal Year`=count.y, `To Date`=count.x)
            table_3_12[is.na(table_3_12)] <-  0
            
            return(table_3_12)
            
          })
          
          table_313 <- reactive({
            
            #Table 3.13: Private SMPs with Post-Construction SRTs
            #First column: This fiscal year
            
            #Table 3.13: Private SMPs with Post-Construction SRTs
            #First column: This fiscal year
            
            sql_string <- "select cr.\"smp_type\", count(distinct newtests.system_id) FROM 
	(select system_id from fieldwork.viw_srt_full srt
    group by system_id, public
    having min(test_date) >= '%s'
    and min(test_date) <= '%s'
    and public = false) newtests
    left join external.tbl_planreview_crosstab cr on newtests.system_id = cr.\"smp_id\"::text
    group by cr.\"smp_type\""
            
            
            table_3_13_private_postcon_smp_withSRT <- dbGetQuery(poolConn, paste(sprintf(sql_string, FYSTART_reactive(), FYEND_reactive()),collapse=""))
            
            #Table 3.13: Private SMPs with Post-Construction SRTs
            #2nd column: To Date
            sql_string <-"select cr.\"smp_type\", count(distinct newtests.system_id) FROM 
	(select system_id from fieldwork.viw_srt_full srt
    group by system_id, public
    having min(test_date) <= '%s'
    and public = false) newtests
    left join external.tbl_planreview_crosstab cr on newtests.system_id = cr.\"smp_id\"::text
    group by cr.\"smp_type\""
            
            table_3_13_private_postcon_smp_withSRT_todate <- dbGetQuery(poolConn, paste(sprintf(sql_string,FYEND_reactive()),collapse=""))
            
            table_3_13 <- table_3_13_private_postcon_smp_withSRT_todate %>%
              left_join(table_3_13_private_postcon_smp_withSRT, by="smp_type")
            
            table_3_13[is.na(table_3_13)] <-  0
            table_3_13<-table_3_13[,c(1,3,2)]
            colnames(table_3_13)<- c("SMP Type","This Fiscal Year","To Date")
            
            
            return(table_3_13) 
          })
          
          table_314 <- reactive({
            
            #Section 3.4.3: Private CET Testing
            #Table 3.14 and the 3.4.3 paragraph
            #Column 1: Private systems with CET this fiscal year
            sql_string_35 <-"select count(distinct system_id) 
                    from fieldwork.viw_capture_efficiency_full 
                    where phase = 'Post-Construction'
                    and test_date >= '%s'
                    and test_date <= '%s'
                    and public = FALSE"
            
            table_3_14_private_systems_cet <- dbGetQuery(poolConn, paste(sprintf(sql_string_35,FYSTART_reactive(), FYEND_reactive()),collapse=""))
            
            
            #Section 3.4.3: Private CET Testing
            #Table 3.14 and the 3.4.3 paragraph
            #Column 2: Private systems with CET to date
            sql_string_36 <-"select count(distinct system_id) 
                  from fieldwork.viw_capture_efficiency_full 
                  where phase = 'Post-Construction'
                  and test_date <= '%s'
                  and public = FALSE"
            
            table_3_14_private_systems_cet_todate <- dbGetQuery(poolConn, paste(sprintf(sql_string_36, FYEND_reactive()),collapse=""))
            
            table_3_14_private_systems_cet["To Date"] <-table_3_14_private_systems_cet_todate
            table_3_14 <-table_3_14_private_systems_cet
            colnames(table_3_14) <- c("This Fiscal Year","To Date")
            rownames(table_3_14) <- "Number of Systems with CETs Administered"
            
            
            
            
            return(table_3_14)
          })
          
          #ractable table outputs
          output$`Table 3-1` <- renderReactable(reactable(table_31(), striped = TRUE))
          output$`Table 3-2` <- renderReactable(reactable(table_32(),pagination = FALSE, striped = TRUE))
          output$`Table 3-3` <- renderReactable(reactable(table_33(), striped = TRUE))
          output$`Table 3-4` <- renderReactable(reactable(table_34(), striped = TRUE))
          output$`Table 3-5` <- renderReactable(reactable(table_35(), striped = TRUE))
          output$`Table 3-6` <- renderReactable(reactable(table_36(), striped = TRUE))
          output$`Table 3-7` <- renderReactable(reactable(table_37(), striped = TRUE))
          output$`Table 3-8` <- renderReactable(reactable(table_38(), striped = TRUE))
          output$`Table 3-9` <- renderReactable(reactable(table_39(), striped = TRUE))
          output$`Table 3-10` <- renderReactable(reactable(table_310(), striped = TRUE))
          output$`Table 3-11` <- renderReactable(reactable(table_311(), pagination = FALSE, striped = TRUE))
          output$`Table 3-12` <- renderReactable(reactable(table_312(), striped = TRUE))
          output$`Table 3-13` <- renderReactable(reactable(table_313(), striped = TRUE))
          output$`Table 3-14` <- renderReactable(reactable(table_314(), striped = TRUE))
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
              
              df_list <- list(Table_3_1=table_31(), Table_3_2=table_32(), Table_3_3=table_33(), Table_3_4=table_34(), Table_3_5=table_35(), 
                              Table_3_6=table_36(), Table_3_7=table_37(), Table_3_8=table_38(), Table_3_9=table_39(), Table_3_10=table_310(), 
                              Table_3_11=table_311(), Table_3_12=table_312(), Table_3_13=table_313(), Table_3_14=table_314())
              write.xlsx(x = df_list , file = filename, rowNames = TRUE)
            }
          ) 
          

        })
        
       
      }
    )
  }
    
        
        
  
  
  
  