library(tidyverse)
library(lubridate)
library(odbc)
 # start_date1 = ymd_hms("2022-04-01 00:00:00"),
 # start_date2 = ymd_hms("2022-07-01 00:00:00"),
 # end_date1 = ymd_hms("2022-06-30 23:59:59"),
 # end_date2 = ymd_hms("2022-09-30 23:59:59"),
 # fy1 = "FY22",
 # fy2 = "FY23",
 # quater1 = "Q4",
 # quarter2 = "Q1"

#Preamble
inputs <- list(public_query_text = " AND public = TRUE",
 phase = NULL,
 start_date = c(ymd_hms("2022-04-01 00:00:00"), ymd_hms("2022-07-01 00:00:00")),
 end_date = c(ymd_hms("2022-06-30 23:59:59"), ymd_hms("2022-09-30 23:59:59")),
 fy = c("FY22", "FY23"),
 quarter = c("Q4", "Q1"),
 quarter_range = NULL,
 to_date_range = NULL,
 stringsAsFactors=FALSE)

mars <- dbConnect(odbc(), "mars_testing")

#Public post-construction
inputs$phase <- "Post-Construction"
outputs <- list()
outputs[["Old Quarter"]] <- list()
outputs[["New Quarter"]] <- list()

#Public sensors deployed
for(i in 1:2){
  query <- paste0("SELECT distinct sensor_serial FROM fieldwork.deployment_full_cwl 
   WHERE deployment_dtime_est <= '", inputs$end_date[i], "'
   AND (collection_dtime_est >= '", inputs$start_date[i], "' 
   OR collection_dtime_est IS NULL) AND 
   public = TRUE") 

  results <- dbGetQuery(mars, query)
  outputs[[i]][["Quarter: PuPC Sensors Deployed"]] <- results
}

#systems with CWL monitoring this quarter
for(i in 1:2){
  query <- paste0("SELECT DISTINCT smp_to_system(d.smp_id) FROM fieldwork.deployment_full_cwl d
   WHERE deployment_dtime_est <= '", inputs$end_date[i], "'
   AND (collection_dtime_est >= '", inputs$start_date[i], "'
   OR collection_dtime_est IS NULL)
   AND d.public = TRUE")
  results <- dbGetQuery(mars, query)
  outputs[[i]][["Quarter: PuPC CWL Systems"]] <- results
}

#No. of public systems monitored To-Date
for(i in 1:2){
  query <- paste0("SELECT DISTINCT smp_to_system(d.smp_id) as system_id FROM fieldwork.deployment_full_cwl d
    WHERE d.public = TRUE and deployment_dtime_est <= '", inputs$end_date[i], "' and smp_to_system(d.smp_id) is not null")
  results <- dbGetQuery(mars, query)
  outputs[[i]][["To-Date: PuPC CWL Systems"]] <- results
}

#Systems newly with cwl data this quarter
for(i in 1:2){
  query <- paste0("with fq_input as (select fiscal_quarter_lookup_uid from public.fiscal_quarter_lookup WHERE fiscal_quarter = 'FY",
   str_sub(inputs$fy[i],-2),inputs$quarter[i],"') ,
   
   newest_smps as (select distinct smp_id, system_id from ow_leveldata_entrydates 
   where fiscal_quarter_lookup_uid = (select max(fiscal_quarter_lookup_uid) from fq_input)),
   
   previous_date as (select max(fiscal_quarter_lookup_uid) as previous_date from ow_leveldata_entrydates 
   where fiscal_quarter_lookup_uid < (select max(fiscal_quarter_lookup_uid)from fq_input)),
   
   previous_smps as (select distinct smp_id, system_id from ow_leveldata_entrydates 
   where fiscal_quarter_lookup_uid <= (select previous_date from previous_date))
   
   select * from newest_smps 
   where system_id not in (select system_id from previous_smps) -- System never given CWL before
   and smp_id like '%-%-%' -- Public SMPs only")
  results <- dbGetQuery(mars, query)
  outputs[[i]][["Quarter: PuPC New Systems"]] <- results
}

#Tests
inputs$quarter_range <- c(paste0(" AND test_date >= '", inputs$start_date[1], "' AND test_date <= '", inputs$end_date[1], "'"),
                          paste0(" AND test_date >= '", inputs$start_date[2], "' AND test_date <= '", inputs$end_date[2], "'"))
inputs$to_date_range <- c(paste0(" AND test_date <= '", inputs$end_date[1], "'"),
                          paste0(" AND test_date <= '", inputs$end_date[2], "'"))
#Pre inspections
for(i in 1:2){
  query <- paste0("SELECT * FROM fieldwork.srt_full
   WHERE phase = '", inputs$phase, "' AND
   type = 'Pre-Inspection Dye Test'", inputs$quarter_range[i], inputs$public_query_text)
  results <- dbGetQuery(mars, query)
  outputs[[i]][["Quarter: PuPC Pre-Inspections"]] <- results
}



for(i in 1:2){
  query <- paste0("SELECT * FROM fieldwork.srt_full
   WHERE phase = '", inputs$phase, "' AND
   type = 'Pre-Inspection Dye Test'", inputs$to_date_range[i], inputs$public_query_text)
  results <- dbGetQuery(mars, query)
  outputs[[i]][["To-Date: PuPC Pre-Inspections"]] <- results
}



#Dye tests
for(i in 1:2){
  query <- paste0("SELECT * FROM fieldwork.srt_full
   WHERE phase = '", inputs$phase, "' AND
   type = 'CCTV Dye Test'", inputs$quarter_range[i], inputs$public_query_text)
  results <- dbGetQuery(mars, query)
  outputs[[i]][["Quarter: PuPC Dye Tests"]] <- results
}



for(i in 1:2){
  query <- paste0("SELECT * FROM fieldwork.srt_full
   WHERE phase = '", inputs$phase, "' AND
   type = 'CCTV Dye Test'", inputs$to_date_range[i], inputs$public_query_text)
  results <- dbGetQuery(mars, query)
  outputs[[i]][["To-Date: PuPC Dye Tests"]] <- results
}


#Performance tests
for(i in 1:2){
  query <- paste0("SELECT * FROM fieldwork.srt_full
   WHERE phase = '", inputs$phase, "' AND
   type = 'Performance Test'", inputs$quarter_range[i], inputs$public_query_text)
  results <- dbGetQuery(mars, query)
  outputs[[i]][["Quarter: PuPC Performance Tests"]] <- results
}

for(i in 1:2){
  query <- paste0("SELECT * FROM fieldwork.srt_full
   WHERE phase = '", inputs$phase, "' AND
   type = 'Performance Test'", inputs$to_date_range[i], inputs$public_query_text)
  results <- dbGetQuery(mars, query)
  outputs[[i]][["To-Date: PuPC Performance Tests"]] <- results
}


#CET
for(i in 1:2){
  query <- paste0("with old_systems as (select system_id, min(test_date) as earliest_test 
    from fieldwork.capture_efficiency_full where phase = '", inputs$phase, "' AND test_date < '", 
    inputs$start_date[i], "' ", inputs$public_query_text, " AND low_flow_bypass_observed IS NOT NULL
    group by system_id)
  
  SELECT distinct system_id FROM fieldwork.capture_efficiency_full 
   WHERE phase = '", inputs$phase, "'", inputs$quarter_range[i], inputs$public_query_text, 
                  " AND low_flow_bypass_observed IS NOT NULL and system_id not in (select system_id from old_systems)")
  results <- dbGetQuery(mars, query)
  outputs[[i]][["Quarter: PuPC New Capture Efficiency"]] <- results
}



for(i in 1:2){
  query <- paste0("SELECT distinct system_id FROM fieldwork.capture_efficiency_full 
   WHERE phase = '", inputs$phase, "'", inputs$to_date_range[i], inputs$public_query_text,
                  " AND low_flow_bypass_observed IS NOT NULL")
  results <- dbGetQuery(mars, query)
  outputs[[i]][["To-Date: PuPC Capture Efficiency"]] <- results
}



#PPSIRT
for(i in 1:2){
  query <- paste0("SELECT * FROM fieldwork.porous_pavement_smp_averages
   WHERE test_date >= '", inputs$start_date[i], "' AND
   test_date <= '", inputs$end_date[i], "'")
  results <- dbGetQuery(mars, query)
  outputs[[i]][["Quarter: PuPC Porous Pavement"]] <- results
}



for(i in 1:2){
  query <- paste0("SELECT COUNT(*) - 10 as count FROM fieldwork.porous_pavement_smp_averages
    WHERE test_date <= '", inputs$end_date[i], "'")
  results <- dbGetQuery(mars, query)
  outputs[[i]][["To-Date: Porous Pavement"]] <- results
}


#Public mid-construction
inputs$phase <- "Construction"

#Pre inspections
for(i in 1:2){
  query <- paste0("SELECT * FROM fieldwork.srt_full
   WHERE phase = '", inputs$phase, "' AND
   type = 'Pre-Inspection Dye Test'", inputs$quarter_range[i], inputs$public_query_text)
  results <- dbGetQuery(mars, query)
  outputs[[i]][["Quarter: PuMC Pre-Inspections"]] <- results
}

for(i in 1:2){
  query <- paste0("SELECT * FROM fieldwork.srt_full
   WHERE phase = '", inputs$phase, "' AND
   type = 'Pre-Inspection Dye Test'", inputs$to_date_range[i], inputs$public_query_text)
  results <- dbGetQuery(mars, query)
  outputs[[i]][["To-Date: PuMC Pre-Inspections"]] <- results
}

#Dye tests
for(i in 1:2){
  query <- paste0("SELECT * FROM fieldwork.srt_full
   WHERE phase = '", inputs$phase, "' AND
   type = 'CCTV Dye Test'", inputs$quarter_range[i], inputs$public_query_text)
  results <- dbGetQuery(mars, query)
  outputs[[i]][["Quarter: PuMC Dye Tests"]] <- results
}
for(i in 1:2){
  query <- paste0("SELECT * FROM fieldwork.srt_full
   WHERE phase = '", inputs$phase, "' AND
   type = 'CCTV Dye Test'", inputs$to_date_range[i], inputs$public_query_text)
  results <- dbGetQuery(mars, query)
  outputs[[i]][["To-Date: PuMC Dye Tests"]] <- results
}

#Performance tests
for(i in 1:2){
  query <- paste0("SELECT * FROM fieldwork.srt_full
   WHERE phase = '", inputs$phase, "' AND
   type = 'Performance Test'", inputs$quarter_range[i], inputs$public_query_text)
  results <- dbGetQuery(mars, query)
  outputs[[i]][["Quarter: PuMC Performance Tests"]] <- results
}

for(i in 1:2){
  query <- paste0("SELECT * FROM fieldwork.srt_full
   WHERE phase = '", inputs$phase, "' AND
   type = 'Performance Test'", inputs$to_date_range[i], inputs$public_query_text)
  results <- dbGetQuery(mars, query)
  outputs[[i]][["To-Date: PuMC Performance Tests"]] <- results
}

#Private Post-Construction
inputs$phase <- "Post-Construction"
inputs$public_query_text = " AND public = FALSE"

#Private sensors deployed
for(i in 1:2){
  query <- paste0("SELECT distinct(sensor_serial) FROM fieldwork.deployment_full_cwl 
   WHERE deployment_dtime_est <= '", inputs$end_date[i], "'
   AND (collection_dtime_est >= '", inputs$start_date[i], "' 
   OR collection_dtime_est IS NULL) AND 
   public = FALSE") 

  results <- dbGetQuery(mars, query)
  outputs[[i]][["Quarter: VtPC Sensors Deployed"]] <- results
}

#systems with CWL monitoring this quarter
for(i in 1:2){
  query <- paste0("SELECT DISTINCT smp_to_system(d.smp_id) FROM fieldwork.deployment_full_cwl d
   WHERE deployment_dtime_est <= '", inputs$end_date[i], "'
   AND (collection_dtime_est >= '", inputs$start_date[i], "'
   OR collection_dtime_est IS NULL)
   AND d.public = FALSE")
  results <- dbGetQuery(mars, query)
  outputs[[i]][["Quarter: VtPC CWL Systems"]] <- results
}

#No. of private systems monitored To-Date
for(i in 1:2){
  query <- paste0("SELECT DISTINCT smp_to_system(d.smp_id) FROM fieldwork.deployment_full_cwl d
    WHERE d.public = FALSE and deployment_dtime_est <= '", inputs$end_date[i], "'")
  results <- dbGetQuery(mars, query)
  outputs[[i]][["To-Date: VtPC CWL Systems"]] <- results
}

#Private Tests
#Pre inspections
for(i in 1:2){
  query <- paste0("SELECT * FROM fieldwork.srt_full
   WHERE phase = '", inputs$phase, "' AND
   type = 'Pre-Inspection Dye Test'", inputs$quarter_range[i], inputs$public_query_text)
  results <- dbGetQuery(mars, query)
  outputs[[i]][["Quarter: VtPC Pre-Inspections"]] <- results
}



for(i in 1:2){
  query <- paste0("SELECT * FROM fieldwork.srt_full
   WHERE phase = '", inputs$phase, "' AND
   type = 'Pre-Inspection Dye Test'", inputs$to_date_range[i], inputs$public_query_text)
  results <- dbGetQuery(mars, query)
  outputs[[i]][["To-Date: VtPC Pre-Inspections"]] <- results
}



#Dye tests
for(i in 1:2){
  query <- paste0("SELECT * FROM fieldwork.srt_full
   WHERE phase = '", inputs$phase, "' AND
   type = 'CCTV Dye Test'", inputs$quarter_range[i], inputs$public_query_text)
  results <- dbGetQuery(mars, query)
  outputs[[i]][["Quarter: VtPC Dye Tests"]] <- results
}



for(i in 1:2){
  query <- paste0("SELECT * FROM fieldwork.srt_full
   WHERE phase = '", inputs$phase, "' AND
   type = 'CCTV Dye Test'", inputs$to_date_range[i], inputs$public_query_text)
  results <- dbGetQuery(mars, query)
  outputs[[i]][["To-Date: VtPC Dye Tests"]] <- results
}


#Performance tests
for(i in 1:2){
  query <- paste0("SELECT * FROM fieldwork.srt_full
   WHERE phase = '", inputs$phase, "' AND
   type = 'Performance Test'", inputs$quarter_range[i], inputs$public_query_text)
  results <- dbGetQuery(mars, query)
  outputs[[i]][["Quarter: VtPC Performance Tests"]] <- results
}

for(i in 1:2){
  query <- paste0("SELECT * FROM fieldwork.srt_full
   WHERE phase = '", inputs$phase, "' AND
   type = 'Performance Test'", inputs$to_date_range[i], inputs$public_query_text)
  results <- dbGetQuery(mars, query)
  outputs[[i]][["To-Date: VtPC Performance Tests"]] <- results
}



