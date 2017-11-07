
# packages
library(readxl)
library(devtools)
install_github(repo="DerekYves/rsurveygizmo")
library(futile.logger)


## there are 3 levels of logging in this file TRACE (most words), INFO, and ERROR (least words)
#  by default, script is logged at the INFO level
#  All ERROR messages are also reported to Error folder, see log_and_quit().

setwd("C:/Users/Tresi-Emma/Documents/R for MECSH")
try(rm(err_msg))

## start log file
flog.appender(appender.file("Log/PEIPSQ_fetch.log"), name='logger.c')
flog.info("START", name='logger.c')
flog.info("PSQ Fetch Started - Working Directory set", name='logger.c')


### set error handling.
log_and_quit <- function() {
  # Log error
  if(exists("err_msg")) {
    print('exists')
  } else {
    err_msg <- geterrmessage()
  }
  flog.error("Fatal Error: %s", err_msg, name='logger.c')
  setwd("C:/Users/Tresi-Emma/Documents/Interfaces")
  flog.appender(appender.file("PSQ_fetch.txt"), name='logger.a')
  flog.info("Failure", name='logger.a')
  setwd("C:/Users/Tresi-Emma/Desktop")
  flog.appender(appender.file("PSQ FETCH FAILED.log"), name='logger.b')
  flog.error("PSQ Fetch failed: %s", err_msg, name='logger.b')
  setwd("C:/Users/Tresi-Emma/Documents/R for MECSH")
  #options(error = recover)
}
options(error = log_and_quit)

##### Wrap rest of script in a function #####
Extract_Load_Data <- function(){
  
  
  #### PART A - Load "Base" Data
  
  #Load base file
  PSQ_base <- read.csv("Reference/PSQ_base.csv", na = c("NA", "555"))
  PSQ_base$date <- as.Date(substr(PSQ_base$time_end,1,10), "%d/%m/%Y")
  PSQ_base$PSQ_total <- rowSums(PSQ_base[10:19])
  PSQ_base$PSQ_mean <- rowMeans(PSQ_base[10:19])
  PSQ_base$PSQ_under20 <- PSQ_base$PSQ_total < 20
  try(PSQ_base$Other_Notes <- NULL)
  try(PSQ_base$time_end <- NULL)
  rows_fetched <- max(row(PSQ_base))
  flog.info("PSQ_base data.frame loaded with %s rows", rows_fetched, name='logger.c')
  
  #Load Sites file
  sites <- read_excel("Reference/sitesGizmo.xlsx", sheet = "sites")
  
  
  #### PART B - Fetch Data
  
  ### ROUND 1 - Jersey
  
  #STEP 1 - Set sites to fetch data from
  survey_name <-  "PSQ"
  site_name <-  "Jersey"
  
  set_fetch_sites <- function(survey_name, site_name){
    fetch_sites <- subset(sites, sites$survey == survey_name & sites$sub_site != "Base" & sites$site == site_name) 
    return(fetch_sites)
  }
  
  ## STEP 2 - Fetch data
  
  # for each site...
  get_site_data <- function(sites_df,site_num) {
    # get the Survey ID
    survey_id <- sites_df[site_num,]$survey_id
    # download the data direct into dataframe
    flog.info("%s survey being fetched", survey_id, name='logger.c')
    this_site_df <-Rsurveygizmo::pullsg(survey_id, api="c46686a8c42e2972b07e59cbdc2f6e6e43932276e5e942953d", completes_only = TRUE, clean = TRUE)
    rows_fetched <- max(row(this_site_df))
    flog.info(" - %s rows added", rows_fetched, name='logger.c')
    # remove unnecessary columns 
    this_site_df$id <- NULL
    this_site_df$datestarted <- NULL  
    this_site_df$rsp_lng <- NULL
    this_site_df$rsp_lat <- NULL
    this_site_df$rsp_post <- NULL
    this_site_df$Confirmation.Email_ID26 <- NULL
    # Re-name columns
    names(this_site_df) <- c("status", "date", "response_id", "Q1", "age_band","age_other", "Q2","Q3","Q4","Q5","Q6",
                             "Q7", "Q8", "Q9", "Q10", "comments", "country", "site", "region")
    # add recoding and value-adding code here
    this_site_df$response_id <- paste(sites_df[site_num,]$survey_id,"-",this_site_df$response_id, sep="")
    this_site_df$region <- paste(sites_df[site_num,]$sub_site)
    this_site_df$site <- paste(sites_df[site_num,]$site)
    # return the dataframe
    return(this_site_df)
  }
  
  # assemble sites data
  # first delete any existing sites data
  try(rm(fetch_sites))
  
  # now loop over all sites
  Load_site_data <- function(){
    rows_fetched <- max(row(subset(sites, sites$survey == survey_name & sites$sub_site != "Base" & sites$site == site_name) ))
    flog.info("%s %s sites selected", rows_fetched, site_name, name='logger.c')
    for (i in seq_len(rows_fetched)) {
      fetch_sites <- set_fetch_sites(survey_name,site_name)
      if (exists("fetched_data")) {
        fetched_data <- rbind(fetched_data, get_site_data(fetch_sites,i))
        flog.trace(" - rows added to sites_data", name='logger.c')
      } else {
        fetched_data <- get_site_data(fetch_sites,i)
        flog.trace(" - rows added to sites_data", name='logger.c')
      }
    } 
    rows_fetched <- max(row(fetched_data))
    flog.info("In total %s rows were extracted", rows_fetched, name='logger.c')
    return(fetched_data)
  }
  
  sites_data <- Load_site_data()
  
  
  ##STEP 3 - Clean downloaded data and format to match "Base" Data
  
  # reorder variables to match
  load_file <- sites_data[c(3,2,1,17,18,19, 5, 6, 4, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)]
  flog.trace(" - extracted data formatted", name='logger.c')
  
  # calculate and check
  score_calc_and_check <- function(load_file){
    # perform calculations 
    load_file$PSQ_total <- rowSums(load_file[9:18])
    load_file$PSQ_mean <- rowMeans(load_file[9:18])
    load_file$PSQ_under20 <- load_file$PSQ_total < 20
    flog.trace(" - calculations completed", name='logger.c')
    # Check of impossible scores
    min_score <- min(load_file$PSQ_total, na.rm = TRUE)
    max_score <- max(load_file$PSQ_total, na.rm = TRUE)
    flog.trace("min score: %s", min_score, name='logger.c')
    flog.trace("max score: %s", max_score, name='logger.c')
    # log result
    if (min_score < 10 | max_score > 50){
      flog.error("Data NOT in correct range", name='logger.c')
      err_msg <- "Data NOT in correct range"
      log_and_quit()
    } else {
      flog.info("Data in correct range", name='logger.c')
    }
    return(load_file)
  }
  
  # perforam calculations, check totals & save load file for site
  PSQ_1jer_load_file <- score_calc_and_check(load_file)
  flog.info("%s load_file created", site_name, name='logger.c')
  
  
  
  
  ### ROUND 2 - SydneyLHD
  
  #STEP 1 - Set sites to fetch data from
  survey_name <-  "PSQ"
  site_name <-  "SydneyLHD"
  
  
  ## STEP 2 - Fetch data
  
  # for each site...
  get_site_data <- function(sites_df,site_num) {
    # get the Survey ID
    survey_id <- sites_df[site_num,]$survey_id
    # download the data direct into dataframe
    flog.info("%s survey being fetched", survey_id, name='logger.c')
    this_site_df <-Rsurveygizmo::pullsg(survey_id, api="c46686a8c42e2972b07e59cbdc2f6e6e43932276e5e942953d", completes_only = TRUE, clean = TRUE)
    rows_fetched <- max(row(this_site_df))
    flog.info(" - %s rows added", rows_fetched, name='logger.c')
    # remove unnecessary columns 
    this_site_df$id <- NULL
    this_site_df$datestarted <- NULL  
    this_site_df$rsp_lng <- NULL
    this_site_df$rsp_lat <- NULL
    this_site_df$rsp_post <- NULL
    this_site_df$Confirmation.Email..Health.Families_ID25 <- NULL
    # Re-name columns
    names(this_site_df) <- c("status", "date", "response_id", "Q1", "age_band","age_other", "Q2","Q3","Q4","Q5","Q6",
                             "Q7", "Q8", "Q9", "Q10", "country", "site", "region")
    # add recoding and value-adding code here
    this_site_df$response_id <- paste(sites_df[site_num,]$survey_id,"-",this_site_df$response_id, sep="")
    this_site_df$region <- paste(sites_df[site_num,]$sub_site)
    this_site_df$site <- paste(sites_df[site_num,]$site)
    # return the dataframe
    return(this_site_df)
  }
  
  # assemble sites data
  try(rm(sites_data))
  sites_data <- Load_site_data()
  
  
  ##STEP 3 - Clean downloaded data and format to match "Base" Data
  
  # reorder variables to match - and add comments column
  load_file <- data.frame(c(sites_data[c(3,2,1,16,17,18, 5, 6, 4, 7, 8, 9, 10, 11, 12, 13, 14, 15)]),comments="")
  flog.trace(" - extracted data formatted", name='logger.c')
  
  # perform calculations
  PSQ_2syd_load_file <- score_calc_and_check(load_file)
  flog.info("%s load_file created", site_name, name='logger.c')
  
  ### ROUND 3 - Lewisham
  
  #STEP 1
  survey_name <-  "PSQ"
  site_name <-  "Lewisham"
  
  
  ## STEP 2 - Fetch data
  
  # for each site...
  get_site_data <- function(sites_df,site_num) {
    # get the Survey ID
    survey_id <- sites_df[site_num,]$survey_id
    # download the data direct into dataframe
    flog.info("%s survey being fetched", survey_id, name='logger.c')
    this_site_df <-Rsurveygizmo::pullsg(survey_id, api="c46686a8c42e2972b07e59cbdc2f6e6e43932276e5e942953d", completes_only = TRUE, clean = TRUE)
    rows_fetched <- max(row(this_site_df))
    flog.info(" - %s rows added", rows_fetched, name='logger.c')
    # remove unnecessary columns 
    this_site_df$id <- NULL
    this_site_df$datestarted <- NULL  
    this_site_df$rsp_lng <- NULL
    this_site_df$rsp_lat <- NULL
    this_site_df$rsp_post <- NULL
    this_site_df$Confirmation.Email_ID27 <- NULL
    # Re-name columns
    names(this_site_df) <- c("status", "date", "response_id", "Q1",  "Q2","Q3","Q4","Q5","Q6", "Q7", "Q8", "Q9", "Q10",
                             "age_band","age_other", "comments","country", "site", "region")
    # add recoding and value-adding code here
    this_site_df$response_id <- paste(sites_df[site_num,]$survey_id,"-",this_site_df$response_id, sep="")
    this_site_df$country <- paste("UK")
    this_site_df$region <- paste(sites_df[site_num,]$sub_site)
    this_site_df$site <- paste(sites_df[site_num,]$site)
    # return the dataframe
    return(this_site_df)
  }
  
  # assemble sites data
  try(rm(sites_data))
  sites_data <- Load_site_data()
  
  
  ##STEP 3 - Clean downloaded data and format to match "Base" Data
  
  # reorder variables to match
  load_file <- sites_data[c(3,2,1,17,18,19, 14, 15, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 16)]
  load_file$Q3 <- as.numeric(load_file$Q3)
  load_file$Q10 <- as.numeric(load_file$Q10)
  flog.trace(" - extracted data formatted", name='logger.c')
  
  # perform calculations
  PSQ_3lew_load_file <- score_calc_and_check(load_file)
  flog.info("%s load_file created", site_name, name='logger.c')
  
  ### ROUND 4 - Plymouth
  
  #STEP 1
  survey_name <-  "PSQ"
  site_name <-  "Plymouth"
  
  ## STEP 2 - Fetch data
  
  # for each site...
  get_site_data <- function(sites_df,site_num) {
    # get the Survey ID
    survey_id <- sites_df[site_num,]$survey_id
    # download the data direct into dataframe
    flog.info("%s survey being fetched", survey_id, name='logger.c')
    this_site_df <-Rsurveygizmo::pullsg(survey_id, api="c46686a8c42e2972b07e59cbdc2f6e6e43932276e5e942953d", completes_only = TRUE, clean = TRUE)
    rows_fetched <- max(row(this_site_df))
    flog.info(" - %s rows added", rows_fetched, name='logger.c')
    # remove unnecessary columns 
    this_site_df$id <- NULL
    this_site_df$datestarted <- NULL  
    this_site_df$rsp_lng <- NULL
    this_site_df$rsp_lat <- NULL
    this_site_df$rsp_post <- NULL
    this_site_df$Confirmation.Email_ID27 <- NULL
    this_site_df$X11option10086other <- NULL
    this_site_df$rsp_region <- NULL
    # Re-name columns
    names(this_site_df) <- c("status", "date", "response_id", "Q1", "region", "Q2","Q3","Q4","Q5","Q6",
                             "Q7", "Q8", "Q9", "Q10", "age_band","age_other","comments", "country", "site")
    # add recoding and value-adding code here
    this_site_df$response_id <- paste(sites_df[site_num,]$survey_id,"-",this_site_df$response_id, sep="")
    this_site_df$country <- paste("UK")
    this_site_df$site <- paste(sites_df[site_num,]$site)
    # return the dataframe
    return(this_site_df)
  }
  
  # assemble sites data
  try(rm(sites_data))
  sites_data <- Load_site_data()
  
  
  ##STEP 3 - Clean downloaded data and format to match "Base" Data
  
  # reorder variables to match
  load_file <- sites_data[c(3,2,1,18,19, 5, 15, 16, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 17)]
  flog.trace(" - extracted data formatted", name='logger.c')
  
  # perform calculations
  PSQ_4ply_load_file <- score_calc_and_check(load_file)
  flog.info("%s load_file created", site_name, name='logger.c')
  
  
  
  ### ROUND 5 - Somerset
  
  #STEP 1
  survey_name <-  "PSQ"
  site_name <-  "Somerset"
  
  ## STEP 2 - Fetch data
  
  # for each site...
  get_site_data <- function(sites_df,site_num) {
    # get the Survey ID
    survey_id <- sites_df[site_num,]$survey_id
    # download the data direct into dataframe
    flog.info("%s survey being fetched", survey_id, name='logger.c')
    this_site_df <-Rsurveygizmo::pullsg(survey_id, api="c46686a8c42e2972b07e59cbdc2f6e6e43932276e5e942953d", completes_only = TRUE, clean = TRUE)
    rows_fetched <- max(row(this_site_df))
    flog.info(" - %s rows added", rows_fetched, name='logger.c')
    # remove unnecessary columns 
    this_site_df$id <- NULL
    this_site_df$datestarted <- NULL  
    this_site_df$rsp_lng <- NULL
    this_site_df$rsp_lat <- NULL
    this_site_df$rsp_post <- NULL
    this_site_df$Confirmation.Email_ID27 <- NULL
    this_site_df$Client.group_ID28 <- NULL
    this_site_df$rsp_region <- NULL
    # Re-name columns
    names(this_site_df) <- c("status", "date", "response_id", "Q1", "region", "Q2","Q3","Q4","Q5","Q6",
                             "Q7", "Q8", "Q9", "Q10", "age_band","age_other","comments", "country", "site")
    # add recoding and value-adding code here
    this_site_df$response_id <- paste(sites_df[site_num,]$survey_id,"-",this_site_df$response_id, sep="")
    this_site_df$country <- paste("UK")
    this_site_df$site <- paste(sites_df[site_num,]$site)
    # return the dataframe
    return(this_site_df)
  }
  
  # assemble sites data
  try(rm(sites_data))
  sites_data <- Load_site_data()
  
  
  ##STEP 3 - Clean downloaded data and format to match "Base" Data
  
  # reorder variables to match
  load_file <- sites_data[c(3,2,1,18,19, 5, 15, 16, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 17)]
  flog.trace(" - extracted data formatted", name='logger.c')
  
  # perform calculations
  PSQ_5som_load_file <- score_calc_and_check(load_file)
  flog.info("%s load_file created", site_name, name='logger.c')
  
  
  ### ROUND 6 - right@home victoria
  
  #STEP 1
  survey_name <-  "PSQ"
  site_name <-  "RH_Victoria"
  
  ## STEP 2 - Fetch data
  
  # for each site...
  get_site_data <- function(sites_df,site_num) {
    # get the Survey ID
    survey_id <- sites_df[site_num,]$survey_id
    # download the data direct into dataframe
    flog.info("%s survey being fetched", survey_id, name='logger.c')
    this_site_df <-Rsurveygizmo::pullsg(survey_id, api="c46686a8c42e2972b07e59cbdc2f6e6e43932276e5e942953d", completes_only = TRUE, clean = TRUE)
    rows_fetched <- max(row(this_site_df))
    flog.info(" - %s rows added", rows_fetched, name='logger.c')
    # remove unnecessary columns 
    this_site_df$id <- NULL
    this_site_df$datestarted <- NULL  
    this_site_df$rsp_lng <- NULL
    this_site_df$rsp_lat <- NULL
    this_site_df$rsp_post <- NULL
    # Re-name columns
    names(this_site_df) <- c("status", "date", "response_id", "Q1", "age_band","age_other", "Q2","Q3","Q4","Q5","Q6",
                             "Q7", "Q8", "Q9", "Q10","comments", "country", "site" ,"region")
    # add recoding and value-adding code here
    this_site_df$response_id <- paste(sites_df[site_num,]$survey_id,"-",this_site_df$response_id, sep="")
    this_site_df$region <- paste(sites_df[site_num,]$sub_site)
    this_site_df$site <- paste(sites_df[site_num,]$site)
    # return the dataframe
    return(this_site_df)
  }
  
  # assemble sites data
  try(rm(sites_data))
  sites_data <- Load_site_data()
  
  
  ##STEP 3 - Clean downloaded data and format to match "Base" Data
  
  # reorder variables to match
  load_file <- sites_data[c(3,2,1,17,18,19, 5, 6, 4, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)]
  flog.trace(" - extracted data formatted", name='logger.c')
  
  # perform calculations
  PSQ_6vic_load_file <- score_calc_and_check(load_file)
  flog.info("%s load_file created", site_name, name='logger.c')
  
  
  ### ROUND 7 - VirginCare
  
  #STEP 1
  survey_name <-  "PSQ"
  site_name <-  "VirginCare"
  
  
  ## STEP 2 - Fetch data
  
  # for each site...
  get_site_data <- function(sites_df,site_num) {
    # get the Survey ID
    survey_id <- sites_df[site_num,]$survey_id
    # download the data direct into dataframe
    flog.info("%s survey being fetched", survey_id, name='logger.c')
    this_site_df <-Rsurveygizmo::pullsg(survey_id, api="c46686a8c42e2972b07e59cbdc2f6e6e43932276e5e942953d", completes_only = TRUE, clean = TRUE)
    rows_fetched <- max(row(this_site_df))
    flog.info(" - %s rows added", rows_fetched, name='logger.c')
    # remove unnecessary columns 
    this_site_df$id <- NULL
    this_site_df$datestarted <- NULL  
    this_site_df$rsp_lng <- NULL
    this_site_df$rsp_lat <- NULL
    this_site_df$rsp_post <- NULL
    this_site_df$PSQ.Total_ID30 <- NULL
    #this_site_df$Confirmation.Email_ID27 <- NULL
    # Re-name columns
    names(this_site_df) <- c("status", "date", "response_id", "Q1", "age_band","age_other", "Q2","Q3","Q4","Q5","Q6", "Q7", "Q8", "Q9", "Q10",
                              "comments","country", "site", "region")
    # add recoding and value-adding code here
    this_site_df$response_id <- paste(sites_df[site_num,]$survey_id,"-",this_site_df$response_id, sep="")
    this_site_df$country <- paste("UK")
    this_site_df$region <- paste(sites_df[site_num,]$sub_site)
    this_site_df$site <- paste(sites_df[site_num,]$site)
    # return the dataframe
    return(this_site_df)
  }
  
  # assemble sites data
  try(rm(sites_data))
  sites_data <- Load_site_data()
  
  ##STEP 3 - Clean downloaded data and format to match "Base" Data
  
  # reorder variables to match
  load_file <- sites_data[c(3,2,1,17,18,19, 5, 6, 4, 7, 8, 9, 10, 11, 12, 13,14,15, 16)]
  flog.trace(" - extracted data formatted", name='logger.c')
  
  # perform calculations
  PSQ_7Esx_load_file <- score_calc_and_check(load_file)
  flog.info("%s load_file created", site_name, name='logger.c')
  
  
  ### ROUND 8 - Southern NSW LHD
  
  #STEP 1
  survey_name <-  "PSQ"
  site_name <-  "SNSWLHD"
  
  
  ## STEP 2 - Fetch data
  
  # for each site...
  get_site_data <- function(sites_df,site_num) {
    # get the Survey ID
    survey_id <- sites_df[site_num,]$survey_id
    # download the data direct into dataframe
    flog.info("%s survey being fetched", survey_id, name='logger.c')
    this_site_df <-Rsurveygizmo::pullsg(survey_id, api="c46686a8c42e2972b07e59cbdc2f6e6e43932276e5e942953d", completes_only = TRUE, clean = TRUE)
    rows_fetched <- max(row(this_site_df))
    flog.info(" - %s rows added", rows_fetched, name='logger.c')
    # remove unnecessary columns 
    this_site_df$id <- NULL
    this_site_df$datestarted <- NULL  
    this_site_df$rsp_lng <- NULL
    this_site_df$rsp_lat <- NULL
    this_site_df$rsp_post <- NULL
    # Re-name columns
    names(this_site_df) <- c("status", "date", "response_id", "Q1", "age_band","age_other", "Q2","Q3","Q4","Q5","Q6",
                             "Q7", "Q8", "Q9", "Q10", "comments", "country", "site", "region")
    # add recoding and value-adding code here
    this_site_df$response_id <- paste(sites_df[site_num,]$survey_id,"-",this_site_df$response_id, sep="")
    this_site_df$region <- paste(sites_df[site_num,]$sub_site)
    this_site_df$site <- paste(sites_df[site_num,]$site)
    # return the dataframe
    return(this_site_df)
  }
  
  # assemble sites data
  try(rm(sites_data))
  sites_data <- Load_site_data()
  
  
  ##STEP 3 - Clean downloaded data and format to match "Base" Data
  
  # reorder variables to match
  load_file <- sites_data[c(3,2,1,17,18,19, 5, 6, 4, 7, 8, 9, 10, 11, 12, 13,14,15, 16)]
  flog.trace(" - extracted data formatted", name='logger.c')
  
  # perform calculations
  PSQ_8sthn_load_file <- score_calc_and_check(load_file)
  flog.info("%s load_file created", site_name, name='logger.c')
  
  
  #### PART C - Combine files and Export
  
  # Merge data files
  merged <- rbind(PSQ_base, PSQ_1jer_load_file, PSQ_2syd_load_file, PSQ_3lew_load_file, 
                  PSQ_4ply_load_file, PSQ_5som_load_file, PSQ_6vic_load_file, PSQ_7Esx_load_file, PSQ_8sthn_load_file)
  
  rows_fetched <- max(row(merged))
  flog.info("PSQ_base merged with load files", name='logger.c')
  flog.info("In total %s rows in merge file", rows_fetched, name='logger.c')
  
  
  # Check of impossible scores
  min_score <- min(merged$PSQ_total, na.rm = TRUE)
  max_score <- max(merged$PSQ_total, na.rm = TRUE)
  flog.trace("min score: %s", min_score, name='logger.c')
  flog.trace("max score: %s", max_score, name='logger.c')
  
  # check for duplicates
  duplicates <- sum(duplicated(merged$response_id))
  flog.trace("number of duplicates: %s", duplicates, name='logger.c')
  
  
  ###### STOP ######
  # If Min < 10, max score >=50 and duplicates equal zero, then STOP ####
  if (duplicates > 0 | min_score < 10 | max_score > 50){
    flog.error("data NOT in correct range", name='logger.c')
    err_msg <- "data NOT in correct range"
    log_and_quit()
  } else {
    flog.info("data in correct range", name='logger.c')
  }
  
  # add todays date
  extract_date <- as.character(Sys.Date())
  merged <- data.frame(merged[c(1:22)],extract_date)
  
  # Clean up
  flog.trace("clean up files", name='logger.c')
  rm(load_file)
  rm(PSQ_sites)
  rm(sites_data)
  
  
  #### Part D - export
  
  # export PSQ data to load folder
  PSQ_file_name <- paste("Load/PSQ_Data.csv", sep = "")
  write.table(merged, file = PSQ_file_name, sep="\t", row.names = FALSE)
  flog.info("File %s saved into Load folder", PSQ_file_name, name='logger.c')
  
  # export PSQ data to archive folder
  PSQ_file_name <- paste("Archive/PSQ_Data ", extract_date,".csv", sep = "")
  write.table(merged, file = PSQ_file_name, sep="\t", row.names = FALSE)
  flog.info("File %s saved into Archive folder", PSQ_file_name, name='logger.c')
  flog.info("END", PSQ_file_name, name='logger.c')
  
  setwd("C:/Users/Tresi-Emma/Documents/Interfaces")
  flog.appender(appender.file("PSQ_fetch.txt"), name='logger.a')
  flog.error("Success", name='logger.a')
  
}

tryCatch(Extract_Load_Data(), finally = log_and_quit())
