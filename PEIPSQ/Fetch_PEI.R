
# packages
library(readxl)
library(devtools)
install_github(repo="DerekYves/rsurveygizmo")
library(futile.logger)


## there are 3 levels of logging in this file TRACE (most words), INFO, and ERROR (least words)
#  by default, script is logged at the INFO level
#  All ERROR messages are also reported to Error folder, see log_and_quit().

setwd("C:/R")
try(rm(err_msg))

## start log file
flog.appender(appender.file("Log/PEIPSQ_fetch.log"), name='logger.c')
flog.info("START", name='logger.c')
flog.info("PEI Fetch Started - Working Directory set", name='logger.c')


### set error handling.
log_and_quit <- function() {
  # Log error
  if(exists("err_msg")) {
    print('exists')
  } else {
    err_msg <- geterrmessage()
  }
  flog.error("Fatal Error: %s", err_msg, name='logger.c')
  setwd("C:/R/BatchInterfaces/PEIPSQ")
  flog.appender(appender.file("PEI_fetch.txt"), name='logger.a')
  flog.info("Failure", name='logger.a')
  setwd("//ad.uws.edu.au/dfshare/HomesCMB$/30042685/Desktop")
  flog.appender(appender.file("PEI FETCH FAILED.log"), name='logger.b')
  flog.error("PEI Fetch failed: %s", err_msg, name='logger.b')
  setwd("C:/R")
}
options(error = log_and_quit)

##### Wrap rest of script in a function #####
Extract_Load_Data <- function(){
  
  
  #### PART A - Load "Base" Data
  
  #Load base file
  PEI_base <- read.csv("Reference/PEI_base.csv", na = c("NA", "555"))
  PEI_base$date <- as.Date(substr(PEI_base$date,1,10), "%d/%m/%Y")
  PEI_base$PEI_total <- rowSums(PEI_base[10:15])
  PEI_base$PEI_mean <- rowMeans(PEI_base[10:15])
  PEI_base$PEI_over4 <- PEI_base$PEI_total > 4
  try(PEI_base$other_notes <- NULL)
  try(PEI_base$time_end <- NULL)
  rows_fetched <- max(row(PEI_base))
  flog.info("PEI_base data.frame loaded with %s rows", rows_fetched, name='logger.c')
  
  #Load Sites file
  sites <- read_excel("Reference/sitesGizmo.xlsx", sheet = "sites")
  
  
  #### PART B - Fetch Data
  
  
  ### ROUND 1 - Jersey
  #STEP 1 - Set sites to fetch data from
  
  survey_name <-  "PEI"
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
    this_site_df$rsp_lng <- NULL
    this_site_df$rsp_lat <- NULL
    this_site_df$rsp_post <- NULL
    this_site_df$Confirmation.Email_ID14 <- NULL
    # Re-name columns
    names(this_site_df) <- c("id", "status", "date","time_start", "response_id", "Q1","Q2","Q3","Q4","Q5","Q6",
                             "age_band","age_other","comments", "country","site","region")
    # add recoding and value-adding code here
    this_site_df$response_id <- paste(sites_df[site_num,]$survey_id,"-",this_site_df$response_id, sep="")
    this_site_df$region <- paste(sites_df[site_num,]$sub_site)
    this_site_df$site <- paste(sites_df[site_num,]$site)
    this_site_df[, 6:11][this_site_df[, 6:11] == "NA"] <- NA
    this_site_df[, 6:11][this_site_df[, 6:11] == "555"] <- NA
    this_site_df$id <- NULL
    this_site_df$time_start <- NULL
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
  load_file <- sites_data[c(3,2,1,13,14,15, 10, 11, 4, 5, 6, 7, 8, 9, 12)]
  load_file$Q1 <- as.numeric(load_file$Q1)
  load_file$Q2 <- as.numeric(load_file$Q2)
  load_file$Q3 <- as.numeric(load_file$Q3)
  load_file$Q4 <- as.numeric(load_file$Q4)
  load_file$Q5 <- as.numeric(load_file$Q5)
  load_file$Q6 <- as.numeric(load_file$Q6)
  flog.trace(" - extracted data formatted", name='logger.c')
  
  # calculate and check
  score_calc_and_check <- function(load_file){
    # perform calculations 
    load_file$PEI_total <- rowSums(load_file[9:14])
    load_file$PEI_mean <- rowMeans(load_file[9:14])
    load_file$PEI_over4 <- load_file$PEI_total > 4
    flog.trace(" - calculations completed", name='logger.c')
    # Check of impossible scores
    min_score <- min(load_file$PEI_total, na.rm = TRUE)
    max_score <- max(load_file$PEI_total, na.rm = TRUE)
    flog.trace("min score: %s", min_score, name='logger.c')
    flog.trace("max score: %s", max_score, name='logger.c')
    # log result
    if (min_score < 0 | max_score > 12){
      flog.error("Data NOT in correct range", name='logger.c')
      err_msg <- "Data NOT in correct range"
      log_and_quit()
    } else {
      flog.info("Data in correct range", name='logger.c')
    }
    return(load_file)
  }
  
  # perforam calculations, check totals & save load file for site
  PEI_1jer_load_file <- score_calc_and_check(load_file)
  flog.info("%s load_file created", site_name, name='logger.c')
  
  
  
  ### ROUND 2 - SydneyLHD
  
  #STEP 1
  survey_name <-  "PEI"
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
    this_site_df$rsp_lng <- NULL
    this_site_df$rsp_lat <- NULL
    this_site_df$rsp_post <- NULL
    this_site_df$Confirmation.Email_ID14 <- NULL
    # Re-name columns
    names(this_site_df) <- c("id", "status", "date","time_start", "response_id", "Q1","Q2","Q3","Q4","Q5","Q6",
                             "age_band","age_other", "country","site","region")
    # add recoding and value-adding code here
    this_site_df$response_id <- paste(sites_df[site_num,]$survey_id,"-",this_site_df$response_id, sep="")
    this_site_df$region <- paste(sites_df[site_num,]$sub_site)
    this_site_df$site <- paste(sites_df[site_num,]$site)
    this_site_df[, 6:11][this_site_df[, 6:11] == "NA"] <- NA
    this_site_df[, 6:11][this_site_df[, 6:11] == "555"] <- NA
    this_site_df$id <- NULL
    this_site_df$time_start <- NULL
    # return the dataframe
    return(this_site_df)
  }
  
  # assemble sites data
  try(rm(sites_data))
  sites_data <- Load_site_data()
  
  
  ##STEP 3 - Clean downloaded data and format to match "Base" Data
  
  # reorder variables to match
  load_file <- data.frame(c(sites_data[c(3,2,1,12,13,14, 10, 11, 4, 5, 6, 7, 8, 9)]),comments="")
  load_file$Q1 <- as.numeric(load_file$Q1)
  load_file$Q2 <- as.numeric(load_file$Q2)
  load_file$Q3 <- as.numeric(load_file$Q3)
  load_file$Q4 <- as.numeric(load_file$Q4)
  load_file$Q5 <- as.numeric(load_file$Q5)
  load_file$Q6 <- as.numeric(load_file$Q6)
  flog.trace(" - extracted data formatted", name='logger.c')
  
  # perforam calculations, check totals & save load file for site
  PEI_2syd_load_file <- score_calc_and_check(load_file)
  flog.trace("%s load_file created", site_name, name='logger.c')
  
  
  ### ROUND 3 - Lewisham
  
  #STEP 1
  survey_name <-  "PEI"
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
    this_site_df$rsp_lng <- NULL
    this_site_df$rsp_lat <- NULL
    this_site_df$rsp_post <- NULL
    this_site_df$Confirmation.Email_ID15 <- NULL
    # Re-name columns
    names(this_site_df) <- c("id", "status", "date","time_start", "response_id", "Q1","Q2","Q3","Q4","Q5","Q6",
                             "age_band","age_other","comments", "country","site","region")
    # add recoding and value-adding code here
    this_site_df$response_id <- paste(sites_df[site_num,]$survey_id,"-",this_site_df$response_id, sep="")
    this_site_df$region <- paste(sites_df[site_num,]$sub_site)
    this_site_df$site <- paste(sites_df[site_num,]$site)
    this_site_df$country <- paste("UK")
    this_site_df[, 6:11][this_site_df[, 6:11] == "NA"] <- NA
    this_site_df[, 6:11][this_site_df[, 6:11] == "555"] <- NA
    this_site_df[, 12][this_site_df[, 12] == "555"] <- "Other"
    this_site_df$id <- NULL
    this_site_df$time_start <- NULL
    # return the dataframe
    return(this_site_df)
  }
  
  # assemble sites data
  try(rm(sites_data))
  sites_data <- Load_site_data()
  
  
  ##STEP 3 - Clean downloaded data and format to match "Base" Data
  
  # reorder variables to match
  load_file <- sites_data[c(3,2,1,13,14,15, 10, 11, 4, 5, 6, 7, 8, 9, 12)]
  load_file$Q1 <- as.numeric(load_file$Q1)
  load_file$Q2 <- as.numeric(load_file$Q2)
  load_file$Q3 <- as.numeric(load_file$Q3)
  load_file$Q4 <- as.numeric(load_file$Q4)
  load_file$Q5 <- as.numeric(load_file$Q5)
  load_file$Q6 <- as.numeric(load_file$Q6)
  flog.trace(" - extracted data formatted", name='logger.c')
  
  # perforam calculations, check totals & save load file for site
  PEI_3lew_load_file <- score_calc_and_check(load_file)
  flog.info("%s load_file created", site_name, name='logger.c')
  
  
  ### ROUND 4 - Plymouth
  
  #STEP 1
  survey_name <-  "PEI"
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
    this_site_df$rsp_lng <- NULL
    this_site_df$rsp_lat <- NULL
    this_site_df$rsp_post <- NULL
    this_site_df$Confirmation.Email_ID15 <- NULL
    this_site_df$rsp_region <- NULL
    this_site_df$X11option10042other <- NULL
    # Re-name columns
    names(this_site_df) <- c("id", "status", "date","time_start", "response_id", "Q1","Q2","Q3","Q4","Q5","Q6","region",
                             "age_band","age_other", "comments","country","site")
    # add recoding and value-adding code here
    this_site_df$response_id <- paste(sites_df[site_num,]$survey_id,"-",this_site_df$response_id, sep="")
    this_site_df$country <- paste("UK")
    this_site_df$site <- paste(sites_df[site_num,]$site)
    this_site_df[, 6:11][this_site_df[, 6:11] == "NA"] <- NA
    this_site_df[, 6:11][this_site_df[, 6:11] == "555"] <- NA
    this_site_df[, 13][this_site_df[, 13] == "555"] <- "Other"
    this_site_df$id <- NULL
    this_site_df$time_start <- NULL
    # return the dataframe
    return(this_site_df)
  }
  
  # assemble sites data
  try(rm(sites_data))
  sites_data <- Load_site_data()
  
  
  ##STEP 3 - Clean downloaded data and format to match "Base" Data
  
  # reorder variables to match
  load_file <- sites_data[c(3,2,1,14,15, 10, 11,12, 4, 5, 6, 7, 8, 9, 13)]
  load_file$Q1 <- as.numeric(load_file$Q1)
  load_file$Q2 <- as.numeric(load_file$Q2)
  load_file$Q3 <- as.numeric(load_file$Q3)
  load_file$Q4 <- as.numeric(load_file$Q4)
  load_file$Q5 <- as.numeric(load_file$Q5)
  load_file$Q6 <- as.numeric(load_file$Q6)
  flog.trace(" - extracted data formatted", name='logger.c')
  
  # perforam calculations, check totals & save load file for site
  PEI_4ply_load_file <- score_calc_and_check(load_file)
  flog.trace("%s load_file created", site_name, name='logger.c')
  
  
  ### ROUND 5 - Somerset
  
  #STEP 1
  survey_name <-  "PEI"
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
    this_site_df$rsp_lng <- NULL
    this_site_df$rsp_lat <- NULL
    this_site_df$rsp_post <- NULL
    this_site_df$Confirmation.Email_ID15 <- NULL
    this_site_df$rsp_region <- NULL
    # Data in this column might be of use, but to view it all other sites would need to have the column added as well.
    this_site_df$Client.group_ID17 <- NULL
    # Re-name columns
    names(this_site_df) <- c("id", "status", "date","time_start", "response_id", "Q1","Q2","Q3","Q4","Q5","Q6","region",
                             "age_band","age_other", "comments","country","site")
    # add recoding and value-adding code here
    this_site_df$response_id <- paste(sites_df[site_num,]$survey_id,"-",this_site_df$response_id, sep="")
    this_site_df$country <- paste("UK")
    this_site_df$site <- paste(sites_df[site_num,]$site)
    this_site_df[, 6:11][this_site_df[, 6:11] == "NA"] <- NA
    this_site_df[, 6:11][this_site_df[, 6:11] == "555"] <- NA
    this_site_df[, 13][this_site_df[, 13] == "555"] <- "Other"
    this_site_df$id <- NULL
    this_site_df$time_start <- NULL
    # return the dataframe
    return(this_site_df)
  }
  
  # assemble sites data
  try(rm(sites_data))
  sites_data <- Load_site_data()
  
  
  ##STEP 3 - Clean downloaded data and format to match "Base" Data
  
  # reorder variables to match
  load_file <- sites_data[c(3,2,1,14,15, 10, 11,12, 4, 5, 6, 7, 8, 9, 13)]
  load_file$Q1 <- as.numeric(load_file$Q1)
  load_file$Q2 <- as.numeric(load_file$Q2)
  load_file$Q3 <- as.numeric(load_file$Q3)
  load_file$Q4 <- as.numeric(load_file$Q4)
  load_file$Q5 <- as.numeric(load_file$Q5)
  load_file$Q6 <- as.numeric(load_file$Q6)
  flog.trace(" - extracted data formatted", name='logger.c')
  
  # perforam calculations, check totals & save load file for site
  PEI_5som_load_file <- score_calc_and_check(load_file)
  flog.info("%s load_file created", site_name, name='logger.c')
  
  
  ### ROUND 6 - right @ home - Victoria
  
  #STEP 1
  survey_name <-  "PEI"
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
    this_site_df$rsp_lng <- NULL
    this_site_df$rsp_lat <- NULL
    this_site_df$rsp_post <- NULL
    #this_site_df$rsp_region <- NULL
    # Re-name columns
    names(this_site_df) <- c("id", "status", "date","time_start", "response_id", "Q1","Q2","Q3","Q4","Q5","Q6",
                             "age_band","age_other","comments","country","site","region")
    # add recoding and value-adding code here
    this_site_df$response_id <- paste(sites_df[site_num,]$survey_id,"-",this_site_df$response_id, sep="")
    this_site_df$region <- paste(sites_df[site_num,]$sub_site)
    this_site_df$site <- paste(sites_df[site_num,]$site)
    this_site_df[, 6:11][this_site_df[, 6:11] == "NA"] <- NA
    this_site_df[, 6:11][this_site_df[, 6:11] == "555"] <- NA
    this_site_df[, 13][this_site_df[, 13] == "555"] <- "Other"
    this_site_df$id <- NULL
    this_site_df$time_start <- NULL
    # return the dataframe
    return(this_site_df)
  }
  
  # assemble sites data
  try(rm(sites_data))
  sites_data <- Load_site_data()
  
  
  ##STEP 3 - Clean downloaded data and format to match "Base" Data
  
  # reorder variables to match
  load_file <- sites_data[c(3,2,1,13,14,15, 10, 11, 4, 5, 6, 7, 8, 9, 12)]
  load_file$Q1 <- as.numeric(load_file$Q1)
  load_file$Q2 <- as.numeric(load_file$Q2)
  load_file$Q3 <- as.numeric(load_file$Q3)
  load_file$Q4 <- as.numeric(load_file$Q4)
  load_file$Q5 <- as.numeric(load_file$Q5)
  load_file$Q6 <- as.numeric(load_file$Q6)
  flog.trace(" - extracted data formatted", name='logger.c')
  
  # perforam calculations, check totals & save load file for site
  PEI_6vic_load_file <- score_calc_and_check(load_file)
  flog.info("%s load_file created", site_name, name='logger.c')
  
  
  ### ROUND 7 - VirginCare
  
  #STEP 1
  survey_name <-  "PEI"
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
    this_site_df$rsp_lng <- NULL
    this_site_df$rsp_lat <- NULL
    this_site_df$rsp_post <- NULL
    this_site_df$Confirmation.Email_ID14 <- NULL
    this_site_df$PEI.Total_ID16 <- NULL
    # Re-name columns
    names(this_site_df) <- c("id", "status", "date","time_start", "response_id", "Q1","Q2","Q3","Q4","Q5","Q6",
                             "age_band","age_other","comments", "country","site","region")
    # add recoding and value-adding code here
    this_site_df$response_id <- paste(sites_df[site_num,]$survey_id,"-",this_site_df$response_id, sep="")
    this_site_df$region <- paste(sites_df[site_num,]$sub_site)
    this_site_df$site <- paste(sites_df[site_num,]$site)
    this_site_df$country <- paste("UK")
    this_site_df[, 6:11][this_site_df[, 6:11] == "NA"] <- NA
    this_site_df[, 6:11][this_site_df[, 6:11] == "555"] <- NA
    this_site_df[, 12][this_site_df[, 12] == "555"] <- "Other"
    this_site_df$id <- NULL
    this_site_df$time_start <- NULL
    # return the dataframe
    return(this_site_df)
  }
  
  # assemble sites data
  try(rm(sites_data))
  sites_data <- Load_site_data()
  
  
  ##STEP 3 - Clean downloaded data and format to match "Base" Data
  
  # reorder variables to match
  load_file <- sites_data[c(3,2,1,13,14,15, 10, 11, 4, 5, 6, 7, 8, 9, 12)]
  load_file$Q1 <- as.numeric(load_file$Q1)
  load_file$Q2 <- as.numeric(load_file$Q2)
  load_file$Q3 <- as.numeric(load_file$Q3)
  load_file$Q4 <- as.numeric(load_file$Q4)
  load_file$Q5 <- as.numeric(load_file$Q5)
  load_file$Q6 <- as.numeric(load_file$Q6)
  flog.trace(" - extracted data formatted", name='logger.c')
  
  # perforam calculations, check totals & save load file for site
  PEI_7Esx_load_file <- score_calc_and_check(load_file)
  flog.info("%s load_file created", site_name, name='logger.c')
  
  
  ### ROUND 8 - Southern NSW LHD
  
  #STEP 1
  survey_name <-  "PEI"
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
    this_site_df$rsp_lng <- NULL
    this_site_df$rsp_lat <- NULL
    this_site_df$rsp_post <- NULL
    # Re-name columns
    names(this_site_df) <- c("id", "status", "date","time_start", "response_id", "Q1","Q2","Q3","Q4","Q5","Q6",
                             "age_band","age_other","comments", "country","site","region")
    # add recoding and value-adding code here
    this_site_df$response_id <- paste(sites_df[site_num,]$survey_id,"-",this_site_df$response_id, sep="")
    this_site_df$region <- paste(sites_df[site_num,]$sub_site)
    this_site_df$site <- paste(sites_df[site_num,]$site)
    this_site_df[, 6:11][this_site_df[, 6:11] == "NA"] <- NA
    this_site_df[, 6:11][this_site_df[, 6:11] == "555"] <- NA
    this_site_df$id <- NULL
    this_site_df$time_start <- NULL
    # return the dataframe
    return(this_site_df)
  }
  
  # assemble sites data
  try(rm(sites_data))
  sites_data <- Load_site_data()
  
  
  ##STEP 3 - Clean downloaded data and format to match "Base" Data
  
  # reorder variables to match
  load_file <- sites_data[c(3,2,1,13,14,15, 10, 11, 4, 5, 6, 7, 8, 9, 12)]
  load_file$Q1 <- as.numeric(load_file$Q1)
  load_file$Q2 <- as.numeric(load_file$Q2)
  load_file$Q3 <- as.numeric(load_file$Q3)
  load_file$Q4 <- as.numeric(load_file$Q4)
  load_file$Q5 <- as.numeric(load_file$Q5)
  load_file$Q6 <- as.numeric(load_file$Q6)
  flog.trace(" - extracted data formatted", name='logger.c')
  
  # perforam calculations, check totals & save load file for site
  PEI_8sthn_load_file <- score_calc_and_check(load_file)
  flog.info("%s load_file created", site_name, name='logger.c')
  
  
  #### PART C - Combine files and Export
  
  # Merge data files
  merged <- rbind(PEI_base, PEI_1jer_load_file, PEI_2syd_load_file, PEI_3lew_load_file, 
                  PEI_4ply_load_file, PEI_5som_load_file, PEI_6vic_load_file, PEI_7Esx_load_file, PEI_8sthn_load_file)
  
  rows_fetched <- max(row(merged))
  flog.info("PEI_base merged with load files", name='logger.c')
  flog.info("In total %s rows in merge file", rows_fetched, name='logger.c')
  
  # Check of impossible scores
  min_score <- min(merged$PEI_total, na.rm = TRUE)
  max_score <- max(merged$PEI_total, na.rm = TRUE)
  flog.trace("min score: %s", min_score, name='logger.c')
  flog.trace("max score: %s", max_score, name='logger.c')
  
  # check for duplicates
  duplicates <- sum(duplicated(merged$response_id))
  flog.trace("number of duplicates: %s", duplicates, name='logger.c')
  
  
  ###### STOP ######
  # If Min does not equal 0, max score <=12 and duplicates equal zero, then STOP ####
  if (duplicates > 0 | min_score != 0 | max_score != 12){
    flog.error("Data NOT in correct range", name='logger.c')
    err_msg <- "Data NOT in correct range"
    log_and_quit()
  } else {
    flog.info("Data in correct range", name='logger.c')
  }
  
  # add todays date
  extract_date <- as.character(Sys.Date())
  merged <- data.frame(merged[c(1:18)],extract_date)
  
  # Clean up
  flog.trace("clean up files", name='logger.c')
  rm(load_file)
  rm(PEI_sites)
  rm(sites_data)
  try(rm(err_msg))
  
  
  #### Part D - export
  
  # export PEI data to load folder
  PEI_file_name <- paste("Load/PEI_Data.csv", sep = "")
  write.table(merged, file = PEI_file_name, sep=",", row.names = FALSE)
  flog.info("File %s saved into Load folder", PEI_file_name, name='logger.c')
  
  # export PEI data to archive folder
  PEI_file_name <- paste("Archive/PEI_Data ", extract_date,".csv", sep = "")
  write.table(merged, file = PEI_file_name, sep=",", row.names = FALSE)
  flog.info("File %s saved into Archive folder", PEI_file_name, name='logger.c')
  flog.info("END", PEI_file_name, name='logger.c')
  
  setwd("C:/R/BatchInterfaces/PEIPSQ")
  flog.appender(appender.file("PEI_fetch.txt"), name='logger.a')
  flog.info("Success", name='logger.a')
  
}

tryCatch(Extract_Load_Data(), finally = log_and_quit())

