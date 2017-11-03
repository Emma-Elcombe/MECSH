
# load columns from all PEI and PSQ in gizmo
# This code gets all the columns for all the PEI and PSQ surveys, it allows for a survey by survey 
# compariosn of which columns are present in each survey

# NOTE: Only works on surveys with data.

# set up

library(readxl)
library(devtools)
install_github(repo="DerekYves/rsurveygizmo")

setwd("C:/Users/Tresi-Emma/Documents/R for MECSH")

# get sites
sites <- read_excel("Reference/sitesGizmo.xlsx", sheet = "sites")
fetch_sites <- subset(sites, sites$sub_site != "Base" & (sites$survey =="PEI" | sites$survey=="PSQ"))

get_site_data <- function(sites_df,site_num) {
  # get the Survey ID
  survey_id <- sites_df[site_num,]$survey_id
  survey <- sites_df[site_num,]$survey
  site <- sites_df[site_num,]$site
  sub_site <- sites_df[site_num,]$sub_site
  report_name <- sites_df[site_num,]$report_name
  # download the data direct into dataframe
  this_site_df <-Rsurveygizmo::pullsg(survey_id, api="c46686a8c42e2972b07e59cbdc2f6e6e43932276e5e942953d", completes_only = TRUE, clean = TRUE)
  nexft <-  data.frame(colnames(this_site_df),survey_id,survey, site, sub_site, report_name)
  # return the dataframe
  return(nexft)
}

Load_fields <- function(){
  rows_fetched <- max(row(subset(fetch_sites)))
  for (i in seq_len(rows_fetched)) {
    if (exists("fetched_data")) {
      fetched_data <- rbind(fetched_data, get_site_data(fetch_sites,i))
    } else {
      fetched_data <- get_site_data(fetch_sites,i)
    }
  } 
  return(fetched_data)
}


fields_data <- Load_fields()

# export data to PEIPSQ folder
file_name <- paste("MECSH/PEIPSQ/Fields.csv", sep = "")
write.table(fields_data, file = file_name, sep=",", row.names = FALSE)

