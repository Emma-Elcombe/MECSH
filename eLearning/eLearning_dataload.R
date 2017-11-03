
## eLearning

```{r setup, include=FALSE}

#Packages needed
library(readxl)
library(dplyr)
library(futile.logger)

setwd("C:/Users/Tresi-Emma/Documents/R for MECSH")



```

# Logging

There are 3 levels of logging in this file TRACE (most words), INFO, and ERROR (least words)
 by default, script is logged at the INFO level
 All ERROR messages are also reported to Error folder, see log_and_quit().

```{r logging and error handling, include=FALSE}

# Logging
flog.appender(appender.file("Log/eLearning.log"), name='logger.c')
flog.info("START", name='logger.c')
flog.info("eLearning Started - Working Directory set", name='logger.c')

# set error handling.
log_and_quit <- function() {
  # Log error
  if(exists("err_msg")) {
    print('exists')
  } else {
    err_msg <- geterrmessage()
  }
  flog.error("Fatal Error: %s", err_msg, name='logger.c')
  setwd("C:/Users/Tresi-Emma/Documents/Interfaces")
  flog.appender(appender.file("eLearning.txt"), name='logger.a')
  flog.info("Failure", name='logger.a')
  setwd("C:/Users/Tresi-Emma/Desktop")
  flog.appender(appender.file("eLearning FAILED.log"), name='logger.b')
  flog.error("PEI Fetch failed: %s", err_msg, name='logger.b')
  setwd("C:/Users/Tresi-Emma/Documents/R for MECSH")
}
options(error = log_and_quit)

```

# Load elearning data 

Data is loaded from the 'R for MECSH/Load' folder. 
It arrives in this folder from the eLearnign website

```{r data loading}

#Load eLearning file
courseData <- read_excel("Load/Victoria-eLearning.xlsx", sheet = "Victoria-Current")

#Table creation - these steps are unlikely to be needed in the future, as 3 raw tables will arive, not 1 merged table.

ID_status <- courseData[c("NurseID","Location",
                        "C1Completed On","C1Status","C2Status","C3Status","C4Status",
                        "C5Status","C6Status","C6Completed On")]

# only because test load file has extra rows
ID_status <- ID_status[1:29,1:10]

ID_px <- courseData[c("NurseID","Location","LICENSEE GROUP NAME","T3NOTES","FirstName","LastName",
                      "FPM Completed", "Position", "Position2" )]

```

Completion status are calculated and checked for errors


```{r elearning completion status calucations, include=FALSE}

#Completed
ID_status$C1 <- match(ID_status$C1Status, "Completed", nomatch = 0)
ID_status$C2 <- match(ID_status$C2Status, "Completed", nomatch = 0)
ID_status$C3 <- match(ID_status$C3Status, "Completed", nomatch = 0)
ID_status$C4 <- match(ID_status$C4Status, "Completed", nomatch = 0)
ID_status$C5 <- match(ID_status$C5Status, "Completed", nomatch = 0)
ID_status$C6 <- match(ID_status$C6Status, "Completed", nomatch = 0)

ID_status$total_comp <- rowSums(ID_status[c(11:16)])
ID_status$COMPLETED <- match(ID_status$total_comp==6,1,0)


#Incomplete
ID_status$C1 <- match(ID_status$C1Status, "Incomplete", nomatch = 0)
ID_status$C2 <- match(ID_status$C2Status, "Incomplete", nomatch = 0)
ID_status$C3 <- match(ID_status$C3Status, "Incomplete", nomatch = 0)
ID_status$C4 <- match(ID_status$C4Status, "Incomplete", nomatch = 0)
ID_status$C5 <- match(ID_status$C5Status, "Incomplete", nomatch = 0)
ID_status$C6 <- match(ID_status$C6Status, "Incomplete", nomatch = 0)

ID_status$total_Incomp <- rowSums(ID_status[c(11:16)])

#Not attempted
ID_status$C1 <- match(ID_status$C1Status, "Not Attempted", nomatch = 0)
ID_status$C2 <- match(ID_status$C2Status, "Not Attempted", nomatch = 0)
ID_status$C3 <- match(ID_status$C3Status, "Not Attempted", nomatch = 0)
ID_status$C4 <- match(ID_status$C4Status, "Not Attempted", nomatch = 0)
ID_status$C5 <- match(ID_status$C5Status, "Not Attempted", nomatch = 0)
ID_status$C6 <- match(ID_status$C6Status, "Not Attempted", nomatch = 0)

ID_status$total_NotAtmpt <- rowSums(ID_status[c(11:16)])


#check all equal 6
ID_status$CHECK <- rowSums(ID_status[c(17:19)])
Avg_cnt <- mean(ID_status$CHECK, na.rm = TRUE)
if (Avg_cnt != 6){
    flog.error("CHECKING failed", name='logger.c')
    err_msg <- "CHECKING failed"
    log_and_quit()
  } else {
    flog.info("CHECKING passed", name='logger.c')
}

#clean up
ID_status$C1 <- NULL
ID_status$C2 <- NULL
ID_status$C3 <- NULL
ID_status$C4 <- NULL
ID_status$C5 <- NULL
ID_status$C6 <- NULL
ID_status$CHECK <- NULL


```



# All Staff
All_Staff <- ID_status %>% group_by(Location) %>% 
  select(Location, COMPLETED) %>% summarise (n=n(), perc_comp = mean(COMPLETED)*100)


# Staff filtered for training date
ID_status$Strt_date <- as.Date(substr(ID_status$`C1Completed On`,1,10), "%Y-%m-%d")

Filtered_Staff <- filter(ID_status, Strt_date < "2016-01-01")

Filter_Staff <- ID_status %>% filter(Strt_date < "2016-01-01") %>% group_by(Location) %>% 
  select(Location, COMPLETED) %>% summarise (n=n(), perc_comp = mean(COMPLETED)*100)


# Adding filter for Postition type
trim_px <- ID_px[,c(1,7,9)]

Staff_type <- Filtered_Staff %>% left_join(trim_px)

Filter_Staff <- Staff_type %>% filter(Strt_date < "2016-01-01", (Position2=="SW" | Position2=="Nurse")) %>% 
  group_by(Location) %>% 
  select(Location, COMPLETED) %>% summarise (n=n(), perc_comp = mean(COMPLETED)*100)


