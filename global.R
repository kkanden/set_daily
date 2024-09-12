# sql
library(DBI)
library(odbc)
# data manipulation
library(data.table)
library(tidyverse)
library(stringi)
# shiny
library(DT)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyalert)
library(shinyBS)
library(bslib)

con_string <- "Driver={ODBC Driver 18 for SQL Server};
Server=tcp:kanden.database.windows.net,1433;
Database=set_daily;
Uid=__USER__;
Pwd=__PASSWD__;
Encrypt=yes;
TrustServerCertificate=no;
Connection Timeout=60;"

connect <- function(){
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = con_string %>% 
      stringi::stri_replace_all(fixed = "__USER__", Sys.getenv("azure_user")) %>% 
      stringi::stri_replace_all(fixed = "__PASSWD__", Sys.getenv("azure_pwd")) 
  )
}

disconnect <- function(con){
  DBI::dbDisconnect(con)
}

con <- connect()


completion_times <- dbGetQuery(con, "SELECT * FROM completion_times") %>% 
  as.data.table


players <- dbGetQuery(con, "SELECT * FROM players") %>% 
  as.data.table
  




disconnect(con)



seconds_to_string <- function(seconds){
  if(is.na(seconds)){
    return(NA)
  }
  
  minutes <- as.integer(seconds / 60)
  secs <- as.integer(seconds %% 60)
  milliseconds <- as.integer((seconds - floor(seconds)) * 1000)
  
  formatted_time <- sprintf("%d:%02d.%03d",
                            minutes,
                            secs,
                            milliseconds)
  return(formatted_time)
}

string_to_seconds <- function(string){
  split <- stringi::stri_split(string, regex = "[:.]")[[1]]
  
  mins <- as.integer(split[1])
  secs <- as.integer(split[2])
  msecs <- fifelse(length(split) == 3,
                   split[3],
                   "0")
  
  msecs <- fcase(msecs == "0", 0L,
                 nchar(msecs) == 1, as.integer(msecs) * 100L,
                 nchar(msecs) == 2, as.integer(msecs) * 10L,
                 nchar(msecs) == 3, as.integer(msecs)
                 )
  
  
  time_sec <- mins * 60 + secs + msecs / 1000
  return(time_sec)
}

