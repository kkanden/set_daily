# sql
library(DBI)
library(odbc)
# data manipulation
library(data.table)
library(stringi)
library(dplyr)
library(tidyr)
# shiny
library(DT)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyalert)
library(shinyBS)
library(bslib)
library(bsicons)
library(plotly)


connect <- function() {
  con <- DBI::dbConnect(RPostgres::Postgres(),
    host = "kanden-kanden-b7d6.d.aivencloud.com",
    dbname = "defaultdb",
    user = config::get("user"),
    password = config::get("pwd"),
    port = 13499
  )
}

disconnect <- function(con) {
  DBI::dbDisconnect(con)
}

con <- connect()


completion_times <- dbGetQuery(con, "SELECT * FROM completion_times") |>
  as.data.table()


players <- dbGetQuery(con, "SELECT * FROM players") |>
  as.data.table()

disconnect(con)
