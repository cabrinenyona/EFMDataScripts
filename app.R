# Template file to run PLHR changes.
library(rio)
library(plhR)
library(shiny)
#library(shinythemes) 
library(shinyjs)
library(plotly)
library(shinydashboard)
library(jsonlite)
library(rjson)
library(here)     
library(ggplot2)
library(tibble)
library(stringr)
library(forcats)
library(lubridate)
library(purrr)
library(tidyr)
library(dplyr)
library(gt)
library(readxl)
library(postgresr)
library(ggthemes)
library(shinyauthr)
library(zoo)

# R files where we call and tidy the data
source("Personal Setup.R")
source("Functions.R")
source("EFM_loading_data.R")
source("Credentials_data.R")

# Excel file with the specifications in it
data_l <- import_list("EFM_shiny.xlsx")

# Run the shiny dashboard
PLH_shiny(title = "EFM Research",
          data_list = data_l,
          data_frame = plhdata_org,
          status = "primary",
          key_var = "id")
