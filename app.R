# Template file to run PLHR changes.
library(rio)
library(plhR)
library(shiny)
#library(shinythemes)
library(shinyjs)
library(plotly)
library(shinydashboard)
library(jsonlite)
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

# setwd("~/GitHub/plhR/test")

## Testing with WASH data)

# R file where we call and tidy the data
source("Personal Setup.R")
data_l <- import_list("EFM_shiny (1).xlsx")
source("EFM_loading_data.R")


# Excel file with the specifications in it
data_l <- import_list("EFM_shiny (1).xlsx")
data_l$contents <- data_l$contents[1:3,]


# Run the shiny dashboard
PLH_shiny(title = "EFM Research",
          data_list = data_l,
          data_frame = plhdata_org,
          status = "primary")

