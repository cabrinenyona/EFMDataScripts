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




## Testing with WASH data)

# R file where we call and tidy the data
source("EFM_loading_data.R")

# Excel file with the specifications in it
data_l <- import_list("EFM_shiny.xlsx")

data_l$main_page <- NULL

data_l$contents <- data_l$contents[1,]
#TODO: remove data column in spreadsheet
data_l$activities <- NULL
data_l$storybooks <- NULL
data_l$download <- NULL
data_l$demographics <- data_l$demographics[1,]
# Run the shiny dashboard
PLH_shiny(title = "EFM Research",
          data_list = data_l,
          data_frame = plhdata_org,
          status = "primary"
          )

