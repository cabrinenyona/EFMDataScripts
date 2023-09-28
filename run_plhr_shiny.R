# Template file to run PLHR changes.
library(rio)
library(plhR)
library(readxl)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(here)

## Testing with WASH data)

# Excel file with the specifications in it
data_l <- import_list("WASH_shiny.xlsx")

# R file where we call and tidy the data
source("WASH_setup.R")

# To illustrate calling multiple data frames in the shiny system ------------------
# this is under the modules tab, where we tried a different data set to check it worked with
# mutliple data sets :) 
flow_checkin_data <- readRDS("flow_checkin_data.RDS")

# This here wouldn't usually be needed, but is only here to illustrate the "main_page" options
# usually you would do this with things in the WASH data :) 
df <- readRDS("df.RDS")
our_data <- our_data[1:489,]
our_data <- bind_cols(df, our_data)

# Run the shiny dashboard
PLH_shiny(title = "EFM Research",
          data_list = data_l,
          data_frame = our_data,
          status = "primary",
          colour = "blue")



