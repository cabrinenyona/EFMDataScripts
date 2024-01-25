#install.packages("RPostgres")
library(RPostgres)
library(DBI)

#' AIM: We want to be able to call the original data, clean it / remove the irrelevant ones
#' and then save that so we can call just this reduced data set.

#Connect to Database to get original data
plh_con <- DBI::dbConnect(RPostgres::Postgres(),
                     dbname = 'plh',
                     host = 'apps-server.idems.international',
                     port = 5432,
                     user = 'parent_app',
                     password = 'parent_app')

# TODO: explore do a query (filter)
#DBI::dbReadTable(conn = plh_con, name = "app_users")


#Connect to Database to write cleaned data
# parent_app_con <- dbConnect(RPostgres::Postgres(),
#                             dbname = 'parent_app',
#                             host = 'apps-server.idems.international',
#                             port = 5432,
#                             user = 'parent_app',
#                             password = 'parent_app')
# 
# parent_app_tables <- dbListTables(plh_con)

# x <- system.time(DBI::dbReadTable(conn = plh_con, name = "app_users"))
# 
# y <- system.time(DBI::dbReadTable(conn = parent_app_con, name = "plhdata_org_clean"))
# 
# x; y

#dbListTables(parent_app_con)
#plhdata_org <- DBI::dbReadTable(conn = parent_app_con, name = "plhdata_org_clean")
#plhdata_org <- DBI::dbReadTable(conn = parent_app_con, name = "Cleaned PLH data")
