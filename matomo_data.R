#' Visitors > User IDs > date range > import
#' 
#' UUID? Check.
#' Visits: If a visitor comes for the first time, or if they visit a page more than 30 minutes after their last page view, this is a new visit
#' Actions: The number of actions performed (page views, site searches, downloads, outlinks)
#' Actions per visit: average number of actions per visits
#' avg time on website: Average duration of a visit (seconds)
#' Bounces: number of visits that only had a single page view (so the visitor left the website directly from the entrance page)
#' Bounce proportion: proportion of visits that only had a single page view (so the visitor left the website directly from the entrance page)
#' Time on Website: total time spent by visitor (in seconds) over period. - note that I calculated this so it can be out by about 10 seconds.
#' 
#' Unique visitors (daily sum)
#' Users (daily sum)

#' Conversion rate: 
library(rjson)

token_matomo <- read.table("token_matomo", quote="\"", comment.char="")

calling_matomo_data <- function(date_from = "2023-10-25", date_to = "2024-10-25", data = plhdata_org,
                                type = c("none", "EFM_KE", "EFM_not_KE"),
                                token = token_matomo){
  type <- match.arg(type)
  
  if (type == "EFM_KE"){
    segment_name <- "segment=pageTitle%3D%3DEarly%252520Family%252520Math;countryCode%3D%3Dke"
  } else if (type == "EFM_not_KE"){
    segment_name <- "segment=pageTitle%3D%3DEarly%252520Family%252520Math;countryCode!%3Dke"
  } else {
    segment_name <- "segment="
  }
  
  json_file <- paste0("https://apps-server.idems.international/analytics/index.php?apiAction=getUsers&apiModule=UserId&date=", date_from, ",", date_to, "&expanded=1&filter_limit=-1&format=JSON&idSite=1&method=API.getProcessedReport&module=API&period=range&", segment_name, "&token_auth=", token)
  
  json_data <- jsonlite::fromJSON(txt=json_file, flatten = TRUE)
  
  our_data <- json_data$reportData
  names(our_data) <- c("UUID", "Visits", "Actions", "C", "D", "Actions per visit", "Avg. Time on Website", "Bounce Rate")
  our_data <- our_data %>% dplyr::select(-c("C", "D"))
  our_data$`Bounce proportion` <- as.numeric(as.character(stringr::str_split(our_data$`Bounce Rate`, "%", simplify = TRUE)[,1]))/100
  our_data <- our_data %>% mutate(Bounce = round(`Bounce proportion` * Visits, 0)) %>% dplyr::select(-c("Bounce Rate"))
  our_data$`Avg. Time on Website` <- period_to_seconds(hms(x = our_data$`Avg. Time on Website`, format = "%H:%M:%S"))[1:length(our_data$`Avg. Time on Website`)]
  our_data$`Time on Website` <- our_data$`Avg. Time on Website` * our_data$Visits # this is calculated so can be out by 10 seconds or so
  
  valid_uuid <- data[["app_user_id"]]
  #data <- data %>% dplyr::select(c(UUID = app_user_id))
  our_data <- our_data %>% filter(UUID %in% valid_uuid)
  
  return(our_data)
}

# We can pull the data from matomo - e.g., for EFM app users who are from anywhere except Kenya
efm_not_kenya <- calling_matomo_data(type = "EFM_not_KE")

# for EFM app users who are just from Kenya:
efm_kenya <- calling_matomo_data(type = "EFM_KE")

# their IDs:
not_kenya_ids <- efm_not_kenya$UUID
kenya_ids <- efm_kenya$UUID

plhdata_org <- plhdata_org %>% mutate(country = ifelse(app_user_id %in% not_kenya_ids, "Not Kenya", 
                                                       ifelse(app_user_id %in% kenya_ids, "Kenya",
                                                              "Not on Matomo")))

x %>% group_by(country) %>% summarise(n())

#writexl::write_xlsx(x = x, path = "EFM_uuid_country.xlsx")

# what users are not in kenya? - we can exclude them.
# but just because they are in kenya, doesn't mean we exclude everyone else. So we do not know
# how to treat those not in the matomo data.

####################### LOOKING AT THE MATOMO DATA #############################

# Of those who do not have information connected to matomo, 6/7 are web-users
plhdata_org %>% filter(country == "Not on Matomo") %>% View()

# Number of app launches on metabase vs matomo
View(efm_kenya)
# In efm_kenya and efm_not_kenya we have "Visits" - this is the nujmber of visits to the app
# In metabase we have "field.app-launch-count" - this is the number of app launches to the app
matomo_data <- dplyr::bind_rows(efm_kenya, efm_not_kenya)

plhdata_org <- dplyr::full_join(plhdata_org, matomo_data, by = c("app_user_id" = "UUID"))

plhdata_org$app_launch_count <- as.numeric(plhdata_org$app_launch_count)
plhdata_org <- plhdata_org %>%
  mutate(app_launch_count_checker = ifelse(app_launch_count > Visits, "1",
                                           ifelse(app_launch_count == Visits, "2",
                                                  ifelse(app_launch_count < Visits, "3",
                                                         "4"))))

plhdata_org %>% group_by(app_launch_count_checker) %>% summarise(n())

plhdata_org %>% 
  dplyr::select(c(app_launch_count_checker, app_user_id, app_launch_count, Visits, country)) %>%
  View()
#
# filter to "short strings"?

# Checking if people are web or app users in Kenya / by country
# plhdata_org <- plhdata_org %>%
#   mutate(user_type = ifelse(str_count(app_user_id) == 16, "app",
#                             ifelse(str_count(app_user_id) == 36, "web",
#                                    "unknown")))
# plhdata_org %>% group_by(user_type, country) %>% summarise(n())
