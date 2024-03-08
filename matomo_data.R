#' Visitors > User IDs > date range > import
#' 
#' UUID? Check.
#' Visits: If a vistor comes for the first time, or if they visit a page more than 30 minutes after their last page view, this is a new visit
#' Actions: The number of actions performed (page views, site searches, downloads, outlinks)
#' Actions per visit: average number of actions per visits
#' avg time on website: Average duration of a visit (seconds)
#' Bounces: number of visits that only had a single page view (so the visitor left the website directly from the entrance page)
#' Bounce proportion: proportion of visits that only had a single page view (so the visitor left the website directly from the entrance page)
#' Time on Website: total time spent by visitor (in seconds) over period. - note that I calculated this so it can be out by about 10 seconds.


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

efm_not_kenya <- calling_matomo_data(type = "EFM_not_KE")
efm_kenya <- calling_matomo_data(type = "EFM_KE")

not_kenya_ids <- efm_not_kenya$UUID
kenya_ids <- efm_kenya$UUID

x <- plhdata_org %>% mutate(country = ifelse(app_user_id %in% not_kenya_ids, "Not Kenya", 
                                             ifelse(app_user_id %in% kenya_ids, "Kenya",
                                                    "Not on Matomo"))) %>%
  dplyr::select(country, app_user_id)

x %>% group_by(country) %>% summarise(n())

#writexl::write_xlsx(x = x, path = "EFM_uuid_country.xlsx")

# what users are not in kenya? - we can exclude them.
# but just because they are in kenya, doesn't mean we exclude everyone else. So we do not know
# how to treat those not in the matomo data.