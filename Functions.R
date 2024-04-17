# Function to call matomo data
calling_matomo_data <- function(date_from = "2023-10-25", date_to = "2024-10-25", data = plhdata_org,
                                type = c("none", "EFM_KE", "EFM_not_KE"),
                                token = token_matomo){
  type <- match.arg(type)
  
  if (type == "EFM_KE"){
    segment_name <- "segment=countryCode%3D%3Dke;pageTitle%3D%3DEarly%252520Family%252520Math"
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


# Functions
# function to fix up namings to make it a bit prettier!
naming_conventions <- function(x, replace, replace_after) {
  if (!missing(replace)){
    x <- gsub(paste("^.*?", replace, ".*", sep = ""), "", x)
  }
  if (!missing(replace_after)){
    x <- gsub(paste(replace_after, "$", sep = ""), "", x)
  }
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x <- gsub("_", " ", x)
  x
}

# so we can use "add_na_variable" to add into the data variables which are not in the data
# but will be at some point
# this function checks if the variable is in the data.
# If it is not in the data, then it adds it as a new variable with all NAs.
add_na_variable <- function(data = contacts_unflat, variable){
  for (names in variable) {
    if (!names %in% colnames(data)) {
      data[, names] <- NA
      warning(paste(names, "does not exist. Adding NAs"))
    }
  }
  return(data)
}

# Function to count dates in each element of the vector
count_dates <- function(x) {
  if (is.na(x)) {
    return(0)
  } else {
    dates <- unlist(strsplit(x, ";"))
    return(length(dates))
  }
}