library(glmnet)
library(Matrix)
data <- read.csv("D:/TCD/Study/Dissertation/mine/Data/netflix_user_data.csv", header=TRUE, sep= ",") 

#reclassify "state" and create "region"
group_map <- list(
  A = c("Uttar Pradesh", "Madhya Pradesh", "Rajasthan", "Haryana", "Punjab", "Himachal Pradesh", "Uttarakhand"),
  B = c("Bihar", "Jharkhand", "West Bengal", "Odisha", "Assam", "Sikkim", "Tripura"),
  C = c("Arunachal Pradesh", "Manipur", "Mizoram", "Nagaland", "Meghalaya", "Chhattisgarh", "Goa"),
  D = c("Maharashtra", "Gujarat", "Karnataka", "Tamil Nadu", "Kerala", "Telangana", "Andhra Pradesh")
)
get_region <- function(state_name) {
  for (region_code in names(group_map)) {
    if (state_name %in% group_map[[region_code]]) return(region_code)
  }
  return(NA)
}
data$region <- sapply(data$state, get_region)

#create buffer_vs_view
data$buffer_vs_view <- as.numeric(as.numeric(data$buffering_time) / as.numeric(data$view_duration))


x_numeric <- c(
  "content_completion_rate", "rating_given", 
  "view_duration", "buffering_time"
)

x_factors <- c(
  "plan_type", "viewing_device", "content_genre",
  "age_group", "gender",
  "content_language", "network_type", "region"
)

get_summary_stats <- function(x) {
  stats <- data.frame(
    Median = median(x, na.rm = TRUE),
    Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE)
  )
  print(stats)
}

for (i in x_numeric){
  cat('variable name:', i, '\n')
  get_summary_stats(data[[i]])
}

get_stats <- function(x) {
  stats <- data.frame(
    table(x)
  )
  print(stats)
}

for (i in x_factors){
  cat('variable name:', i, '\n')
  get_stats(data[[i]])
}