library(glmnet)
library(Matrix)
data <- read.csv("D:/TCD/Study/Dissertation/mine/Data/sample_data.csv", header=TRUE, sep= ",") 

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

#x&y

y <- as.numeric(data$content_completion_rate)
x_numeric <- scale(data[, c(
  "rating_given", "view_duration", "buffering_time",
  "search_queries_per_month", "ads_watched", 
  "customer_support_interactions", "number_of_profiles"
)])
x_numeric_df <- as.data.frame(x_numeric)
x_factors_df <- data[, c(
  "plan_type", "viewing_device", "network_type",
  "content_genre", "content_type",
  "age_group", "gender", "time_of_day", "watching_with_others",
  "content_language", "user_feedback", "binge_watching",
  "download_quality", "account_type", "promotions_used", "region"
)]
x_factors_df[] <- lapply(x_factors_df, as.factor)
x_df <- cbind(x_numeric_df, x_factors_df)

x <- as.matrix(x_df)


###
library(e1071)
skewness(y)  
kurtosis(y)

qqnorm(y)
qqline(y, col = "red")
###


#Lasso
cv_model <- cv.glmnet(x, y, alpha = 1, family = "gaussian")
plot(cv_model)

#coefs
library(dplyr)
coefs <- coef(cv_model, s = "lambda.min")
coef_df <- as.data.frame(as.matrix(coefs))
coef_df$variable <- rownames(coef_df)
colnames(coef_df)[1] <- "coefficient"
important_vars <- coef_df %>%
  filter(coefficient != 0 & variable != "(Intercept)") %>%
  arrange(desc(abs(coefficient)))
print(important_vars)

#plot
library(ggplot2)
ggplot(important_vars, aes(x = reorder(variable, abs(coefficient)), y = coefficient)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "LASSO: Non-zero Coefficients",
       x = "Variable",
       y = "Coefficient") +
  theme_minimal()