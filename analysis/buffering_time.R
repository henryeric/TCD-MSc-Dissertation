library(glmnet)
library(Matrix)
data <- read.csv("D:/TCD/Study/Dissertation/mine/Data/proper_data.csv", header=TRUE, sep= ",") 

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

#x&y

y <- as.numeric(data$buffer_vs_view)
y_log <- log(y + 0.001)
x_numeric <- scale(data[, c(
  "content_completion_rate", "rating_given", 
  "search_queries_per_month", "ads_watched", 
  "customer_support_interactions", "number_of_profiles"
)])
x_numeric_df <- as.data.frame(x_numeric)
x_factors_df <- data[, c(
  "plan_type", "viewing_device", "content_genre", "content_type",
  "age_group", "gender", "time_of_day", "watching_with_others",
  "content_language", "user_feedback", "binge_watching",
  "download_quality", "network_type", "account_type", "promotions_used", "region"
)]
x_factors_df[] <- lapply(x_factors_df, as.factor)
x_df <- cbind(x_numeric_df, x_factors_df)

x <- as.matrix(x_df)


###
library(e1071)
skewness(y_log)  
kurtosis(y_log)

qqnorm(y_log)
qqline(y_log, col = "red")
hist(y_log, breaks = 50, main = "Histogram of log(buffer_vs_view+0.001)", xlab = "log(buffer_vs_view+0.001)")

###

#stepwise
library(MASS)
library(nnet)
full_model <- lm(y ~ ., data = data.frame(y = y_log, x_df))
step_model <- stepAIC(full_model, direction = "both", trace = TRUE)
summary(step_model)

###
plot(step_model$fitted.values, resid(step_model), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

qqnorm(resid(step_model))
qqline(resid(step_model), col = "blue")

shapiro.test(resid(step_model))

library(lmtest)
bptest(step_model)

y_log_hat <- predict(step_model)
y_hat <- exp(y_log_hat) - 0.001
r2 <- cor(y, y_hat)^2
rmse <- sqrt(mean((y - y_hat)^2))

cat("R-squared (on original y):", r2, "\n")
cat("RMSE (on original y):", rmse, "\n")

#simplify
step_model2 <- lm(y ~ gender + time_of_day + region,
                   data = data.frame(y = y_log, x_df))
summary(step_model2)

##plot
library(ggplot2)
library(broom)
coefs_df <- tidy(step_model2, conf.int = TRUE)
coefs_df <- subset(coefs_df, term != "(Intercept)")

ggplot(coefs_df, aes(x = term, y = estimate)) +
  geom_point(color = "#2c7bb6", size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, color = "#2c7bb6") +
  coord_flip() +
  labs(
    title = "Coefficients and 95% CI in Linear Regression (log-transformed buffering time)",
    y = "Estimated coefficient ± 95% CI",
    x = "Variables"
  ) +
  theme_minimal()


