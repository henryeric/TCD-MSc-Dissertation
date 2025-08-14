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

#x&y

y <- as.factor(data$viewing_device)
x_numeric <- scale(data[, c(
  "view_duration", "buffering_time", "content_completion_rate", 
  "search_queries_per_month", "rating_given", "ads_watched", 
  "customer_support_interactions", "number_of_profiles"
)])
x_numeric_df <- as.data.frame(x_numeric)
x_factors_df <- data[, c(
  "plan_type", "content_genre", "content_type",
  "age_group", "gender", "time_of_day", "watching_with_others",
  "content_language", "user_feedback", "binge_watching",
  "download_quality", "network_type", "account_type", "promotions_used", "region"
)]
x_factors_df[] <- lapply(x_factors_df, as.factor)
x_df <- cbind(x_numeric_df, x_factors_df)

#vif
library(car)
v_model_formula <- as.formula(paste("as.numeric(y) ~", paste(colnames(x_df), collapse = " + ")))
v_model <- lm(v_model_formula, data = x_df)
vif_values <- vif(v_model)
print(vif_values)

#stepwise AIC
library(MASS)
library(nnet)
full_model <- multinom(y ~ ., data = data.frame(y = y, x_df))
step_model <- stepAIC(full_model, direction = "both", trace = TRUE)
summary(step_model)

z <- summary(step_model)$coefficients / summary(step_model)$standard.errors
p <- 2 * (1 - pnorm(abs(z)))
print(p)

null_model <- multinom(y ~ 1, data = data.frame(y = y, x_df), trace = FALSE)
AIC(null_model, step_model)#diff_AIC>10 -> ok!

#simplify the model
step_model2 <- multinom(y ~ search_queries_per_month + 
                          download_quality, 
                        data = data.frame(y = y, x_df))
summary(step_model2)

z1 <- summary(step_model2)$coefficients / summary(step_model2)$standard.errors
p1 <- 2 * (1 - pnorm(abs(z1)))
print(p1)

AIC(step_model2, full_model)

#Coefficients and CI of Variables in Multinomial Logistic Regression
library(ggplot2)
library(broom)
coefs_df <- tidy(step_model2, conf.int = TRUE)
coefs_df <- subset(coefs_df, term != "(Intercept)")
ggplot(coefs_df, aes(x = term, y = estimate, color = y.level)) +
  geom_point(position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, position = position_dodge(width = 0.7)) +
  coord_flip() +
  labs(title = "Coefficients and CI of Variables in Multinomial Logistic Regression",
       y = "Estimated coefficient ± 95% CI", x = "Variables") +
  theme_minimal()


