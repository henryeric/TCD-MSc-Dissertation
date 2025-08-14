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

#create sub_duration
data$sub_duration <- as.numeric(as.Date(data$subscription_end_date) - as.Date(data$subscription_start_date))

#create buffer_vs_view
data$buffer_vs_view <- as.numeric(as.numeric(data$buffering_time) / as.numeric(data$view_duration))


#plot
library(ggplot2)

y <- c("gender", "content_language", "content_genre", "age_group")


#device
x_device <- data$viewing_device

for (i in y) {
  df_plot <- data.frame(
    v = x_device,
    variable = data[[i]]
  )
  freq_df <- df_plot %>%
    group_by(v, variable) %>%
    summarise(n = n(), .groups = "drop")
  min_count <- min(freq_df$n[freq_df$n > 0], na.rm = TRUE)
  max_count <- max(freq_df$n, na.rm = TRUE)
  y_min <- min_count * 0.95
  y_max <- max_count * 1.05
  p <- ggplot(freq_df, aes(x = v, y = n, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      x = "Categories of Viewing Devices",
      y = "Frequencies",
      fill = i,
      title = paste("Distribution of", i, "across different devices")
    ) +
    coord_cartesian(ylim = c(y_min, y_max)) + 
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 30, hjust = 1)
    )
  print(p)
}


#network
x_network <- data$network_type

for (i in y) {
  df_plot <- data.frame(
    v = x_network,
    variable = data[[i]]
  )
  freq_df <- df_plot %>%
    group_by(v, variable) %>%
    summarise(n = n(), .groups = "drop")
  min_count <- min(freq_df$n[freq_df$n > 0], na.rm = TRUE)
  max_count <- max(freq_df$n, na.rm = TRUE)
  y_min <- min_count * 0.95
  y_max <- max_count * 1.05
  p <- ggplot(freq_df, aes(x = v, y = n, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      x = "Categories of Network_type",
      y = "Frequencies",
      fill = i,
      title = paste("Distribution of", i, "across different network")
    ) +
    coord_cartesian(ylim = c(y_min, y_max)) + 
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 30, hjust = 1)
    )
  print(p)
}



#subscription_cost
x_plan <- data$plan_type

for (i in y) {
  df_plot <- data.frame(
    v = x_plan,
    variable = data[[i]]
  )
  freq_df <- df_plot %>%
    group_by(v, variable) %>%
    summarise(n = n(), .groups = "drop")
  min_count <- min(freq_df$n[freq_df$n > 0], na.rm = TRUE)
  max_count <- max(freq_df$n, na.rm = TRUE)
  y_min <- min_count * 0.95
  y_max <- max_count * 1.05
  p <- ggplot(freq_df, aes(x = v, y = n, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      x = "Categories of Plan_type/substription_cost",
      y = "Frequencies",
      fill = i,
      title = paste("Distribution of", i, "across different plan_type")
    ) +
    coord_cartesian(ylim = c(y_min, y_max)) + 
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 30, hjust = 1)
    )
  print(p)
}


#region
x_region <- data$region

for (i in y) {
  df_plot <- data.frame(
    v = x_region,
    variable = data[[i]]
  )
  freq_df <- df_plot %>%
    group_by(v, variable) %>%
    summarise(n = n(), .groups = "drop")
  min_count <- min(freq_df$n[freq_df$n > 0], na.rm = TRUE)
  max_count <- max(freq_df$n, na.rm = TRUE)
  y_min <- min_count * 0.95
  y_max <- max_count * 1.05
  p <- ggplot(freq_df, aes(x = v, y = n, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      x = "Categories of Region",
      y = "Frequencies",
      fill = i,
      title = paste("Distribution of", i, "across different region")
    ) +
    coord_cartesian(ylim = c(y_min, y_max)) + 
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 30, hjust = 1)
    )
  print(p)
}










#densityplot
find_valleys <- function(x, y) {
  n <- length(y)
  valleys <- logical(n)
  for (i in 2:(n-1)) {
    valleys[i] <- (y[i] < y[i-1]) & (y[i] < y[i+1])
  }
  y[valleys]
}

#subscription_duration
x_subduration <- as.numeric(data$sub_duration)

for (i in y) {
  df_plot <- data.frame(
    category = data[[i]],
    value = x_subduration    ###about x, only adjust here
  )
  dens_info <- df_plot %>%
    group_by(category) %>%
    summarise(
      dens = list(density(value, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      peak_y = max(dens$y),
      peak_x = dens$x[which.max(dens$y)],
      valleys = list(find_valleys(dens$x, dens$y)),
      min_valley = ifelse(length(valleys) > 0, min(valleys), NA_real_)
    ) %>%
    ungroup()
  y_min <- min(dens_info$min_valley, na.rm = TRUE) * 0.95
  y_max <- max(dens_info$peak_y) * 1.05
  p <- ggplot(df_plot, aes(x = value, fill = category)) +
    geom_density(alpha = 0.5) +
    geom_text(
      data = dens_info,
      aes(x = peak_x, y = peak_y, label = paste0("Peak: ", round(peak_x, 1))),
      inherit.aes = FALSE,
      vjust = -0.5,
      size = 3,
      color = "black"
    ) +
    labs(
      x = "subscription_duration", y = "Density", fill = i,
      title = paste("Density distribution of subscription_duration by", i)
    ) +
    coord_cartesian(ylim = c(y_min, y_max)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 30, hjust = 1))
  print(p)
}




#rating_given
x_rating <- as.numeric(data$rating_given)

for (i in y) {
  df_plot <- data.frame(
    category = data[[i]],
    value = x_rating    ###about x, only adjust here
  )
  dens_info <- df_plot %>%
    group_by(category) %>%
    summarise(
      dens = list(density(value, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      peak_y = max(dens$y),
      peak_x = dens$x[which.max(dens$y)],
      valleys = list(find_valleys(dens$x, dens$y)),
      min_valley = ifelse(length(valleys) > 0, min(valleys), NA_real_)
    ) %>%
    ungroup()
  y_min <- min(dens_info$min_valley, na.rm = TRUE) * 0.95
  y_max <- max(dens_info$peak_y) * 1.05
  p <- ggplot(df_plot, aes(x = value, fill = category)) +
    geom_density(alpha = 0.5) +
    geom_text(
      data = dens_info,
      aes(x = peak_x, y = peak_y, label = paste0("Peak: ", round(peak_x, 1))),
      inherit.aes = FALSE,
      vjust = -0.5,
      size = 3,
      color = "black"
    ) +
    labs(
      x = "Rating_given", y = "Density", fill = i,
      title = paste("Density distribution of rating-given by", i)
    ) +
    coord_cartesian(ylim = c(y_min, y_max)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 30, hjust = 1))
  print(p)
}




#content_completion_rate
x_completion <- as.numeric(data$content_completion_rate)

for (i in y) {
  df_plot <- data.frame(
    category = data[[i]],
    value = x_completion    ###about x, only adjust here
  )
  dens_info <- df_plot %>%
    group_by(category) %>%
    summarise(
      dens = list(density(value, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      peak_y = max(dens$y),
      peak_x = dens$x[which.max(dens$y)],
      valleys = list(find_valleys(dens$x, dens$y)),
      min_valley = ifelse(length(valleys) > 0, min(valleys), NA_real_)
    ) %>%
    ungroup()
  y_min <- min(dens_info$min_valley, na.rm = TRUE) * 0.95
  y_max <- max(dens_info$peak_y) * 1.05
  p <- ggplot(df_plot, aes(x = value, fill = category)) +
    geom_density(alpha = 0.5) +
    geom_text(
      data = dens_info,
      aes(x = peak_x, y = peak_y, label = paste0("Peak: ", round(peak_x, 1))),
      inherit.aes = FALSE,
      vjust = -0.5,
      size = 3,
      color = "black"
    ) +
    labs(
      x = "Content_completion_rate", y = "Density", fill = i,
      title = paste("Density distribution of content_completion_rate by", i)
    ) +
    coord_cartesian(ylim = c(y_min, y_max)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 30, hjust = 1))
  print(p)
}



#buffer_vs_view
x_buffer_vs_view <- log(as.numeric(data$buffer_vs_view) + 0.001)

for (i in y) {
  df_plot <- data.frame(
    category = data[[i]],
    value = x_buffer_vs_view    ###about x, only adjust here
  )
  dens_info <- df_plot %>%
    group_by(category) %>%
    summarise(
      dens = list(density(value, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      peak_y = max(dens$y),
      peak_x = dens$x[which.max(dens$y)],
      valleys = list(find_valleys(dens$x, dens$y)),
      min_valley = ifelse(length(valleys) > 0, min(valleys), NA_real_)
    ) %>%
    ungroup()
  y_min <- min(dens_info$min_valley, na.rm = TRUE) * 0.95
  y_max <- max(dens_info$peak_y) * 1.05
  p <- ggplot(df_plot, aes(x = value, fill = category)) +
    geom_density(alpha = 0.5) +
    geom_text(
      data = dens_info,
      aes(x = peak_x, y = peak_y, label = paste0("Peak: ", round(peak_x, 1))),
      inherit.aes = FALSE,
      vjust = -0.5,
      size = 3,
      color = "black"
    ) +
    labs(
      x = "Buffer_vs_view(log(+0.001))", y = "Density", fill = i,
      title = paste("Density distribution of buffer_vs_view(log(+0.001)) by", i)
    ) +
    coord_cartesian(ylim = c(y_min, y_max)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 30, hjust = 1))
  print(p)
}
