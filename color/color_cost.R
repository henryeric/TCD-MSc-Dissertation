library(ggplot2)
library(cowplot)
library(tidyr)

# data
set.seed(123)
data <- data.frame(
  variable = rep(c("gender", "content_language", "content_genre", "age", "promotions", "watching_with_others"), each = 2),
  category = c("male", "female",
         "Hindi", "English/Japanese", 
         "drama", "action/horror",
         "young", "old",
         "more", "less",
         "loneliness", "bustle"
         ),
  tendency = c(2.1, 6.7,   
          3.7, 6.4,   
          3.1, 6.2,   
          2.5, 7,   
          4.4, 5.6,   
          3.8, 6.3)   
)

#
color_bar <- ggplot() +
  geom_tile(data = data.frame(x = 1:100, y = 1),
            aes(x = x, y = y, fill = x), 
            show.legend = FALSE) +
  scale_fill_gradientn(colors = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
                       limits = c(1, 100)) +
  scale_x_continuous(breaks = c(1, 100),
                     labels = c("cheap", "expensive")) +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    text = element_text(size = 12),
    plot.margin = margin(t = 5, b = 5)
  )

# 
main_plot <- ggplot(data, aes(x = tendency, y = reorder(variable, -tendency), 
                              fill = tendency, color = after_scale(fill))) +
  geom_segment(aes(x = 2.5, xend = 7.5, yend = reorder(variable, -tendency)), 
               color = "gray90", linewidth = 6) +
  geom_point(shape = 21, size = 8, stroke = 1.5) +
  geom_text(aes(label = category), color = "black", fontface = "bold", size = 4) +
  scale_fill_gradientn(colors = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
                       limits = c(1, 8)) +
  scale_x_continuous(limits = c(1, 8), 
                     breaks = c(1, 7),
                     labels = c("cheap tendency", "expensive tendency")) +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_line(color = "grey95"),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text = element_text(size = 12, color = "black"),
    axis.text.x = element_text(size = 10, margin = margin(t = 5)),
    plot.margin = margin(t = 5, b = 15, l = 10, r = 10)
  )

#
title <- ggdraw() + 
  draw_label("The tendency of variable categories towards subscription service fees", 
             fontface = 'bold', size = 16, x = 0, hjust = 0.15) +
  theme(plot.margin = margin(t = 0, b = 10, l = 10))

# 
final_plot <- plot_grid(
  title,
  color_bar,
  main_plot,
  ncol = 1,
  rel_heights = c(0.07, 0.2, 0.73), 
  align = "v"
)

# 
print(final_plot)

