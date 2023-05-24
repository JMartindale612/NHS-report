library(tidyverse)
library(grid)

# Create the dataset
data <- tibble(
  dimension = factor(c("ability_total", "ability_ver", "ability_num", "ability_log"),
                     levels = c("ability_total", "ability_ver", "ability_num", "ability_log"),
                     labels = c("Total", "Verbal", "Numerical", "Logical")),
  score = c(54, 87, 23, 50)
)

# Create the plot
top_labels <- data.frame(
  x = seq(5, 95, 10),
  y = rep("Total", 10),
  label = 1:10
)

ggplot(data, aes(x = score, y = dimension)) +
  geom_col(fill = "darkblue") +
  labs(x = "Score", y = "Cognitive ability dimensions") +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  geom_vline(xintercept = seq(0, 100, 10), linetype = "dashed", color = "grey") +
  geom_text(data = top_labels, aes(x = x, y = y, label = label), vjust = -0.5) +
  theme_minimal() +
  scale_y_discrete(limits=rev)