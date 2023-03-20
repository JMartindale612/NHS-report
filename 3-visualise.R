# 3 - Visualise ----

source("2-score.R")

data <- tibble(
  dimension = c("ability_total", "ability_ver", "ability_num", "ability_log"),
  score = c(54, 87, 23, 50)
)

data %>%
  ggplot(aes(x = dimension,
             y = score)) +
  # Column group
  geom_col() +
  # Flip axes so scores are on the X axes
  coord_flip() +
  # Add labels
  geom_text(aes(label = score), vjust = 0.5, colour = "yellow")