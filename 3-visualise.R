# 3 - Visualise ----

source("2-clean.R")

test <- scores %>%
  filter(ID == "participant1") %>%
  pivot_longer(cols = -ID, names_to = c("test"), values_to = "score")


data %>%
  ggplot(aes(x = dimension,
             y = score)) +
  # Column group
  geom_col() +
  # Flip axes so scores are on the X axes
  coord_flip() +
  # Add labels
  geom_text(aes(label = score), vjust = 0.5, colour = "yellow")