# 3 - Visualise ----

## Descriptives ----

vars <- scores %>% select(cog_num_mean:cog_tot_mean) %>% names()

descriptives <- tibble(
  vars = vars,
  mean = numeric(length(vars)),
  sd = numeric(length(vars))
)

for (i in seq_along(vars)) {
  descriptives$mean[i] <- round(mean(scores[[vars[i]]], na.rm=TRUE), digits = 2)
  descriptives$sd[i] <- round(sd(scores[[vars[i]]], na.rm=TRUE), digits = 2)
}

print(descriptives)



source("2-score.R")

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