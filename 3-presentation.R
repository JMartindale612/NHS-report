# 3 - Visualise ----

source("2-score.R")


## Plotting ----

data_plot <- scores10 %>%
  filter(ID == "participant1") %>%
  select(Psychopathy:`Vulnerable Narcissism`) %>%
    pivot_longer(everything(), 
                 names_to = c("test"), 
                 values_to = "score") %>%
  mutate(
    test = factor(
      test,
      levels = c("Psychopathy", "Narcissism", "Machiavellianism", "Grandiose Narcissism", "Vulnerable Narcissism")
    )
  )

data_plot

p <- ggplot(data_plot,
            aes(
              x = test,
              y = score
            ))

p + 
  # Bar and text values
  geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
  geom_text(aes(label = round(score, 1)), 
            hjust = 1.2, vjust = 0.3, 
            color = "white") +
  # Axes
  coord_flip() +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),
    limits = c(0, 100),
    expand = c(0, 0)) + 
  # Labels
  labs(x = "", y = "Percentile Score", title = "Psychometric Score") +
  theme_minimal() +
  # Aesthetics
  scale_fill_manual(values = c("Psychopathy" = "#003087",
                               "Narcissism" = "#005EB8",
                               "Machiavellianism" = "#99C7EB",
                               "Grandiose Narcissism" = "#919EA8",
                               "Vulnerable Narcissism" = "#DDE1E4"))

