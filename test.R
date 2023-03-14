library(kernlab)
library(pacman)

install.packages(c(
  "gapminder", "ggforce", "gh", "globals", "openintro", "profvis",
  "RSQLite", "shiny", "shinycssloaders", "shinyFeedback",
  "shinythemes", "testthat", "thematic", "tidyverse", "vroom",
  "waiter", "xml2", "zeallot"
))

# data("spam")

chilean_exports <- "year,product,export,percentage
2006,copper,4335009500,81
2006,others,1016726518,19
2007,copper,9005361914,86
2007,others,1523085299,14
2008,copper,6907056354,80
2008,others,1762684216,20
2009,copper,10529811075,81
2009,others,2464094241,19
2010,copper,14828284450,85
2010,others,2543015596,15
2011,copper,15291679086,82
2011,others,3447972354,18
2012,copper,14630686732,80
2012,others,3583968218,20
2013,copper,15244038840,79
2013,others,4051281128,21
2014,copper,14703374241,78
2014,others,4251484600,22
2015,copper,13155922363,78
2015,others,3667286912,22
"

exports_data <- read_csv(chilean_exports)
exports_data
# A tibble: 20 × 4
# year product      export percentage
# <dbl> <chr>         <dbl>      <dbl>
#   1  2006 copper   4335009500         81
# 2  2006 others   1016726518         19

data <- tibble(
  dimension = c("ability_total", "ability_ver", "ability_num", "ability_log"),
  score = c(54, 87, 23, 50)
)

data <- data %>%
  mutate(dimension = factor(dimension, 
                            levels = c("ability_total",
                                       "ability_ver",
                                       "ability_num",
                                       "ability_log"),
                            labels = c("Total score",
                                       "Numerical score",
                                       "Verbal score",
                                       "Logical score")))

fill <- c("#E1B378")

plot <- ggplot(aes(y = score, x = dimension), data = data) +
  # Base is a simple column chart
  geom_col() + 
  # Add text labels of score to each bar
  geom_text(aes(label = score), 
            position = position_stack(vjust = 0.5), # Position them on each bar
            size = 3,
            colour = "white") + 
  # Flip axes
  coord_flip() + 
  # Extend scale of X axis
  scale_y_continuous(breaks = seq(0, 100, 10)) + 
  # Add labels
  labs(title = "Cognitive ability test scores", 
       subtitle = "Percentile scores compared to N=1,100 panel data sample") +
  labs(x = "Test dimension", y = "Percentile scores") +
  # Change colours
  theme_bw()

plot




