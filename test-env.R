library(tinytex)
library(tidyverse)

# Run the below if tinytex is not installed on local system
# tinytex::install_tinytex()


if (!require("pacman")) install.packages("pacman")
p_load(ggplot2, ggthemes, dplyr, readr, scales, forcats)

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

# Data cleaning ----

bigdata <- readxl::read_xlsx("data/data.xlsx")

bigdata_long <- bigdata %>%
  pivot_longer(
    cols = numeric1:fdt_warm,
    names_to = "item",
    values_to = "score"
  )

bigdata %>%
  pivot_longer(
    cols = numeric1:fdt_warm,
    names_to = "item",
    values_to = "score"
  ) %>%
  filter(ID == "participant1", 
         item  %in% c("numeric1","numeric2","numeric3",
                      "numeric4","numeric5","numeric6")) %>%
  summarise(
    numeric_score = mean(score, na.rm = TRUE)
  )

numeric <- function(x){
  bigdata %>%
    # Convert data to long format
    # Add select in here?
    pivot_longer(
      cols = numeric1:fdt_warm,
      names_to = "item",
      values_to = "score"
    ) %>%
    # Filter by participant ID and only "numeric" items
    filter(ID == x, 
           item  %in% c("numeric1","numeric2","numeric3",
                        "numeric4","numeric5","numeric6")) %>%
    summarise(
      numeric_score = mean(score, na.rm = TRUE)
)
}

numeric("participant1")

?pivot_longer







test <- function(x, y){
  bigdata %>%
    # Convert data to long format
    # Add select in here?
    pivot_longer(
      cols = select(contains(y)),
      names_to = "item",
      values_to = "score"
    ) %>%
    # Filter by participant ID and only "numeric" items
    filter(ID == y) %>%
    summarise(
      numeric_score = mean(score, na.rm = TRUE)
    )
}


bigdata %>%
  # Select participant ID and sub-test columns
  select(ID, contains("numeric")) %>%
  # Pivot to long data set
  pivot_longer(
    cols = contains("numeric"),
    names_to = "item",
    values_to = "score"
  ) %>%
  # Filter by participant
  filter(ID == "participant1") %>%
  summarise(
    numeric_score = mean(score, na.rm = TRUE))

func <- function(x, y){
  bigdata %>%
    # Select participant ID and sub-test columns
    select(ID, contains(x)) %>%
    # Pivot to long data set
    pivot_longer(
      cols = contains(x),
      names_to = "item",
      values_to = "score"
    ) %>%
    # Filter by participant
    filter(ID == y) %>%
    summarise(
      numeric_score = mean(score, na.rm = TRUE))
}

func("numeric", "participant1")


x <- 2
