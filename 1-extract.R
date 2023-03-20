# 0 - Extract

require(tinytex)
require(tidyverse)

# test <- tibble::tibble(
#   x = round(rnorm(500, mean = 3, sd = 2), digits = 0),
#   y = rnorm(10)
# )

data <- readxl::read_xlsx("data/data.xlsx")
