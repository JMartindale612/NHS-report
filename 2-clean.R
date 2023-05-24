
# 2 - Clean ----

source("1-extract.R")

scores <- data %>%
    # Calculate mean scores for each cognitive ability sub-test
    mutate(
      cog_num_mean = rowMeans(select(., starts_with("cog_num")), na.rm=TRUE),
      cog_ver_mean = rowMeans(select(., starts_with("cog_ver")), na.rm=TRUE), 
      cog_abs_mean = rowMeans(select(., starts_with("cog_abs")), na.rm=TRUE)) %>%
    # Calculate overall score for cognitive ability
    mutate(
      cog_tot_mean = rowMeans(select(., contains("mean")), na.rm = TRUE)
    ) %>%
    mutate(
      cog_num_zscore = ((cog_num_mean - mean(cog_num_mean)) / sd(cog_num_mean)),
      cog_ver_zscore = ((cog_ver_mean - mean(cog_ver_mean)) / sd(cog_ver_mean)),
      cog_abs_zscore = ((cog_abs_mean - mean(cog_abs_mean)) / sd(cog_abs_mean)),
      cog_tot_zscore = ((cog_tot_mean - mean(cog_tot_mean)) / sd(cog_tot_mean))
    ) %>%
    mutate(
      cog_num_percentile = pnorm(cog_num_zscore) * 100,
      cog_ver_percentile = pnorm(cog_ver_zscore) * 100,
      cog_abs_percentile = pnorm(cog_abs_zscore) * 100,
      cog_tot_percentile = pnorm(cog_tot_zscore) * 100,
      cog_num_sten = round(5.5 + 2 * cog_num_zscore),
      cog_ver_sten = round(5.5 + 2 * cog_ver_zscore),
      cog_abs_sten = round(5.5 + 2 * cog_abs_zscore),
      cog_tot_sten = round(5.5 + 2 * cog_tot_zscore)
    ) %>%
    select(ID, cog_num_mean:cog_tot_sten) %>%
    print(with = Inf)

# Descriptives ----

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
