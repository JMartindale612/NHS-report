
# 2 - Clean ----

source("1-extract.R")

## Variables ----

# Define personality scores
personality_vars <- list(
  psychopathy = c("fdt_anger", "fdt_modest", "fdt_tender",
                  "fdt_warm", "fdt_dutif", "fdt_trust",
                  "fdt_assert", "fdt_self-dis", "fdt_vulner",
                  "fdt_straight", "fdt_compl", "fdt_delib",
                  "fdt_self-ass", "fdt_altr", "fdt_depr",
                  "fdt_excite", "fdt_anxie", "fdt_impul"),
  narcissism = c("fdt_achiev", "fdt_modest", "fdt_assert",
                 "fdt_trust", "fdt_entit", "fdt_gregar",
                 "fdt_altr", "fdt_fant", "fdt_indiff",
                 "fdt_tender", "fdt_straight", "fdt_vulner_rev",
                 "fdt_anger", "fdt_shame", "fdt_excite"),
  machiavelli = c("fdt_achiev", "fdt_activ", "fdt_altr",
                 "fdt_assert", "fdt_compet", "fdt_delib_rev",
                 "fdt_vulner", "fdt_modest", "fdt_order",
                 "fdt_self-ass","fdt_straight",	"fdt_tender",
                 "fdt_trust"),
  grand_narc = c("fdt_achiev", "fdt_modest", "fdt_assert",	
                 "fdt_entit", "fdt_gregar", "fdt_altr",
                 "fdt_fant", "fdt_indiff", "fdt_tender",	
                 "fdt_straight", "fdt_excite"),
  vuln_narc = c("fdt_trust", "fdt_vulner_rev", "fdt_anger", "fdt_shame")										
)

data$fdt_vulner_rev <- 6 - data$fdt_vulner # (High) Vulnerability for Vulnerable Narcissism
data$fdt_delib_rev <- 6 - data$fdt_delib # (High) Deliberation for Machiavellianism

## Scoring ----

# There is repetition in the below code to make it clearer for readers
# and because the "mean() / sd()" calculations will be replaced with 
# explicit numbers from the norm group

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
    mutate(
      psych_mean = rowMeans(select(., all_of(personality_vars$psychopathy)), na.rm=TRUE),
      narc_mean = rowMeans(select(., all_of(personality_vars$narcissism)), na.rm=TRUE),
      mach_mean = rowMeans(select(., all_of(personality_vars$machiavelli)), na.rm=TRUE),
      gnarc_mean = rowMeans(select(., all_of(personality_vars$vuln_narc)), na.rm=TRUE),
      vnarc_mean = rowMeans(select(., all_of(personality_vars$grand_narc)), na.rm=TRUE)
    ) %>%
    mutate(
      psych_zscore = ((psych_mean - mean(psych_mean)) / sd(psych_mean)),
      narc_zscore = ((narc_mean - mean(narc_mean)) / sd(narc_mean)),
      mach_zscore = ((mach_mean - mean(mach_mean)) / sd(mach_mean)),
      gnarc_zscore = ((gnarc_mean - mean(gnarc_mean)) / sd(gnarc_mean)),
      vnarc_zscore = ((vnarc_mean - mean(vnarc_mean)) / sd(vnarc_mean))
    ) %>%
    mutate(
      psych_percentile = pnorm(psych_zscore) * 100,
      narc_percentile = pnorm(narc_zscore) * 100,
      mach_percentile = pnorm(mach_zscore) * 100,
      gnarc_percentile = pnorm(gnarc_zscore) * 100,
      vnarc_percentile = pnorm(vnarc_zscore) * 100,
      psych_sten = round(5.5 + 2 * psych_zscore),
      cog_sten = round(5.5 + 2 * narc_zscore),
      cog_sten = round(5.5 + 2 * mach_zscore),
      gnarc_sten = round(5.5 + 2 * gnarc_zscore),
      vnarc_sten = round(5.5 + 2 * vnarc_zscore)
    ) %>%
    select(ID, cog_num_mean:vnarc_sten) %>%
    print(with = Inf)

