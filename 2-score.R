
# 2 - Clean ----

source("1-extract.R")

## Ability ---

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


## Personality ----

reverse_score_item <- function(x){ 
  (6-x) 
}

# (High) Vulnerability for Narc Neuroticism
s1data$dt_Vulner_s1_rev <- rev_score(s1data$dt_Vulner_s1) 
# (Low) Indifference for Narc Neuroticism
s1data$dt_Indiff_s1_rev <- rev_score(s1data$dt_Indiff_s1) 
# (High) Deliberation for Mach Planfulness
s1data$dt_Delib_s1_rev <- rev_score(s1data$dt_Delib_s1) 

# fdt_achiev	fdt_activ	fdt_altr	fdt_anger	fdt_anxie	fdt_assert	
# fdt_compet	fdt_compl	fdt_delib	fdt_depr	fdt_dutif	fdt_entit	
# fdt_excite	fdt_fant	fdt_gregar	fdt_impul	fdt_indiff	
# fdt_modest	fdt_order	fdt_self-ass	fdt_self-dis	fdt_shame	
# fdt_straight	fdt_tender	fdt_trust	fdt_vulner	fdt_warm

personality_vars <- tibble(
  dt_psyTOTAL_4item_s356 = c("dt_Angry_s356",	"dt_Modest_s356", "dt_Tender_s356",
                             "dt_Warmth_s356",	"dt_Dutif_s356",	"dt_Trust_s356",	
                             "dt_Assert_s356",	"dt_SelfDisc_s356",	"dt_Vulner_s356",	
                             "dt_Straight_s356",	"dt_Complianc_s356",	"dt_Delib_s356",	
                             "dt_SelfAss_s356",	"dt_Altru_s356",	"dt_Depress_s356"	
                             "dt_Excite_s356",	"dt_Anxiet_s356",	"dt_Impuls_s356"),
  dt_narTOTAL_4item_s356 = c("dt_Achiev_s356", "dt_Modest_s356",	"dt_Assert_s356",
                             "dt_Trust_s356",	"dt_Entit_s356",	"dt_Gregar_s356",
                             "dt_Altru_s356",	"dt_Fantasy_s356",	"dt_Indiff_s356",
                             "dt_Tender_s356",	"dt_Straight_s356",	"dt_Vulner_s356_rev",
                             dt_Angry_s356	dt_Shame_s356	dt_Excite_s356),
  dt_macTOTAL_4item_s356 = c(dt_Achiev_s356 dt_Activ_s356	dt_Altru_s356
                             dt_Assert_s356	dt_Compet_s356	dt_Delib_s356_rev
                             dt_Vulner_s356	dt_Modest_s356	dt_Order_s356
                             dt_SelfAss_s356	dt_Straight_s356	dt_Tender_s356
                             dt_Trust_s356
  dt_narGRAND_4item_s356 = c(dt_Achiev_s356	dt_Modest_s356	dt_Assert_s356	dt_Entit_s356	dt_Gregar_s356	dt_Altru_s356	dt_Fantasy_s356	dt_Indiff_s356	dt_Tender_s356	dt_Straight_s356	dt_Excite_s356				
  dt_narVULNER_4item_s356	dt_Trust_s356	dt_Vulner_s356_rev	dt_Angry_s356	dt_Shame_s356											
)





print(data, width = Inf)
