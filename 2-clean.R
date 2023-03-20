
# 2 - Clean ----

source("1-extract.R")

#  summarise(
#    numeric_score = mean(score, na.rm = TRUE))

score_loop <- function(prefixes, suffix){
  out <- list()
  
  for (prefix in prefixes){
    column_name <- paste0(prefix, suffix)
    
    out[[column_name]] <- data %>%
      select(ID, contains(prefix)) %>%
      pivot_longer(cols = contains(prefix), 
                   names_to = "item", 
                   values_to = "score") %>%
      group_by(ID) %>%
      summarise(!!column_name := mean(score, na.rm = TRUE))
  }
  
  result <- reduce(out, left_join, by = "ID")
  
  result <- result %>%
    mutate(cog_num_z = (cog_num_score - mean(cog_num_score) / sd(cog_num_score, na.rm = TRUE)),
           cog_ver_z = (cog_ver_score - mean(cog_ver_score) / sd(cog_ver_score, na.rm = TRUE)),
           cog_abs_z = (cog_abs_score - mean(cog_abs_score) / sd(cog_abs_score, na.rm = TRUE)),
           cog_tot_z = (cog_score - mean(cog_score) / sd(cog_score, na.rm = TRUE)))
  
  return(result)
}

result <- score_loop(c("cog_num", "cog_ver", "cog_abs", "cog"), "_score")

# Filter for data of p1
final_scores <- result %>% 
  filter(ID == "participant1") %>%
  select(ID, cog_num_z:cog_tot_z) %>%
  pivot_longer(cols = cog_num_z:cog_tot_z)

ggplot(final_scores)

# Create bar chart of scores for p1
ggplot(summary_table, aes(x = cognitive_test, y = score)) +
  geom_col() +
  ggtitle("Cognitive Scores for p1") +
  xlab("Cognitive Test") +
  ylab("Score")




## Test environment ----

score_loop <- function(prefixes, suffix){
  out <- list()
  
  for (prefix in prefixes){
    column_name <- paste0(prefix, suffix)
    
    out[[column_name]] <- data %>%
      select(ID, contains(prefix)) %>%
      pivot_longer(cols = contains(prefix), 
                   names_to = "item", 
                   values_to = "score") %>%
      group_by(ID) %>%
      summarise(!!column_name := mean(score, na.rm = TRUE))
  }
  
  result <- reduce(out, left_join, by = "ID")
  
  result <- result %>%
    mutate(cog_num_z = scale(cog_num_score),
           cog_ver_z = scale(cog_ver_score),
           cog_abs_z = scale(cog_abs_score),
           cog_tot_z = scale(cog_score),
           cog_num_pct = qnorm(scale(cog_num_score)) * 100,
           cog_ver_pct = qnorm(scale(cog_ver_score)) * 100,
           cog_abs_pct = qnorm(scale(cog_abs_score)) * 100,
           cog_tot_pct = qnorm(scale(cog_score)) * 100)
  
  return(result)
}

result <- score_test(data)

## 

plot_ability_scores <- function(data, participant){
  # Filter for data of specified participant
  final_scores <- data %>% 
    filter(ID == participant) %>%
    select(ID, cog_num_z:cog_tot_z) %>%
    pivot_longer(cols = cog_num_z:cog_tot_z)
  
  # Plot the ability scores
  ggplot(final_scores, aes(x = name, y = value)) +
    geom_col(fill = "blue", width = 0.5) +
    labs(title = paste0(participant, " ability scores"),
         x = "Cognitive ability",
         y = "Standardized score")
}

plot_ability_scores(result, "participant1")
