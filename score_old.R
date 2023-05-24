# 2 - Scoring

source("1-extract.R")

# Define function to calculate ability scores
# Input: prefixes - a vector of column prefixes, suffix - a string suffix for the new column names
# Output: a data frame with ability scores for each prefix

score_test <- function(prefixes, suffix) {
  # Initialize an empty list to store data frames
  out <- list()
  
  # Loop through each prefix
  for (prefix in prefixes) {
    # Generate a new column name
    column_name <- paste0(prefix, suffix)
    
    # Select the columns with the current prefix and pivot longer
    # This creates a new "item" column and "score" column
    # containing the item name and the score for each item
    current_data <- data %>%
      select(ID, contains(prefix)) %>%
      pivot_longer(cols = contains(prefix), 
                   names_to = "item", 
                   values_to = "score")
    
    # Group the data by ID and calculate the mean score for each item
    # The !! operator is used to evaluate the variable column_name as a string
    # This creates a new column with the current prefix and the specified suffix
    current_scores <- current_data %>%
      group_by(ID, item) %>%
      summarise(mean_score = mean(score, na.rm = TRUE)) %>%
      ungroup() %>%
      pivot_wider(names_from = item, values_from = mean_score, names_prefix = paste0(prefix, "_"))
    
    # Add the new scores to the list
    out[[column_name]] <- current_scores
  }
  
  # Reduce the list of data frames into a single data frame
  result <- reduce(out, left_join, by = "ID")
  
  # Return the result
  return(result)
}

# Define function to calculate percentile scores
# Input: data - a data frame with z-score columns for a single participant
# Output: a data frame with percentile scores for each z-score column
calc_percentile_scores <- function(data) {
  # Initialize an empty list to store percentile scores
  out <- list()
  
  # Loop through each z-score column
  for (col_name in colnames(data)) {
    # Calculate the percentile score for the current z-score column
    current_percentiles <- data %>%
      summarise(percentile = round(ecdf(!!sym(col_name))(!!sym(col_name)) * 100, 2))
    
    # Add the new percentile scores to the list
    out[[col_name]] <- current_percentiles
  }
  
  # Reduce the list of data frames into a single data frame
  result <- reduce(out, cbind)
  
  # Return the result
  return(result)
}

# Define function to plot ability scores for a single participant
# Input: participant - a string specifying the participant ID, prefixes - a vector of column prefixes
# Output: a ggplot2 bar chart of ability scores for the specified participant
plot_ability_scores <- function(participant, prefixes) {
  # Calculate the ability scores for the specified prefixes
  scores <- score_test(prefixes, "_score")
  
  # Filter the scores for the specified participant
  filtered_scores <- scores %>% filter(ID == participant)
  
  # Calculate the z-scores for the filtered scores
  z_scores <- filtered_scores %>%
    mutate(across(starts_with("cog_"), ~ (.-mean(.))/sd(., na.rm = TRUE))) %>%
    select(starts_with("cog_"))
  
  # Calculate the percentile scores for the filtered scores
  percentile_scores <- calc_percentile_scores(z_scores)
}
  # Combine the filtered scores and percentile
  