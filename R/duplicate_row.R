duplicate_row <- function(df, condition, n) {

  
  # Filter the data frame based on the condition
  row_to_duplicate_df <- df[condition, ]
  no_duplicate_df <- df[!condition, ]
  
  # Create the duplicated rows
  duplicated_rows <- row_to_duplicate_df %>%
    tidyr::uncount(n)
  
  
  # Bind the duplicated rows to the original data frame
  combined_df = rbind( no_duplicate_df, duplicated_rows)
  
  return(combined_df)
}
