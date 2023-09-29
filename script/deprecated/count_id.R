
library(tidyverse)
df <- read.csv("data-raw/jess_data.csv")



unique_ids_counts <- df %>% group_by(ID, data) %>% 
  summarise(total_count=n(),
            .groups = 'drop')
