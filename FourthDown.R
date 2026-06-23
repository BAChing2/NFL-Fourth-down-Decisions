library(nflreadr)
library(tidyverse)

pbp <- load_pbp(2024)

filtered_pbp <- pbp %>%
  filter(down == 4) %>%
  mutate (decision = case_when (
    play_type %in% c("run", "pass") ~ "went_for_it",
    play_type == "punt" ~ "punt",
    play_type == "field_goal" ~ "field_goal",
    TRUE ~ "other"))
