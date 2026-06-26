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

team_agression <- filtered_pbp %>%
  filter(decision != "other") %>%
  group_by(posteam) %>%
  summarize(
    total_4th = n(),
    go_rate = mean(decision == "went_for_it")
  ) %>%
  arrange(desc(go_rate))

situations <- filtered_pbp %>%
  mutate(distance = case_when (
     ydstogo <= 2 ~ "short",
     ydstogo <= 5 ~ "medium",
     TRUE ~ "long"
  )) %>%
  group_by(distance) %>%
  summarize(go_rate = mean(decision == "went_for_it"))

team_by_distance <- filtered_pbp %>%
  filter(decision != "other") %>%
  mutate(distance = case_when (
    ydstogo <= 2 ~ "short",
    ydstogo <= 5 ~ "medium",
    TRUE ~ "long"
  )) %>%
  group_by(posteam, distance) %>%
  summarize(
    go_rate = mean(decision == "went_for_it"),
    n = n(),
    .groups = "drop"
  ) 

short_yardage <- team_by_distance %>%
  filter(distance == "short") %>%
  arrange(desc(go_rate))

ggplot(team_agression, aes(x = reorder(posteam, go_rate), y = go_rate)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "4th Down Aggressiveness by Team in 2024",
    x = "Team",
    y = "Go For It Rate"
  )
ggplot(short_yardage, aes(x = reorder(posteam, go_rate), y = go_rate)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "4th and Short Aggressiveness by Team in 2024",
    x = "Team",
    y = "Go For It Rate"
  )

ggsave("output/fourth_short_aggressiveness_2024.png", width = 10, height = 8, dpi = 300)
ggsave("output/fourth_down_aggressiveness_2024.png", width = 10, height = 8, dpi = 300)
