#####################################################
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2) 
library(fitzRoy) 
library(slider)
#####################################################
player_stats <- fetch_player_stats_afltables(2024)

colnames(player_stats)

player_stats <- player_stats %>% select(
  Date, Season, Round, Team, Player, ID, Kicks, Marks, Handballs, Disposals, Goals, Behinds, Hit.Outs, Tackles, Rebounds, Inside.50s,
  Clearances, Clangers, Frees.For, Frees.Against, Brownlow.Votes, Contested.Possessions, Uncontested.Possessions, Contested.Marks,
  Marks.Inside.50, One.Percenters, Bounces, Goal.Assists, Time.on.Ground, Age, Career.Games
  
)
colSums(is.na(player_stats))
player_stats <- player_stats %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))
colSums(is.na(player_stats))

stat_cols <- player_stats %>%
  select(where(is.numeric)) %>%
  select(-ID, -Season) %>%  
  colnames()
#####################################################
# Player Position
library(purrr)
seasons <- 2024
player_details_all <- map_dfr(seasons, ~{
  fetch_player_details(season = .x, source = "footywire")
 })

position <- player_details_all %>%
  select(first_name, surname, Position_1, Position_2) %>%
  mutate(
    Player = paste(first_name, surname),
    is_forward = as.integer(Position_1 == "Forward" | Position_2 == "Forward")
  ) %>%
  select(Player, is_forward) %>% 
  mutate(
    across(c(is_forward), ~replace_na(., 0))
  )

player_stats <- player_stats %>%
  left_join(position, by = "Player") %>%
  mutate(across(c(is_forward), ~replace_na(., 0)))
#####################################################
# is_key_forward 
library(purrr)
seasons <- 2024
player_details_specific <- map_dfr(seasons, ~{
  fetch_player_details(season = .x, source = "AFL")
 })

detailed_position <- player_details_specific %>%
  mutate(
    Player = paste(firstName, surname),
    is_key_forward = as.integer(position == "KEY_FORWARD")
  ) %>%
  select(Player, is_key_forward) %>%
  distinct(Player, .keep_all = TRUE)

player_stats <- player_stats %>%
  left_join(detailed_position, by = "Player") %>%
  mutate(across(c(is_key_forward), ~replace_na(.x, 0)))
#####################################################
# Games where teams played two key forwards
multi_keys <- player_stats %>%
  filter(is_key_forward == 1, Time.on.Ground > 75) %>%
  group_by(Date, Round, Team) %>%
  summarise(num_key_forwards = n(), .groups = "drop") %>%
  filter(num_key_forwards >= 2)

print(multi_keys)

one_key <- player_stats %>%
  filter(is_key_forward == 1, Time.on.Ground > 75) %>%
  group_by(Date, Round, Team) %>%
  summarise(num_key_forwards = n(), .groups = "drop") %>%
  filter(num_key_forwards == 1)

print(one_key)
#####################################################
multi_games <- multi_keys %>%
  mutate(has_multiple_kf = TRUE)

one_games <- one_key %>%
  mutate(has_multiple_kf = FALSE)

key_forward_games <- bind_rows(multi_games, one_games)

kf_stats_labeled <- player_stats %>%
  filter(is_key_forward == 1, Time.on.Ground > 75) %>%
  inner_join(key_forward_games, by = c("Date", "Round", "Team"))
#####################################################
# Compare stats
kf_comparison <- kf_stats_labeled %>%
  group_by(has_multiple_kf) %>%
  summarise(
    Games = n(),
    Avg_Disposals = mean(Disposals, na.rm = TRUE),
    Avg_Goals = mean(Goals, na.rm = TRUE),
    Avg_Marks = mean(Marks, na.rm = TRUE),
    Avg_Contested_Marks = mean(Contested.Marks, na.rm = TRUE),
    Avg_Inside50 = mean(Inside.50s, na.rm = TRUE),
    Avg_Clangers = mean(Clangers, na.rm = TRUE)
  )

print(kf_comparison)

kf_stats_labeled %>%
  ggplot(aes(x = has_multiple_kf, y = Goals)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Key Forward Goals: 1 vs Multiple KFs", x = "Multiple KFs?", y = "Goals")

t.test(Goals ~ has_multiple_kf, data = kf_stats_labeled)

