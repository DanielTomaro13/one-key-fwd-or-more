#####################################################
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2) 
library(fitzRoy) 
library(slider)
#####################################################
player_stats <- fetch_player_stats_afltables(1990:2000)

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
# teams_1990 <- c(
#   "Adelaide", "Brisbane Bears", "Carlton", "Collingwood", "Essendon", "Fitzroy",
#   "Footscray", "Geelong", "Hawthorn", "Melbourne", "North Melbourne",
#   "Richmond", "St Kilda", "Sydney", "West Coast"
# )
# 
# player_details_1990 <- map_dfr(teams_1990, ~{
#   tryCatch(
#     fetch_player_details(team = .x, season = 1990, source = "footywire", current = FALSE),
#     error = function(e) tibble()  
#   )
# })

# saveRDS(player_details_1990, "history.RDS")
player_details_1990 <- readRDS("history.RDS")

colnames(player_details_1990)

position <- player_details_1990 %>%
  select(name, position, height) %>%
  mutate(
    Player = name,
    height = as.numeric(height), 
    is_key_forward = as.integer(position == "Forward" & height > 185)
  ) %>%
  select(Player, is_key_forward) %>%
  mutate(across(c(is_key_forward), ~replace_na(., 0)))

player_stats <- player_stats %>%
  left_join(position, by = "Player") %>%
  mutate(across(c(is_key_forward), ~replace_na(., 0)))
#####################################################
# Games where teams played two key forwards
multi_keys <- player_stats %>%
  filter(is_key_forward == 1) %>%
  group_by(Date, Round, Team) %>%
  summarise(num_key_forwards = n(), .groups = "drop") %>%
  filter(num_key_forwards >= 2)

print(multi_keys)

one_key <- player_stats %>%
  filter(is_key_forward == 1) %>%
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
  filter(is_key_forward == 1) %>%
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
