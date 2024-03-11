library(cfbfastR)
library(dplyr)
library(tidyverse)
library(xgboost)
library(caret)
library(vip)
library(gt)
library(gtExtras)
library(fuzzyjoin)

qb_stats <- data.frame()

for (szn in 2014:2023) {
  pbp <- load_cfb_pbp(szn)
  pbp$yards_gained[which(pbp$yards_gained == 11131)] <- 0
  qb_stats_szn <- pbp %>%
    mutate(name = ifelse(is.na(passer_player_name), rusher_player_name, passer_player_name)) %>%
    filter(pass == 1 | rush == 1) %>% 
    group_by(name) %>% 
    mutate(pass_yards = ifelse(pass == 1, yards_gained, 0), 
           sack_yards = ifelse(sack == 1, -yards_gained, 0), 
           success = ifelse(EPA > 0, 1, 0),
           season = szn) %>%
    summarize(season = last(season),
              team = last(pos_team), 
              plays = n(), 
              pass_attempts = sum(pass_attempt, na.rm = TRUE), 
              epa_per_play = mean(EPA, na.rm = TRUE), 
              pass_yards = sum(pass_yards, na.rm = TRUE), 
              pass_touchdowns = sum(pass_td, na.rm = TRUE), 
              sacks = sum(sack, na.rm = TRUE), 
              interceptions = sum(int, na.rm = TRUE), 
              sack_yards = sum(sack_yards, na.rm = TRUE), 
              any_a = (pass_yards + 20 * pass_touchdowns - 45 * interceptions - sack_yards)/(pass_attempts + sacks),
              success_rate = mean(success, na.rm = TRUE) * 100, 

            ) %>%
    filter(pass_attempts >= 200) %>%
    select(name, team, season, epa_per_play, any_a, success_rate) %>%
    filter(!is.na(name))
  
  teams <- cfbd_team_info(year = szn) %>%
    select(school, conference)
  
  conferences <- cfbd_ratings_sp_conference(szn) %>%
    select(conference, rating)
  
  qb_stats_szn <- qb_stats_szn %>%
    left_join(teams, by = c("team"="school")) %>%
    left_join(conferences, by = "conference")
  
  qb_stats_szn <- qb_stats_szn %>% filter(!is.na(conference))
  
  qb_stats <- rbind(qb_stats, qb_stats_szn)
}

# tablecapture = chrome extension

sos <- data.frame()
for (year in 2014:2023) {
  sos_szn <- read_csv(paste0("sos/SoS-", year, ".csv"))
  sos_szn <- sos_szn %>% mutate(szn = year) %>% select(szn, team = Team, rating = Rating)
  sos_szn$team <- sub("\\s*\\((?!OH).*\\)", "", sos_szn$team, perl = TRUE)
  sos <- rbind(sos, sos_szn)
}

sos$team <- gsub("(?<!State)\\bSt\\b", "State", sos$team, perl = TRUE)

teams_og <- qb_stats %>% distinct(team)

teams_try <- left_join(teams_og, sos, by = "team")

na_teams <- teams_try %>% filter(is.na(rating))
na_teams <- stringdist_left_join(na_teams, sos, by = "team", method = "jw", max_dist = 1, distance_col = 'distance') 
na_teams <- na_teams %>% group_by(team.x) %>% filter(distance == min(distance))
wrong_teams <- na_teams %>% filter(team.x %in% c("Appalachian State", "Eastern Michigan", "Florida Atlantic", "Georgia Southern", "Georgia Tech", "Louisiana Monroe", "Mississippi State", "New Mexico State", "Ole Miss", "Virginia Tech", "Washington State")) %>% distinct(team.x)
na_teams <- anti_join(na_teams, wrong_teams, by = "team.x")

wrong_sos <- sos
wrong_sos$team[which(wrong_sos$team == "App State")] <- "Appalachian State"
wrong_sos$team[which(wrong_sos$team == "E Michigan")] <- "Eastern Michigan"
wrong_sos$team[which(wrong_sos$team == "Fla Atlantic")] <- "Florida Atlantic"
wrong_sos$team[which(wrong_sos$team == "GA Southern")] <- "Georgia Southern"
wrong_sos$team[which(wrong_sos$team == "GA Tech")] <- "Georgia Tech"
wrong_sos$team[which(wrong_sos$team == "UL Monroe")] <- "Louisiana Monroe"
wrong_sos$team[which(wrong_sos$team == "Miss State")] <- "Mississippi State"
wrong_sos$team[which(wrong_sos$team == "N Mex State")] <- "New Mexico State"
wrong_sos$team[which(wrong_sos$team == "Mississippi")] <- "Ole Miss"
wrong_sos$team[which(wrong_sos$team == "VA Tech")] <- "Virginia Tech"
wrong_sos$team[which(wrong_sos$team == "Wash State")] <- "Washington State"

wrong_teams <- left_join(wrong_teams, wrong_sos, by = c("team.x"="team")) %>% select(team = team.x, szn, rating)

na_teams <- na_teams %>% select(team = team.x, szn = szn.y, rating = rating.y)
na_teams <- rbind(na_teams, wrong_teams)

teams_try <- teams_try %>% filter(!is.na(rating))

teams <- rbind(teams_try, na_teams) %>% select(szn, team, sos = rating)

qb_stats <- left_join(qb_stats, teams, by = c("season"="szn", "team")) %>% select(-rating)

qb_stats <- qb_stats %>%
  group_by(season) %>%
  mutate_at(c(4:6, 8), function(x) rank(x) / length(x) * 100) %>%
  ungroup()

pca_qbs <- prcomp(qb_stats[,c(4:6,8)], scale = FALSE)
weights_qbs <- pca_qbs$rotation[,1]
weighted_avg_qbs <- rowSums(qb_stats[,c(4:6,8)] * weights_qbs)
qb_stats$pca_metric <- weighted_avg_qbs

qb_picks <- data.frame()
for (yr in 2018:2023) {
  picks_yr <- cfbd_draft_picks(yr) %>%
    filter(position == "Quarterback")
  qb_picks <- rbind(qb_picks, picks_yr)
}

qb_picks$name[which(qb_picks$name == "Alex Mcgough" )] <- "Alex McGough"

qb_pick_stats <- right_join(qb_stats, qb_picks, by = "name") 

qb_pick_stats <- qb_pick_stats %>% filter(!is.na(team))

qb_pick_stats <- qb_pick_stats[,c(1:6,8:9,16:17)]

qb_pick_stats <- qb_pick_stats %>%
  group_by(name) %>%
  mutate(season_recent_num = rank(-season))

qb_pick_stats_wide <- qb_pick_stats %>%
  ungroup() %>%
  pivot_wider(
    names_from = season_recent_num,
    values_from = c(epa_per_play, any_a, success_rate, sos, pca_metric),
    names_sep = "_"
  ) %>%
  group_by(name) %>%
  summarize(team = first(team), overall = first(overall), draft_year = first(year), across(5:24, ~max(., na.rm = TRUE))) %>%
  mutate_all(~ifelse(. == -Inf, NA, .))

one_year <- qb_pick_stats_wide %>% filter(is.na(epa_per_play_2)) %>% select_if(~any(!is.na(.)))
two_year <- qb_pick_stats_wide %>% filter(is.na(epa_per_play_3) & !is.na(epa_per_play_2)) %>% select_if(~any(!is.na(.)))
three_year <- qb_pick_stats_wide %>% filter(is.na(epa_per_play_4) & !is.na(epa_per_play_3) & !is.na(epa_per_play_2)) %>% select_if(~any(!is.na(.)))
four_year <- qb_pick_stats_wide %>% filter(!is.na(epa_per_play_4)) %>% select_if(~any(!is.na(.)))

labels_1 <- as.matrix(one_year[, 3])
train_1 <- as.matrix(one_year[, c(5:8)])

model_1 <- xgboost(data = train_1, label = labels_1, nrounds = 100, objective = "reg:squarederror", early_stopping_rounds = 10, max_depth = 6, eta = 0.3)

labels_2 <- as.matrix(two_year[, 3])
train_2 <- as.matrix(two_year[, c(5:12)])

model_2 <- xgboost(data = train_2, label = labels_2, nrounds = 100, objective = "reg:squarederror", early_stopping_rounds = 10, max_depth = 6, eta = 0.3)

labels_3 <- as.matrix(three_year[, 3])
train_3 <- as.matrix(three_year[, c(5:16)])

model_3 <- xgboost(data = train_3, label = labels_3, nrounds = 100, objective = "reg:squarederror", early_stopping_rounds = 10, max_depth = 6, eta = 0.3)

labels_4 <- as.matrix(four_year[, 3])
train_4 <- as.matrix(four_year[, c(5:20)])

model_4 <- xgboost(data = train_4, label = labels_4, nrounds = 100, objective = "reg:squarederror", early_stopping_rounds = 10, max_depth = 6, eta = 0.3)

qb_stats_2023 <- qb_stats %>% filter(season == 2023)

qbs_2023 <- unique(qb_stats_2023$name)

test_initial <- qb_stats %>% filter(name %in% qbs_2023)

test_initial <- test_initial %>%
  group_by(name) %>%
  mutate(season_recent_num = rank(-season))

test_stats_wide <- test_initial %>%
  ungroup() %>%
  pivot_wider(
    names_from = season_recent_num,
    values_from = c(epa_per_play, any_a, success_rate, sos, pca_metric),
    names_sep = "_"
  ) %>%
  group_by(name) %>%
  summarize(team = last(team), across(4:28, ~max(., na.rm = TRUE))) %>%
  mutate_all(~ifelse(. == -Inf, NA, .))

test_stats_wide <- test_stats_wide %>%
  select(-ends_with("_5"))

one_year_test <- test_stats_wide %>% filter(is.na(epa_per_play_2)) %>% select_if(~any(!is.na(.)))
two_year_test <- test_stats_wide %>% filter(is.na(epa_per_play_3) & !is.na(epa_per_play_2)) %>% select_if(~any(!is.na(.)))
three_year_test <- test_stats_wide %>% filter(is.na(epa_per_play_4) & !is.na(epa_per_play_3) & !is.na(epa_per_play_2)) %>% select_if(~any(!is.na(.)))
four_year_test <- test_stats_wide %>% filter(!is.na(epa_per_play_4)) %>% select_if(~any(!is.na(.)))

model_2_weights <- as.data.frame(vi(model_2))

model_2_weights <- model_2_weights %>%
  mutate(weight_num = paste0("weight", substring(Variable, nchar(Variable) - 1, nchar(Variable)))) %>%
  group_by(weight_num) %>%
  summarize(weight = sum(Importance))

model_2_weights <- model_2_weights %>% pivot_wider(names_from = weight_num, values_from = weight, names_sep = "_")

two_year <- cbind(two_year, model_2_weights)
two_year_test <- cbind(two_year_test, model_2_weights)

two_year <- two_year %>% mutate(pca_final = pca_metric_1 * weight_1 + pca_metric_2 * weight_2)
two_year_test <- two_year_test %>% mutate(pca_final = pca_metric_1 * weight_1 + pca_metric_2 * weight_2)

model_3_weights <- as.data.frame(vi(model_3))

model_3_weights <- model_3_weights %>%
  mutate(weight_num = paste0("weight", substring(Variable, nchar(Variable) - 1, nchar(Variable)))) %>%
  group_by(weight_num) %>%
  summarize(weight = sum(Importance))

model_3_weights <- model_3_weights %>% pivot_wider(names_from = weight_num, values_from = weight, names_sep = "_")

three_year <- cbind(three_year, model_3_weights)
three_year_test <- cbind(three_year_test, model_3_weights)

three_year <- three_year %>% mutate(pca_final = pca_metric_1 * weight_1 + pca_metric_2 * weight_2 + pca_metric_3 * weight_3)
three_year_test <- three_year_test %>% mutate(pca_final = pca_metric_1 * weight_1 + pca_metric_2 * weight_2 + pca_metric_3 * weight_3)

model_4_weights <- as.data.frame(vi(model_4))

model_4_weights <- model_4_weights %>%
  mutate(weight_num = paste0("weight", substring(Variable, nchar(Variable) - 1, nchar(Variable)))) %>%
  group_by(weight_num) %>%
  summarize(weight = sum(Importance))

model_4_weights <- model_4_weights %>% pivot_wider(names_from = weight_num, values_from = weight, names_sep = "_")

four_year <- cbind(four_year, model_4_weights)
four_year_test <- cbind(four_year_test, model_4_weights)

four_year <- four_year %>% mutate(pca_final = pca_metric_1 * weight_1 + pca_metric_2 * weight_2 + pca_metric_3 * weight_3 + pca_metric_4 * weight_4)
four_year_test <- four_year_test %>% mutate(pca_final = pca_metric_1 * weight_1 + pca_metric_2 * weight_2 + pca_metric_3 * weight_3 + pca_metric_4 * weight_4)

one_year <- one_year %>% select(name, team, overall, pca_final = pca_metric_1)
two_year <- two_year %>% select(name, team, overall, pca_final)
three_year <- three_year %>% select(name, team, overall, pca_final)
four_year <- four_year %>% select(name, team, overall, pca_final)

one_year_test <- one_year_test %>% select(name, team, pca_final = pca_metric_1)
two_year_test <- two_year_test %>% select(name, team, pca_final)
three_year_test <- three_year_test %>% select(name, team, pca_final)
four_year_test <- four_year_test %>% select(name, team, pca_final)

one_year_final <- one_year %>% mutate(num_szns = 1)
two_year_final <- two_year %>% mutate(num_szns = 2) 
three_year_final <- three_year %>% mutate(num_szns = 3) 
four_year_final <- four_year %>% mutate(num_szns = 4)

one_year_test <- one_year_test %>% mutate(num_szns = 1)
two_year_test <- two_year_test %>% mutate(num_szns = 2) 
three_year_test <- three_year_test %>% mutate(num_szns = 3) 
four_year_test <- four_year_test %>% mutate(num_szns = 4)

final_stats <- rbind(one_year_final, two_year_final, three_year_final, four_year_final)

test_data <- rbind(one_year_test, two_year_test, three_year_test, four_year_test)

qbs_draft <- c("Caleb Williams", "Drake Maye", "Jayden Daniels", "Michael Penix Jr.", "J.J. McCarthy", "Bo Nix", "Michael Pratt", "Spencer Rattler", "Jordan Travis", "Taulia Tagovailoa", "Kedon Slovis", "Joe Milton III", "Austin Reed", "Brennan Armstrong", "Sam Hartman", "Ben Bryant", "Devin Leary", "Tanner Mordecai", "Spencer Sanders", "Davius Richard", "Jack Plummer", "Emory Jones", "John Rhys Plumlee", "Carter Bradley", "Parker McKinney", "Jason Bean", "Rocky Lombardi", "Gavin Hardison")

qbs_2024_data <- test_data %>% filter(name %in% qbs_draft) 

qbs_2024_data <- qbs_2024_data %>% mutate(rating = 30/min(qbs_2024_data$pca_final) * pca_final) %>% arrange(-rating) %>% select(-pca_final)

players <- data.frame()

qbs_2024_data$name[which(qbs_2024_data$name == "Joe Milton III")] <- "Joe Milton"

for (qb in qbs_2024_data$name) {
  info <- cfbd_player_info(qb)
  players <- rbind(players, info)
}

players <- players %>% select(name, team, id = athlete_id)

qbs_2024_data <- left_join(qbs_2024_data, players, by = c("name", "team"))

teams <- cfbd_team_info() %>% select(team_id, team = school)

qbs_2024_data <- left_join(qbs_2024_data, teams, by = c("team"))

qbs_2024_data <- qbs_2024_data %>%
  mutate(headshot_link = paste0("https://a.espncdn.com/combiner/i?img=/i/headshots/college-football/players/full/", id, ".png&w=350&h=254")) %>%
  mutate(team_logo = paste0("https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/", team_id, ".png&h=200&w=200"))

gt_nice_stats <- qbs_2024_data %>%
  mutate(rating = round(rating, 1)) %>%
  select(headshot_link, name, team_logo, rating)

gt_align_caption <- function(left, right) {
  caption <- paste0(
    '<span style="float: left;">', left, '</span>',
    '<span style="float: right;">', right, '</span>'
  )
  return(caption)
}

caption_1 = gt_align_caption("Data from <b>cfbfastR</b>", "Amrit Vignesh | <b>@avsportsanalyst</b>")
caption_2 = gt_align_caption("<b>Football Analytics</b>", "Varun Ramanathan | <b>@VarunRaman2205</b>")


nice_table <- gt_nice_stats %>% gt() %>%
  gt_img_rows(columns = team_logo, height = 40) %>%
  gt_img_rows(columns = headshot_link, height = 50) %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = c(headshot_link, name, team_logo, rating)
  ) %>%
  gt_hulk_col_numeric(rating) %>%
  cols_label(
    headshot_link = md(""),
    name = md("**QB**"),
    team_logo = md("**School**"),
    rating = md("**Rating**")
  ) %>%
  tab_header(
    title = "2024 NFL Draft QB Prospect Big Board",
    subtitle = md("*Derived from **CFB** Statistics | Rating Relative to **Lowest** Prospect at **30***")
  ) %>% 
  tab_source_note(html(caption_1)) %>%
  tab_source_note(html(caption_2)) %>%
  opt_align_table_header(align = "center") %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(name, rating)
    )
  ) 

gtsave(nice_table, "nice_table.png", vwidth = 1000, vheight = 2500, zoom = 1)
