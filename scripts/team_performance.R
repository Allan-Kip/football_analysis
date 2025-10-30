# team_performance.R
# ==================
# Team Performance Analysis - Premier League

# Load packages
library(tidyverse)
library(lubridate)

# Load dataset
matches <- read.csv("data/matches.csv")

# Quick check of columns
glimpse(matches)

# Clean and prepare data
# Ensure date and score columns exist
matches <- matches %>%
  mutate(Date = dmy(Date),
         TotalGoals = FTHG + FTAG)

# ---- HOME TEAM PERFORMANCE ----
home_stats <- matches %>%
  group_by(HomeTeam) %>%
  summarise(
    Matches = n(),
    Goals_Scored = sum(FTHG, na.rm = TRUE),
    Goals_Conceded = sum(FTAG, na.rm = TRUE),
    Avg_Shots = mean(HS, na.rm = TRUE),
    Wins = sum(FTR == "H", na.rm = TRUE),
    Win_Rate = Wins / Matches * 100
  ) %>%
  arrange(desc(Win_Rate))

# ---- AWAY TEAM PERFORMANCE ----
away_stats <- matches %>%
  group_by(AwayTeam) %>%
  summarise(
    Matches = n(),
    Goals_Scored = sum(FTAG, na.rm = TRUE),
    Goals_Conceded = sum(FTHG, na.rm = TRUE),
    Avg_Shots = mean(AS, na.rm = TRUE),
    Wins = sum(FTR == "A", na.rm = TRUE),
    Win_Rate = Wins / Matches * 100
  ) %>%
  arrange(desc(Win_Rate))

# ---- Combine Home & Away Performance ----
team_summary <- full_join(home_stats, away_stats,
                          by = c("HomeTeam" = "AwayTeam"),
                          suffix = c("_Home", "_Away")) %>%
  rename(Team = HomeTeam) %>%
  mutate(
    Total_Wins = Wins_Home + Wins_Away,
    Total_Goals = Goals_Scored_Home + Goals_Scored_Away,
    Total_Conceded = Goals_Conceded_Home + Goals_Conceded_Away
  )

# ---- Plot 1: Total Goals Scored ----
ggplot(team_summary, aes(x = reorder(Team, -Total_Goals), y = Total_Goals, fill = Total_Goals)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Total Goals Scored by Team (Home + Away)",
       x = "Team", y = "Total Goals") +
  theme_minimal()

# ---- Plot 2: Win Rate Comparison ----
ggplot(team_summary, aes(x = reorder(Team, -Total_Wins), y = Total_Wins, fill = Total_Wins)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Total Wins by Team (Home + Away)",
       x = "Team", y = "Total Wins") +
  theme_minimal()

# ---- Plot 3: Home vs Away Win Rate ----
team_summary_long <- team_summary %>%
  select(Team, Win_Rate_Home, Win_Rate_Away) %>%
  pivot_longer(cols = starts_with("Win_Rate"), names_to = "Type", values_to = "WinRate")

ggplot(team_summary_long, aes(x = Team, y = WinRate, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Home vs Away Win Rate by Team", x = "Team", y = "Win Rate (%)") +
  theme_minimal()


library(fmsb)   # for radar charts
# 1️⃣ Load and Clean Data
# -----------------------------
matches <- read.csv("data/matches.csv")

matches <- matches %>%
  mutate(Date = dmy(Date),
         Month = floor_date(Date, "month"),
         TotalGoals = FTHG + FTAG)

# 2️⃣ Team Performance Summary

home_stats <- matches %>%
  group_by(HomeTeam) %>%
  summarise(
    Matches = n(),
    Goals_Scored = sum(FTHG, na.rm = TRUE),
    Goals_Conceded = sum(FTAG, na.rm = TRUE),
    Avg_Shots = mean(HS, na.rm = TRUE),
    Wins = sum(FTR == "H", na.rm = TRUE),
    Win_Rate = Wins / Matches * 100
  )

away_stats <- matches %>%
  group_by(AwayTeam) %>%
  summarise(
    Matches = n(),
    Goals_Scored = sum(FTAG, na.rm = TRUE),
    Goals_Conceded = sum(FTHG, na.rm = TRUE),
    Avg_Shots = mean(AS, na.rm = TRUE),
    Wins = sum(FTR == "A", na.rm = TRUE),
    Win_Rate = Wins / Matches * 100
  )

team_summary <- full_join(home_stats, away_stats,
                          by = c("HomeTeam" = "AwayTeam"),
                          suffix = c("_Home", "_Away")) %>%
  rename(Team = HomeTeam) %>%
  mutate(
    Total_Wins = Wins_Home + Wins_Away,
    Total_Goals = Goals_Scored_Home + Goals_Scored_Away,
    Total_Conceded = Goals_Conceded_Home + Goals_Conceded_Away
  )

# -----------------------------
# 3️⃣ Win Trends Over Time (Monthly)
# -----------------------------
monthly_trends <- matches %>%
  group_by(Month) %>%
  summarise(
    HomeWins = sum(FTR == "H"),
    AwayWins = sum(FTR == "A"),
    Draws = sum(FTR == "D"),
    AvgGoals = mean(TotalGoals, na.rm = TRUE)
  )

# Plot: Win Trends
ggplot(monthly_trends, aes(x = Month)) +
  geom_line(aes(y = HomeWins, color = "Home Wins"), size = 1.2) +
  geom_line(aes(y = AwayWins, color = "Away Wins"), size = 1.2) +
  geom_line(aes(y = Draws, color = "Draws"), size = 1.2) +
  labs(title = "📅 Win Trends Over Time (Monthly)",
       x = "Month", y = "Number of Matches",
       color = "Result") +
  theme_minimal()

# Plot: Average Goals Over Time
ggplot(monthly_trends, aes(x = Month, y = AvgGoals)) +
  geom_line(color = "darkgreen", size = 1.3) +
  geom_point(color = "darkgreen", size = 3) +
  labs(title = "⚽ Average Goals Per Match Over Time",
       x = "Month", y = "Average Goals") +
  theme_minimal()

# -----------------------------
# 4️⃣ Correlation Heatmap
# -----------------------------
# Select numerical match statistics
corr_data <- matches %>%
  select(HS, AS, HST, AST, HC, AC, HF, AF, FTHG, FTAG)

corr_matrix <- cor(corr_data, use = "complete.obs")

ggcorrplot(corr_matrix,
           method = "circle",
           type = "lower",
           lab = TRUE,
           title = "📈 Correlation Heatmap: Match Statistics",
           ggtheme = theme_minimal())

# -----------------------------
# 5️⃣ Radar Chart for Top 4 Teams
# -----------------------------
# Select top 4 teams by total wins
top4 <- team_summary %>%
  arrange(desc(Total_Wins)) %>%
  slice(1:4) %>%
  select(Team, Total_Goals, Total_Conceded, Avg_Shots_Home, Avg_Shots_Away)

# Normalize data (for radar scale)
radar_data <- top4 %>%
  column_to_rownames("Team")

# Add max and min rows for fmsb
radar_data <- rbind(
  max = apply(radar_data, 2, max),
  min = apply(radar_data, 2, min),
  radar_data
)

# Plot radar
colors <- c("red", "blue", "green", "purple")

radarchart(radar_data,
           axistype = 1,
           pcol = colors,
           plwd = 2,
           plty = 1,
           title = "🧮 Top 4 Teams Comparison (Radar Chart)")

legend("topright", legend = rownames(radar_data)[3:6],
       col = colors, lty = 1, lwd = 2)
