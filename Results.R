## CLEAN TABLE
library(dplyr)
# Read the 'results.csv' file
results_df <- read.csv('/Users/andre/Desktop/ML/Data/Original/results.csv', header = TRUE, stringsAsFactors = FALSE)

# Filter rows with dates starting from 1992
filtered_results <- results_df[results_df$date >= as.Date('2018-01-01'), ]

# View the resulting dataframe
View(filtered_results)

## GOAL DIFFERENCE
# Calculate goals scored at home and away
home_goals_scored <- filtered_results %>%
  group_by(home_team) %>%
  summarise(Home_Goals_Scored = sum(home_score))

away_goals_scored <- filtered_results %>%
  group_by(away_team) %>%
  summarise(Away_Goals_Scored = sum(away_score))

# Calculate goals conceded at home and away
home_goals_conceded <- filtered_results %>%
  group_by(home_team) %>%
  summarise(Home_Goals_Conceded = sum(away_score))

away_goals_conceded <- filtered_results %>%
  group_by(away_team) %>%
  summarise(Away_Goals_Conceded = sum(home_score))

# Merge the goals scored and conceded into one dataframe
goal_data <- merge(home_goals_scored, away_goals_scored, by.x = "home_team", by.y = "away_team", all = TRUE)
goal_data <- merge(goal_data, home_goals_conceded, by.x = "home_team", by.y = "home_team", all = TRUE)
goal_data <- merge(goal_data, away_goals_conceded, by.x = "home_team", by.y = "away_team", all = TRUE)

# Replace NA values with zero - these occur if a team has only scored or only conceded at home or away
goal_data[is.na(goal_data)] <- 0

# Calculate total goals scored and conceded
goal_data$Total_Goals_Scored <- with(goal_data, Home_Goals_Scored + Away_Goals_Scored)
goal_data$Total_Goals_Conceded <- with(goal_data, Home_Goals_Conceded + Away_Goals_Conceded)

# Calculate the goal difference for each country
goal_data$Goal_Difference <- with(goal_data, Total_Goals_Scored - Total_Goals_Conceded)

# Select only the relevant columns and rename for clarity
goal_difference_df <- goal_data %>%
  select(Country = home_team, Goal_Difference)

# Sort the goal difference dataframe by the goal difference in descending order (optional)
goal_difference_df <- goal_difference_df %>%
  arrange(desc(Goal_Difference))

# View the goal difference dataframe
View(goal_difference_df) # Unwanted countries will be dropped in the future merge


## CLEAN SHEETS
# Calculate clean sheets for home games
home_clean_sheets <- filtered_results %>%
  filter(away_score == 0) %>%
  group_by(home_team) %>%
  summarise(Clean_Sheets_Home = n())

# Calculate clean sheets for away games
away_clean_sheets <- filtered_results %>%
  filter(home_score == 0) %>%
  group_by(away_team) %>%
  summarise(Clean_Sheets_Away = n())

# Merge the home and away clean sheets into one dataframe
clean_sheets <- merge(home_clean_sheets, away_clean_sheets, by.x = "home_team", by.y = "away_team", all = TRUE)

# Replace NA values with zero - these occur if a team has clean sheets only at home or away
clean_sheets[is.na(clean_sheets)] <- 0

# Sum the home and away clean sheets to get the total clean sheets for each country
clean_sheets$total_clean_sheets <- with(clean_sheets, Clean_Sheets_Home + Clean_Sheets_Away)

# Select only the relevant columns and rename for clarity
clean_sheets_df <- clean_sheets %>%
  select(Country = home_team, Total_Clean_Sheets = total_clean_sheets)

# Sort the clean sheets dataframe by the number of clean sheets in descending order (optional)
clean_sheets_df <- clean_sheets_df %>%
  arrange(desc(Total_Clean_Sheets))

## POINTS PER YEAR (INTERMEDIATE STEP)
# Add a 'year' column to the dataframe
filtered_results$year <- format(as.Date(filtered_results$date), "%Y")

# Calculate points for each game
filtered_results$home_points <- ifelse(filtered_results$home_score > filtered_results$away_score, 3,
                                 ifelse(filtered_results$home_score == filtered_results$away_score, 1, 0))
filtered_results$away_points <- ifelse(filtered_results$away_score > filtered_results$home_score, 3,
                                 ifelse(filtered_results$away_score == filtered_results$home_score, 1, 0))

# Aggregate points by year and team
home_points_by_year <- aggregate(home_points ~ home_team + year, data = filtered_results, FUN = sum)
away_points_by_year <- aggregate(away_points ~ away_team + year, data = filtered_results, FUN = sum)

# Rename columns for merging
colnames(home_points_by_year) <- c("team", "year", "points")
colnames(away_points_by_year) <- c("team", "year", "points")

# Combine the home and away points
total_points_by_year <- rbind(home_points_by_year, away_points_by_year)
# Aggregate the total points per team per year
total_points_by_year <- aggregate(points ~ team + year, data = total_points_by_year, FUN = sum)

total_points_by_year
points_table <- total_points_by_year[order(total_points_by_year$team, total_points_by_year$year), ]

View(points_table)

# Aggregate the total points per team across all years
total_points_per_country <- aggregate(points ~ team, data = total_points_by_year, FUN = sum)

# Rename the 'points' column to 'total_points' for clarity
colnames(total_points_per_country)[2] <- "total_points"

# Count the number of unique years each country has played
years_played_per_country <- aggregate(year ~ team, data = total_points_by_year, 
                                      FUN = function(x) length(unique(x)))

# Merge this with the total_points_per_country to get the number of years alongside total points
total_points_and_years_per_country <- merge(total_points_per_country, 
                                            years_played_per_country, by = "team")
# Calculate the average points per country
total_points_and_years_per_country$average_points <- with(total_points_and_years_per_country, 
                                                          total_points / length(unique(filtered_results$year)))

# Rename columns for clarity
colnames(total_points_and_years_per_country)[2:3] <- c("total_points", "years_played")

#Sort the table by average points in descending order (optional)
points_per_country <- total_points_and_years_per_country[order(-total_points_and_years_per_country$average_points), ]


## TABLE INDICATRICE DE VICTOIRES
table_I <- filtered_results %>%
  select(-date,-year, -tournament, -city, -country)

table_I$win <- ifelse(table_I$home_point > 1, "1", "0")
View(table_I)

## PPG BY COUNTRY
# Step 1: Calculate points for each game
filtered_results$home_points <- ifelse(filtered_results$home_score > filtered_results$away_score, 3,
                                 ifelse(filtered_results$home_score == filtered_results$away_score, 1, 0))
filtered_results$away_points <- ifelse(filtered_results$away_score > filtered_results$home_score, 3,
                                 ifelse(filtered_results$away_score == filtered_results$home_score, 1, 0))

# Step 2: Aggregate total points for home and away games
home_points_total <- aggregate(home_points ~ home_team, data = filtered_results, FUN = sum)
away_points_total <- aggregate(away_points ~ away_team, data = filtered_results, FUN = sum)

# Rename columns for clarity
names(home_points_total) <- c("team", "points")
names(away_points_total) <- c("team", "points")

# Combine home and away points
total_points <- rbind(home_points_total, away_points_total)

# Aggregate again to sum points from home and away for each country
country_points <- aggregate(points ~ team, data = total_points, FUN = sum)

# Step 3: Count the number of games played by each country
filtered_results$game_count <- 1  # Add a counter for each game
home_games <- aggregate(game_count ~ home_team, data = filtered_results, FUN = sum)
away_games <- aggregate(game_count ~ away_team, data = filtered_results, FUN = sum)

# Rename columns for clarity
names(home_games) <- c("team", "games_played")
names(away_games) <- c("team", "games_played")

# Combine home and away games played
total_games <- rbind(home_games, away_games)

# Aggregate again to sum games played at home and away for each country
country_games <- aggregate(games_played ~ team, data = total_games, FUN = sum)

# Step 4: Calculate points per game for each country
ppg_df <- merge(country_points, country_games, by = "team")
ppg_df$ppg <- with(ppg_df, points / games_played)

# Merge ppg and points per country tables
country_points <- merge(points_per_country, ppg_df, by = "team")
country_points <- country_points[, c("team", "years_played", "games_played", "total_points", "average_points", "ppg")]

#Order by decreasing ppg
country_points <- country_points[order(-country_points$ppg), ]
View(country_points)

## FINAL TABLE
# Merge top scorers table and points table by country
country_data <- select(country_points, -years_played, -average_points)
colnames(country_data) <- c("Country", "games_played", "total_points", "PPG")

country_data <- merge(country_data, top_scorers_by_country, by="Country", all.x = TRUE)
country_data <- merge(country_data, clean_sheets_df, by="Country", all.x = TRUE)
country_data <- merge(country_data, goal_difference_df, by="Country", all.x = TRUE)


country_data <- country_data[order(-country_data$PPG), ]
country_data[is.na(country_data)] <- 0


View(country_data)

## PREPARE FOR EXPORTATION
main_df <- merge(table_I, country_data, by.x = "home_team", by.y="Country", all.x = TRUE)

# Use the rename() function to change column names
main_df <- main_df %>%
  rename(
    GP_H = games_played,
    TP_H = total_points,
    PPG_H = PPG,
    Top_Scorers_H = Top_Scorers,
    CS_H = Total_Clean_Sheets,
    GD_H = Goal_Difference
  )

main_df <- merge(main_df, country_data, by.x = "away_team", by.y="Country", all.x = TRUE)

main_df <- main_df %>%
  rename(
    GP_A = games_played,
    TP_A = total_points,
    PPG_A = PPG,
    Top_Scorers_A = Top_Scorers,
    CS_A = Total_Clean_Sheets,
    GD_A = Goal_Difference
  )

main_df$PPG_diff <- main_df$PPG_H-main_df$PPG_A
main_df$GD_diff <- main_df$GD_H-main_df$GD_A
main_df$Top_Scorers_diff <- main_df$Top_Scorers_H-main_df$Top_Scorers_A
main_df$CS_diff <- main_df$CS_H-main_df$CS_A

final_df <- select(main_df, win, PPG_diff, GD_diff, Top_Scorers_diff, CS_diff)
final_df <- na.omit(final_df)

View(main_df)
View(final_df)
write.csv(final_df, 'final_df.csv', row.names = FALSE)

