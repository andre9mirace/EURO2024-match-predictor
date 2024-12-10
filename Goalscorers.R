## Clean Goalscorers
# Read the 'results.csv' file
goalscorers_df <- read.csv('~/Desktop/ML/Data/Original/goalscorers.csv', header = TRUE, stringsAsFactors = FALSE)

# Filter rows with dates starting from 2020
goalscorers_df <- goalscorers_df[goalscorers_df$date >= as.Date('2018-01-01'), ]

# Separate dataframe with unique player names and their associated country
player_countries <- goalscorers_df %>%
  select(scorer, team) %>%
  distinct()

# Filter own-goals
but_par_joueur <- goalscorers_df %>%
  filter(own_goal == FALSE) %>%
  group_by(scorer, team) %>%
  summarise(Total_goals = n()) %>%
  ungroup() %>%  # Ungroup for further manipulation
  arrange(desc(Total_goals))

colnames(but_par_joueur) <- c("Player", "Country", "Total_goals")

# Filter the 95th percentile of scorers
ninetyfifth_percentile <- quantile(but_par_joueur$Total_goals, 0.95)
top_players <- but_par_joueur %>%
  filter(Total_goals > as.numeric(ninetyfifth_percentile))
View(top_players)

# Count the number of top goalscorers for each country
top_scorers_by_country <- top_players %>%
  group_by(Country) %>%
  summarise(Top_Scorers = n()) %>%
  ungroup()  # Ungroup for further operations

# Sort by the number of top scorers in descending order (optional)
top_scorers_by_country <- top_scorers_by_country %>%
  arrange(desc(Top_Scorers))
View(top_scorers_by_country)
