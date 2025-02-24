library(dplyr)
library(ggplot2)
library(cluster)
library(randomForest)
library(caret)
library(purrr)

# Read the data
salaries <- read.csv("2021-2024PlayersSalaries.csv")
stats_2021 <- read.csv("2021-2022 NBA Player Stats - Regular.csv")
stats_2022 <- read.csv("2022-2023 NBA Player Stats - Regular.csv")
stats_2023 <- read.csv("2023-2024 NBA Player Stats - Regular.csv")

# Columns to select
select_columns <- c("Player", "Pos", "Age", "Tm", "G", "GS", "MP", "FG", "FGA", "FG.", "X3P", "X3PA", "X2P", "X2PA", 
                    "eFG.", "FT", "FTA", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS")

# Function to process data for each year
process_year_data <- function(data, year) {
  data %>%
    select(all_of(select_columns)) %>%
    rename_with(~ paste0(.x, year), -Player)  # Add year suffix to each column, except Player
}

# Process data for each year
stats_2021_processed <- process_year_data(stats_2021, 2021)
stats_2022_processed <- process_year_data(stats_2022, 2022)
stats_2023_processed <- process_year_data(stats_2023, 2023)

# Merge the data
merged_data <- salaries %>%
  left_join(stats_2021_processed, by = "Player") %>%
  left_join(stats_2022_processed, by = "Player") %>%
  left_join(stats_2023_processed, by = "Player")

# View the merged data
head(merged_data)

# Selecting and Renaming Columns
final_data <- merged_data %>%
  select(Player, Age2021 = Age2021, Age2022 = Age2022, Age2023 = Age2023, 
         Salary.2021.2022., Salary.2022.2023., Salary.2023.2024., 
         ends_with("2021"), ends_with("2022"), ends_with("2023")) %>%
  filter(!is.na(Age2022)) %>%
  distinct()

head(final_data)


# Adding Potential Score
young_players <- final_data %>%
  filter(Age2022 < 25)

young_players <- young_players %>%
  rowwise() %>%
  mutate(Potential_Score = mean(c_across(starts_with("PTS") | starts_with("AST") | starts_with("TRB") | starts_with("STL")| starts_with("BLK")), na.rm = TRUE)) %>%
  ungroup()


# Salary Increase Indicator: 0 - Salary Decrease or Same, 1 - Salary Increase
young_players <- young_players %>%
  mutate(Salary_Increase = ifelse(Salary.2023.2024. > Salary.2022.2023., 1, 0))

# Visualizing Potential Score vs. Salary with Salary Increase Indicator
ggplot(young_players, aes(x = Potential_Score, y = Salary.2023.2024., color = as.factor(Salary_Increase))) +
  geom_point(size = 3) +
  scale_color_manual(values = c("0" = "red", "1" = "green")) +
  labs(title = "Potential Score vs. Salary with Salary Increase Indicator",
       x = "Potential Score", y = "Salary 2023-2024", color = "Salary Increase") +
  theme_minimal()

# K-means Clustering
set.seed(123)
cluster_data <- young_players %>%
  select(Potential_Score, Salary.2023.2024.) %>%
  na.omit()

clusters <- kmeans(cluster_data, centers = 5)

young_players$Cluster <- clusters$cluster

# Visualizing Clustering of Young Players Based on Potential and Salary
ggplot(young_players, aes(x = Potential_Score, y = Salary.2023.2024., color = as.factor(Cluster))) +
  geom_point(size = 3) +
  labs(title = "Clustering of Young Players Based on Potential and Salary",
       x = "Potential Score", y = "Salary 2023-2024", color = "Cluster") +
  theme_minimal()

# Potential Young Players with High Potential Score and Low Salary
print(young_players %>% select(Player, Age2022, Potential_Score, Salary.2023.2024., Salary_Increase, Cluster) %>% distinct(), n = Inf)

young_players <- young_players %>%
  arrange(desc(Potential_Score)) %>%
  distinct(Player, .keep_all = TRUE)

print(young_players, n = Inf)

  
# Random Forest Model
# 1. Prepare the training data
train_data <- young_players %>%
  select(Player, 
         Salary.2021.2022., Salary.2022.2023., Salary.2023.2024., 
         ends_with("2021"), ends_with("2022"), ends_with("2023")) %>%
  filter(!is.na(Salary.2023.2024.)) %>%
  mutate(Salary_Next_Year = Salary.2023.2024.) %>%
  select(-Salary.2023.2024.)

train_data <- na.omit(train_data)

# 2. Train the random forest model
set.seed(123)

# Train a random forest regression model
rf_model <- randomForest(Salary_Next_Year ~ ., data = train_data, ntree = 5000)

# 3. Evaluate the model (optional)
# Print model summary
print(rf_model)

# 4. Make predictions for the next season (2024-2025) salary
predictions <- predict(rf_model, newdata = train_data)

# Add predictions to the dataset
train_data$Predicted_Salary <- predictions

actual_values <- train_data$Salary_Next_Year
predicted_values <- predictions

SS_total <- sum((actual_values - mean(actual_values))^2)  
SS_residual <- sum((actual_values - predicted_values)^2)  

R_squared <- 1 - (SS_residual / SS_total)

print(paste("R-squared: ", round(R_squared, 4)))

# 5. Create a data frame with player, age, actual salary, predicted salary and ratio
evaluation_data <- train_data %>%
  select(Player, 
         Age2022 = Age2022, 
         ActualSalary = Salary_Next_Year, 
         PredictedSalary = Predicted_Salary) %>%
  mutate(Ratio = PredictedSalary / ActualSalary) %>%  # Corrected Ratio calculation
  distinct(Player, .keep_all = TRUE)

# 6. Print the evaluation data
evaluation_data <- evaluation_data %>%
  filter(Ratio > 1)
print(evaluation_data)


# 7. Visualize the actual vs predicted salaries
ggplot(train_data, aes(x = Salary_Next_Year, y = Predicted_Salary)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") + # Line of perfect prediction
  labs(title = "Actual vs Predicted Salary for 2024-2025", 
       x = "Actual Salary", y = "Predicted Salary") +
  theme_minimal()

