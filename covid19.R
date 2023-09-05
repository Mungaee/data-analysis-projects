setwd("/Users/eltonmungai/Documents/Portfolio/datasets")
covid_file <- read.csv(file="covid19.csv")
library(readr)
dim(covid_file)
colnames(covid_file)
head(covid_file)
library(dplyr)
glimpse(covid_file)

covid_df_all_states <- covid_file[covid_file$Province_State=="All States",]
covid_df_all_states <- covid_df_all_states[, -which(names(covid_df_all_states) == "Province_State")]

covid_df_all_states_daily <- covid_df_all_states[, c("Date", "Country_Region", "active", "hospitalizedCurr", "daily_tested", "daily_positive")]

library(dplyr)

# Group by Country_Region and summarize the data
covid_df_all_states_daily_sum <- covid_df_all_states_daily %>%
  group_by(Country_Region) %>%
  summarize(
    tested = sum(daily_tested),
    positive = sum(daily_positive),
    active = sum(active),
    hospitalized = sum(hospitalizedCurr)
  ) %>%
  arrange(desc(tested))  # Arrange the tested column in descending order

# Extract the top ten rows
library(dplyr)

# Group by Country_Region and summarize the data
covid_df_all_states_daily_sum <- covid_df_all_states_daily %>%
  group_by(Country_Region) %>%
  summarize(
    tested = sum(daily_tested),
    positive = sum(daily_positive),
    active = sum(active),
    hospitalized = sum(hospitalizedCurr)
  ) %>%
  arrange(desc(tested))  # Arrange the tested column in descending order

# Display the resulting dataframe
covid_df_all_states_daily_sum

# Extract the top ten rows
covid_top_10 <- head(covid_df_all_states_daily_sum, 10)
covid_top_10

library(ggplot2)

# Create a bar graph
ggplot(covid_df_all_states_daily_sum, aes(x = reorder(Country_Region, tested), y = tested)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Total Tested COVID-19 Cases by Country/Region",
    x = "Country/Region",
    y = "Total Tested"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()  # Rotate x-axis labels for better readability

ggplot(covid_top_10, aes(x = reorder(Country_Region, tested), y = tested)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Total Tested COVID-19 Cases by Country/Region",
    x = "Country/Region",
    y = "Total Tested"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()  # Rotate x-axis labels for better readability

# Create vectors from covid_top_10 dataframe
countries <- covid_top_10$Country_Region
tested_cases <- covid_top_10$tested
positive_cases <- covid_top_10$positive
active_cases <- covid_top_10$active
hospitalized_cases <- covid_top_10$hospitalized

# Name the vectors with countries
names(tested_cases) <- countries
names(positive_cases) <- countries
names(active_cases) <- countries
names(hospitalized_cases) <- countries

# Calculate positive-to-tested ratio
positive_tested_ratio <- positive_cases / tested_cases
positive_tested_ratio
# Identify the top three positive-to-tested ratios
top_3_countries <- names(sort(positive_tested_ratio, decreasing = TRUE)[1:3])
top_3_ratios <- positive_tested_ratio[top_3_countries]

# Create the named vector positive_tested_top_3
positive_tested_top_3 <- setNames(top_3_ratios, top_3_countries)

# Display the top 3 countries and their ratios
positive_tested_top_3

# Create vectors for United Kingdom, United States, and Turkey
united_kingdom <- c(0.11326062, 1473672, 166909, 0, 0)
united_states <- c(0.10861819, 17282363, 1877179, 0, 0)
turkey <- c(0.08071172, 2031192, 163941, 2980960, 0)

# Combine the vectors into a matrix using rbind
covid_mat <- rbind(united_kingdom, united_states, turkey)

# Rename the columns of the matrix
colnames(covid_mat) <- c("Ratio", "tested", "positive", "active", "hospitalized")

# Define the question
question <- "Which countries have had the highest number of positive cases against the number of tests?"

# Define the answer as a named vector
answer <- c("Positive tested cases" = positive_tested_top_3)

# Create a list containing data structures
data_structure_list <- list(dataframes = list(covid_df_all_states_daily_sum, covid_top_10),
                            matrices = list(covid_mat),
                            vectors = list(positive_tested_top_3))

# Create a named list containing the data structure lists
named_data_structure_list <- list(data_structure = data_structure_list)

# Create a list containing the question, answer, and data structure lists
covid_analysis_list <- list(question = question, answer = answer, data_structure = named_data_structure_list)

# Display the second element of the covid_analysis_list
print(covid_analysis_list[[2]])
