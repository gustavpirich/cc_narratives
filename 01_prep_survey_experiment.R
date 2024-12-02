# =============================================================================
# Title: Survey Data Preparation Template
# Purpose: Prepare and clean survey data
# Author: [Gustav Pirich]
# =============================================================================

# =============================================================================
# Load Libraries
# =============================================================================
# Install necessary libraries if not already installed
pacman::p_load(tidyverse, here, hmisc, reshape2, stargazer, tidyr)

# =============================================================================
# Set Paths
# =============================================================================
# Use here() for reproducible relative paths
# Load data


# =============================================================================
# Load Data
# =============================================================================
survey <- read.csv("C:\\Users\\GustavPirich\\Dropbox\\climate_nature_narratives\\input\\rawdata\\experiment\\pretest\\climate_change_narratives_November+29,+2024_01.40.csv")

# storing the second row as subheaders toc check up 
subheaders <- c(survey[1:2, ])
subheaders$Q0

# =============================================================================
# Data Cleaning and Merging
# =============================================================================

# Process the data
df_cleaned <- survey %>%
  slice(-1) %>%
  mutate(across(where(is.character), as.character))

# Remove the second row from the dataframe
df_cleaned_1 <- df_cleaned[-2, ]

df_cleaned_3 <- df_cleaned_1 %>%
  slice(-c(1:7)) %>%
  mutate(treatment = ifelse(FL_6_DO == "Treatment", 1, 0))


#Define the lists of adjectives for each classification
hero_adjectives <- c("good", "trustworthy", "reliable", "innovative", "useful", "dependable")
villain_adjectives <- c("bad", "threatening", "harmful", "dangerous", "unreliable", "greedy")
victim_adjectives <- c("vulnerable", "exploited", "afflicted", "weak", "naive", "powerless")

# Create the new columns
df_cleaned_3$hero <- sapply(df_cleaned_3$Q6, function(x) sum(trimws(unlist(strsplit(x, ","))) %in% hero_adjectives))
df_cleaned_3$villain <- sapply(df_cleaned_3$Q6, function(x) sum(trimws(unlist(strsplit(x, ","))) %in% villain_adjectives))
df_cleaned_3$victim <- sapply(df_cleaned_3$Q6, function(x) sum(trimws(unlist(strsplit(x, ","))) %in% victim_adjectives))

# Create the dummy variable columns
df_cleaned_3$hero_dummy <- ifelse(df_cleaned_3$hero > 0, 1, 0)
df_cleaned_3$villain_dummy <- ifelse(df_cleaned_3$villain > 0, 1, 0)
df_cleaned_3$victim_dummy <- ifelse(df_cleaned_3$victim > 0, 1, 0)





# =============================================================================
# Preparation of descriptive statistics
# =============================================================================
descriptive_stats_1 <- df_cleaned_3 %>% 
  #filter(!(hero == 1 & villain == 0 & victim == 0 |
  #           hero == 0 & villain == 1 & victim == 0 |
  #           hero == 0 & villain == 0 & victim == 1)) %>%
  group_by(treatment) %>%
  summarise(
    avg_hero = mean(hero, na.rm = TRUE),
    avg_villain = mean(villain, na.rm = TRUE),
    avg_victim = mean(victim, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = starts_with("avg_"),
    names_to = "Adjective",
    values_to = "Average_Count"
  ) %>%
  mutate(
    Adjective = gsub("avg_", "", Adjective),
    Adjective = toupper(Adjective)
  )

# Figure 1
# Summarize and reshape data
descriptive_stats_1 <- df_cleaned_3 %>% 
  #filter(!(hero == 1 & villain == 0 & victim == 0 |
  #           hero == 0 & villain == 1 & victim == 0 |
  #           hero == 0 & villain == 0 & victim == 1)) %>%
  group_by(treatment) %>%
  summarise(
    avg_hero = mean(hero, na.rm = TRUE),
    avg_villain = mean(villain, na.rm = TRUE),
    avg_victim = mean(victim, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = starts_with("avg_"),
    names_to = "Adjective",
    values_to = "Average_Count"
  ) %>%
  mutate(
    Adjective = gsub("avg_", "", Adjective),
    Adjective = toupper(Adjective)
  )

descriptive_stats_1 <- df_cleaned_3 %>%
  #filter(!(hero == 1 & villain == 0 & victim == 0 |
  #           hero == 0 & villain == 1 & victim == 0 |
  #           hero == 0 & villain == 0 & victim == 1)) %>%
  group_by(treatment) %>%
  summarise(
    score = hero - (victim + villain) / (hero + villain + victim)
  ) %>%
  pivot_longer(
    cols = starts_with("avg_"),
    names_to = "Adjective",
    values_to = "Average_Count"
  ) %>%
  mutate(
    Adjective = gsub("avg_", "", Adjective),
    Adjective = toupper(Adjective)
  )


# Create bar plot
ggplot(descriptive_stats_1, aes(x = factor(treatment), y = Average_Count, fill = Adjective)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black", width = 0.75) +
  geom_text(aes(label = round(Average_Count, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 5) +
  scale_fill_viridis_d() +  # Use viridis color scale
  labs(
    title = "Green-Tech Hero - Intensive Margin",
    subtitle = "When you think about renewable energy companies and green technology, which of the following terms would associate with it?",
    x = "Treatment Group",
    y = "Average Adjective Count",
    fill = "Role"
  ) +
  scale_x_discrete(labels = c("Control (0)", "Treatment (1)")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 12)
  )

# Figure 2
# Summarize and reshape data
descriptive_stats_2 <- df_cleaned_3 %>%
  group_by(treatment) %>%
  summarise(
    avg_hero_dummy = mean(hero_dummy, na.rm = TRUE),
    avg_villain_dummy = mean(villain_dummy, na.rm = TRUE),
    avg_victim_dummy = mean(victim_dummy, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = starts_with("avg_"),
    names_to = "Adjective",
    values_to = "Average_Dummy_Count"
  ) %>%
  mutate(
    Adjective = gsub("avg_", "", Adjective),
    Adjective = gsub("_dummy", "", Adjective),
    Adjective = toupper(Adjective)
  )

# Create bar plot
ggplot(descriptive_stats_2, aes(x = factor(treatment), y = Average_Dummy_Count, fill = Adjective)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black", width = 0.75) +
  geom_text(aes(label = round(Average_Dummy_Count, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 5) +
  scale_fill_viridis_d() +  # Use viridis color scale
  labs(
    title = "Green-Tech Hero - Extensive Margin",
    subtitle = "When you think about renewable energy companies and green technology, which of the following terms would associate with it?",
    x = "Treatment Group",
    y = "Average Dummy Adjective Count",
    fill = "Role"
  ) +
  scale_x_discrete(labels = c("Control (0)", "Treatment (1)")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 12)
  )






# Create the new columns for Q7.1
df_cleaned_3$hero <- sapply(df_cleaned_3$Q7.1, function(x) sum(trimws(unlist(strsplit(x, ","))) %in% hero_adjectives))
df_cleaned_3$villain <- sapply(df_cleaned_3$Q7.1, function(x) sum(trimws(unlist(strsplit(x, ","))) %in% villain_adjectives))
df_cleaned_3$victim <- sapply(df_cleaned_3$Q7.1, function(x) sum(trimws(unlist(strsplit(x, ","))) %in% victim_adjectives))

# Create the dummy variable columns for Q7.1
df_cleaned_3$hero_dummy <- ifelse(df_cleaned_3$hero > 0, 1, 0)
df_cleaned_3$villain_dummy <- ifelse(df_cleaned_3$villain > 0, 1, 0)
df_cleaned_3$victim_dummy <- ifelse(df_cleaned_3$victim > 0, 1, 0)

# =============================================================================
# Preparation of descriptive statistics
# =============================================================================

# Figure 1 (Intensive Margin)
# Summarize and reshape data
descriptive_stats_1 <- df_cleaned_3 %>% 
  #filter(!(hero == 1 & villain == 0 & victim == 0 |
  #           hero == 0 & villain == 1 & victim == 0 |
  #           hero == 0 & villain == 0 & victim == 1)) %>%
  group_by(treatment) %>%
  summarise(
    avg_hero = mean(hero, na.rm = TRUE),
    avg_villain = mean(villain, na.rm = TRUE),
    avg_victim = mean(victim, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = starts_with("avg_"),
    names_to = "Adjective",
    values_to = "Average_Count"
  ) %>%
  mutate(
    Adjective = gsub("avg_", "", Adjective),
    Adjective = toupper(Adjective)
  )

# Create bar plot
ggplot(descriptive_stats_1, aes(x = factor(treatment), y = Average_Count, fill = Adjective)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black", width = 0.75) +
  geom_text(aes(label = round(Average_Count, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 5) +
  scale_fill_viridis_d() +  # Use viridis color scale
  labs(
    title = "Companies Hero - Intensive Margin",
    subtitle = "When you think about private companies, which of the following terms would associate with it?",
    x = "Treatment Group",
    y = "Average Adjective Count",
    fill = "Role"
  ) +
  scale_x_discrete(labels = c("Control (0)", "Treatment (1)")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 12)
  )

# Figure 2 (Extensive Margin)
# Summarize and reshape data
descriptive_stats_2 <- df_cleaned_3 %>% 
  #filter(!(hero == 1 & villain == 0 & victim == 0 |
  #          hero == 0 & villain == 1 & victim == 0 |
  #           hero == 0 & villain == 0 & victim == 1)) %>%
  group_by(treatment) %>%
  summarise(
    avg_hero_dummy = mean(hero_dummy, na.rm = TRUE),
    avg_villain_dummy = mean(villain_dummy, na.rm = TRUE),
    avg_victim_dummy = mean(victim_dummy, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = starts_with("avg_"),
    names_to = "Adjective",
    values_to = "Average_Dummy_Count"
  ) %>%
  mutate(
    Adjective = gsub("avg_", "", Adjective),
    Adjective = gsub("_dummy", "", Adjective),
    Adjective = toupper(Adjective)
  )

# Create bar plot
ggplot(descriptive_stats_2, aes(x = factor(treatment), y = Average_Dummy_Count, fill = Adjective)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black", width = 0.75) +
  geom_text(aes(label = round(Average_Dummy_Count, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 5) +
  scale_fill_viridis_d() +  # Use viridis color scale
  labs(
    title = "Private Companies Hero - Extensive Margin",
    subtitle = "When you think about private companies, which of the following terms would associate with it?",
    x = "Treatment Group",
    y = "Average Dummy Adjective",
    fill = "Role"
  ) +
  scale_x_discrete(labels = c("Control (0)", "Treatment (1)")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 12)
  )






# =============================================================================
# Preparation of descriptive statistics
# =============================================================================

# Figure Emotion Score (Intensive Margin)
# Summarize and reshape data
descriptive_stats_1 <- df_cleaned_3 %>% 
  group_by(treatment) %>%
  summarise(
    score = mean((hero - (villain + victim)) / (villain + victim + hero), na.rm = TRUE)  # Calculate the average score
  )

# Create bar plot with differentiated colors for each treatment group
ggplot(descriptive_stats_1, aes(x = factor(treatment), y = score, fill = factor(treatment))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black", width = 0.75) +
  geom_text(aes(label = round(score, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 5) +
  scale_fill_viridis_d() +  # Use Viridis discrete color scale
  labs(
    title = "Companies Hero - Intensive Margin",
    subtitle = "When you think about private companies, which of the following terms would associate with it?",
    x = "Treatment Group",
    y = "Score",
    fill = "Treatment"
  ) +
  scale_x_discrete(labels = c("Control (0)", "Treatment (1)")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 12)
  )


 
# Analyzing Outcomes Q0
summary_table <- df_cleaned_3 %>%
  group_by(treatment, Q0) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

# Display the summary table
print(summary_table)



# Ensure 'treatment' is a factor
df_cleaned_3$treatment <- factor(df_cleaned_3$treatment, levels = c(0, 1), labels = c("Control", "Treatment"))

# Convert Q0 to a factor with ordered levels
Q0_levels <- c(
  "Strongly disagree",
  "Somewhat disagree",
  "I do not have an opinion on this topic",
  "Somewhat agree",
  "Strongly agree"
)

df_cleaned_3$Q0 <- factor(df_cleaned_3$Q0, levels = Q0_levels, ordered = TRUE)
df_cleaned_3$Q2.1 <- factor(df_cleaned_3$Q2.1, levels = Q0_levels, ordered = TRUE)
df_cleaned_3$Q5 <- factor(df_cleaned_3$Q5, levels = Q0_levels, ordered = TRUE)




# Summary statistics: Counts of Q0 responses by treatment group
summary_table <- df_cleaned_3 %>%
  group_by(treatment, Q0, Q2.1, Q5) %>%
  summarise(mean()) %>%
  ungroup()

# Display the summary table
print(summary_table)

# Calculate proportions
summary_table <- summary_table %>%
  group_by(treatment) %>%
  mutate(Proportion = Count / sum(Count) * 100)







# Summary statistics: Counts of Q0 responses by treatment group
summary_table <- df_cleaned_3 %>%
  group_by(treatment, Q0) %>%
  summarise(Count = n()) %>%
  ungroup()

# Display the summary table
print(summary_table)

# Calculate proportions
summary_table <- summary_table %>%
  group_by(treatment) %>%
  mutate(Proportion = Count / sum(Count) * 100)

# Display the table with proportions

stargazer(df_cleaned_3)

# Create a contingency table
contingency_table <- table(df_cleaned_3$Q0, df_cleaned_3$treatment)

# Perform chi-squared test
chisq_result <- chisq.test(contingency_table)



# Ensure Q0 has all five levels
summary_table <- summary_table %>%
  mutate(Q0 = factor(Q0, levels = c(
    "Strongly disagree",
    "Somewhat disagree",
    "I do not have an opinion on this topic",
    "Somewhat agree",
    "Strongly agree"
  )))

# Reshape the table to wide format
summary_table_wide <- summary_table %>%
  pivot_wider(
    names_from = Q0,
    values_from = Count,
    values_fill = 0
  )

# View the reshaped table
print(summary_table_wide)

stargazer::stargazer(summary_table_wide, out = "text")

# Visualization: Bar plot of Q0 responses by treatment group
ggplot(summary_table, aes(x = Q0, y = Count , fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Distribution of Q0 Responses by Treatment Group",
    x = "Q0 Response",
    y = "Count",
    fill = "Group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0))


# Pivot the table
summary_table_wide <- summary_table %>%
  pivot_wider(
    names_from = Q0,
    values_from = c(Count)
  )

# View the wide table
print(summary_table_wide)


# bar plot
gldat %>%
  ggplot(aes(Satisfaction, Frequency, fill = Course)) + 
  geom_bar(stat="identity", position = position_dodge()) +
  # define colors
  scale_fill_manual(values = clrs3) + 
  # add text
  geom_text(aes(label=Frequency), vjust=1.6, color="white", 
            # define text position and size
            position = position_dodge(0.9),  size=3.5) + 
  theme_bw() 




# 2. Define Your Variables
# ---------------------------------------------------------------
# Use your dataset
df_cleaned_3$Q3_1 <- as.numeric(df_cleaned_3$Q3_1)
df_cleaned_3$Q4_1 <- as.numeric(df_cleaned_3$Q4_1)
df_cleaned_3$Q1_6 <- as.numeric(df_cleaned_3$Q1_6)

survey_data <- df_cleaned_3

# Treatment variable
treatment_var <- "treatment"  # Replace with your actual treatment variable name if different

# List of Likert scale variables
likert_vars <- c("Q0", "Q2.1". "Q5")

# List of 0-100 approval scale variables
approval_vars <- c("Q1_6", "Q3_1", "Q4_1")










# 3. Ensure Likert Scale Variables are Ordered Factors
# ---------------------------------------------------------------
# Define Likert scale levels in order
likert_levels <- c(
  "Strongly disagree",
  "Somewhat disagree",
  "I do not have an opinion on this topic",
  "Somewhat agree",
  "Strongly agree"
)

# Convert Likert variables to ordered factors
survey_data <- survey_data %>%
  mutate(across(all_of(likert_vars),
                ~ factor(., levels = likert_levels, ordered = TRUE)))

# 4. Summary Statistics for Likert Scale Variables
# ---------------------------------------------------------------
# Calculate counts and percentages for each level within each group
likert_summary <- survey_data %>%
  pivot_longer(cols = all_of(likert_vars), names_to = "Variable", values_to = "Response") %>%
  group_by(Variable, Response, !!sym(treatment_var)) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  group_by(Variable, !!sym(treatment_var)) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Print summary table
print(likert_summary)

# 5. Plots for Likert Scale Variables
# ---------------------------------------------------------------
# Create stacked bar charts for Likert variables
for (var in likert_vars) {
  p <- ggplot(survey_data, aes_string(x = treatment_var, fill = var)) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = percent_format()) +
    scale_fill_brewer(palette = "RdYlGn", direction = -1) +
    labs(
      title = paste("Distribution of", var, "Responses by Group"),
      x = "Group",
      y = "Percentage",
      fill = "Response"
    ) +
    scale_x_discrete(labels = c("0" = "Control", "1" = "Treatment")) +
    theme_minimal()
  
  print(p)
}

# 6. Summary Statistics for Approval Scale Variables
# ---------------------------------------------------------------
# Calculate mean and standard deviation for approval variables
approval_summary <- survey_data %>%
  group_by(!!sym(treatment_var)) %>%
  summarise(across(
    all_of(approval_vars),
    list(mean = ~mean(., na.rm = TRUE)),
    .names = "{col}_{fn}"
  ))

# Print summary table
print(approval_summary)




# Specify the Likert-scale variables
likert_vars <- c("Q0", "Q2.1", "Q5")

# Reshape the data into long format for easier summarization
df_long <- df_cleaned_3 %>%
  select(treatment, all_of(likert_vars)) %>%
  pivot_longer(
    cols = all_of(likert_vars),
    names_to = "Question",
    values_to = "Response"
  )

# Create the summary statistics table
summary_table <- df_long %>%
  group_by(treatment, Question, Response) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  group_by(treatment, Question) %>%
  mutate(
    Total = sum(Count),
    Percentage = (Count / Total) * 100
  ) %>%
  ungroup() %>%
  arrange(Question, treatment, Response)

# View the summary table
stargazer(summary_table)





library(flextable)


# Specify the Likert-scale variables
likert_vars <- c("Q0", "Q2.1", "Q5")

# Reshape the data into long format for easier summarization
df_long <- df_cleaned_3 %>%
  select(treatment, Q5) %>%
  pivot_longer(
    cols = "Q5",
    names_to = "Question",
    values_to = "Response"
  )

# Create the summary statistics table
summary_table <- df_long %>%
  #filter(!is.na(Response)) %>%  # Exclude missing values if desired
  group_by(treatment, Question, Response) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  group_by(treatment, Question) %>%
  mutate(
    Total = sum(Count),
    Percentage = (Count / Total) * 100
  ) %>%
  ungroup()

# Pivot the summary table to wide format
summary_table_wide <- summary_table %>%
  #unite("Treatment_Question", treatment, Question, sep = "_") %>%
  pivot_wider(
    names_from = Question,
    values_from = Count 
  ) 
# View the wide summary table
# Pivot the summary table to wide format
summary_table_wide <- summary_table %>%
  select(Question, Response, treatment, Count, Percentage) %>%
  pivot_wider(
    names_from = treatment,
    values_from = c(Count, Percentage)
  ) %>%
  arrange(Question, Response)


# Replace NA values with 0
summary_table_wide <- summary_table_wide %>%
  mutate(across(everything(), ~ replace_na(., 0)))

# Replace NA values with 0 and round percentages
summary_table_wide <- summary_table_wide %>%
  mutate(
    across(starts_with("Percentage"), ~ round(replace_na(., 0), 1))
  )

# Create a nicely formatted table
formatted_table <- flextable(summary_table_wide) %>%
  set_header_labels(
    Count_Control = "Control (Count)",
    Count_Treatment = "Treatment (Count)",
    Percentage_Control = "Control (%)",
    Percentage_Treatment = "Treatment (%)"
  ) %>%
  theme_vanilla() %>%  # Apply a clean theme
  autofit() %>%        # Automatically adjust column widths
  align(align = "center", part = "all") %>%
  bold(part = "header") %>%
  fontsize(size = 10, part = "all") %>%
  add_header_row(
    values = c("", "Control Group", "Treatment Group"),
    colwidths = c(2, 2, 2)
  ) %>%
  merge_h(part = "header") %>%
  set_caption("I would find it desirable to continue relying on fossil ressoures as the basis of our energy production")

# Print the flextable
formatted_table








# Specify the Likert-scale variables
likert_vars <- c("Q0", "Q2.1", "Q5")

# Reshape the data into long format for easier summarization
df_long <- df_cleaned_3 %>%
  select(treatment, Q5) %>%
  pivot_longer(
    cols = "Q5",
    names_to = "Question",
    values_to = "Response"
  )

# Create the summary statistics table
summary_table <- df_long %>%
  filter(!is.na(Response)) %>%  # Exclude missing values if desired
  group_by(treatment, Question, Response) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  group_by(treatment, Question) %>%
  mutate(
    Total = sum(Count),
    Percentage = (Count / Total) * 100
  ) %>%
  ungroup()


# View the wide summary table
# Pivot the summary table to wide format
summary_table_wide <- summary_table %>%
  select(Question, Response, treatment, Count, Percentage) %>%
  pivot_wider(
    names_from = treatment,
    values_from = c(Count, Percentage)
  ) %>%
  arrange(Question, Response)


# Replace NA values with 0
summary_table_wide <- summary_table_wide %>%
  mutate(across(everything(), ~ replace_na(., 0)))


# Replace NA values in Response with "did not reply" and exclude the Question column
summary_table_wide <- summary_table_wide %>%
  #select(-Question) %>%  # Exclude the 'Question' column
  mutate(
    Response = ifelse(is.na(Response), "Did not reply", as.character(Response)),  # Replace NA in Response
    across(starts_with("Percentage"), ~ round(., 1))  # Round percentage columns to 1 decimal place
  )
# Replace NA values with 0 and round percentages
summary_table_wide <- summary_table_wide %>%
  mutate(
    across(starts_with("Percentage"), ~ round(replace_na(., 0), 1))
  )

# Create a nicely formatted table
formatted_table <- flextable(summary_table_wide) %>%
  set_header_labels(
    Count_Control = "Control (Count)",
    Count_Treatment = "Treatment (Count)",
    Percentage_Control = "Control (%)",
    Percentage_Treatment = "Treatment (%)"
  ) %>%
  theme_vanilla() %>%  # Apply a clean theme
  autofit() %>%        # Automatically adjust column widths
  align(align = "center", part = "all") %>%
  bold(part = "header") %>%
  fontsize(size = 10, part = "all") %>%
  add_header_row(
    values = c("", "Control Group", "Treatment Group"),
    colwidths = c(2, 2, 2)
  ) %>%
  merge_h(part = "header") %>%
  set_caption("I would find it desirable to continue relying on fossil ressoures as the basis of our energy production")

# Print the flextable
formatted_table






# Replace NA values in Response with "did not reply" and exclude the Question column
summary_table_wide <- summary_table_wide %>%
  select(-Question) %>%  # Exclude the 'Question' column
  mutate(
    Response = ifelse(is.na(Response), "Did not reply", as.character(Response)),  # Replace NA in Response
    across(starts_with("Percentage"), ~ round(., 1))  # Round percentage columns to 1 decimal place
  )

# Create a nicely formatted table
formatted_table <- flextable(summary_table_wide) %>%
  set_header_labels(
    Count_Control = "Control (Count)",
    Count_Treatment = "Treatment (Count)",
    Percentage_Control = "Control (%)",
    Percentage_Treatment = "Treatment (%)"
  ) %>%
  theme_vanilla() %>%  # Apply a clean theme
  autofit() %>%        # Automatically adjust column widths
  align(align = "center", part = "all") %>%
  bold(part = "header") %>%
  fontsize(size = 10, part = "all") %>%
  set_caption("Do you think the government should reduce its financial support for green energy initiatives to focus more on fossil fuel production?")

# Print the flextable
formatted_table



# =============================================================================
# Notes
# =============================================================================


