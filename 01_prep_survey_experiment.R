# =============================================================================
# Title: Survey Data Preparation Template
# Purpose: Prepare and clean survey data
# Author: [Gustav Pirich]
# =============================================================================

# =============================================================================
# Load Libraries
# =============================================================================
# Install necessary libraries if not already installed
pacman::p_load(tidyverse, here, hmisc, reshape2)

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

# Figure 1 (Intensive Margin)
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



# Export Cleaned Data
# ============================================================================


# =============================================================================
# Notes
# =============================================================================


