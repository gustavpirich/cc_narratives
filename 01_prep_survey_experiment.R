# =============================================================================
# Title: Survey Data Preparation Template
# Purpose: Prepare and clean survey data
# Author: [Gustav Pirich]
# =============================================================================

# =============================================================================
# Load Libraries
# =============================================================================
# Install necessary libraries if not already installed
pacman::p_load(tidyverse, here)

# =============================================================================
# Set Paths
# =============================================================================
# Use here() for reproducible relative paths
# Load data


# =============================================================================
# Load Data
# =============================================================================
survey <- read.csv("C:\\Users\\GustavPirich\\Dropbox\\climate_nature_narratives\\input\\rawdata\\experiment\\pretest\\climate_change_narratives_November+29,+2024_01.40.csv")


# =============================================================================
# Data Cleaning and Merging
# =============================================================================

# Process the data
df_cleaned <- survey %>%
  slice(-c(2:9)) %>%
  mutate(across(where(is.character), as.character))

colnames(df_cleaned) <- c(df_cleaned[1,]) 

df_cleaned <- df_cleaned %>%
  slice(-1) %>%
  rename("Treatment" = "FL_6 - Block Randomizer - Display Order")

df_cleaned_1 <- df_cleaned  %>%
  mutate(Treatment = ifelse(Treatment == "Treatment", 1, 0))


# =============================================================================
# Data Transformation
# =============================================================================
# Add calculated variables, categories, or other transformations

# Convert dates or other data types as needed

# =============================================================================
# Data Validation
# =============================================================================
# Check for duplicates

# Summarize missing data

# =============================================================================
# Export Cleaned Data
# =============================================================================
# Export cleaned dataset
write_csv(NULL, file.path(output_dir, "cleaned_survey_data.csv")) # Replace NULL with your dataset

# Save a summary report
summary_file <- file.path(output_dir, "summary_report.txt")
sink(summary_file)
cat("Summary Report\n")
cat("========================\n")
# Add any summary output or statistics here
sink()

# =============================================================================
# Notes
# =============================================================================
# - Fill in the placeholders with your specific data and operations.
# - Use the defined sections to structure your workflow clearly.
# - Keep it modular for future reuse.
