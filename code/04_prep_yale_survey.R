library(haven)
library(here)

# Replace 'yourfile.sav' with your file path
survey <- read_sav(here("data", "CCAM SPSS Data 2008-2023.sav"))

# View the first few rows
head(survey)


