library(tidyverse)
library(readxl)

pre_test_2 <- read_excel("C:\\Users\\GustavPirich\\Dropbox\\climate_nature_narratives\\input\\rawdata\\experiment_pretest\\pre_test_large_2.xlsx")


pre_test_2 <- pre_test_2[-1,]

str(pre_test_2)
# 1. Check whether participants responded to Q2 with something DIFFERENT 
#    than "Disagree,Strongly agree".
pre_test_2$Check_Q2 <- pre_test_2$Q2 != "Disagree,Strongly agree"

# 2. Check whether participants 'managed to pass' Q18, 
#    i.e. Q18 is NOT "Disagree,Somewhat disagree".
pre_test_2$Check_Q18 <- pre_test_2$Q18 != "Disagree,Somewhat disagree"

# Print the results
pre_test_2

# 1. Flag whether participant failed Q2
#    Here, "failed" means they did NOT choose "Disagree,Strongly agree"
pre_test_2$Failed_Q2 <- pre_test_2$Q2 != "Disagree,Strongly agree"

# 2. Flag whether participant failed Q18
#    Here, "failed" means they did NOT choose "Disagree,Somewhat disagree"
pre_test_2$Failed_Q18 <- pre_test_2$Q18 != "Disagree,Somewhat disagree"

# 3. Identify who failed BOTH
pre_test_2$Failed_Both <- pre_test_2$Failed_Q2 & pre_test_2$Failed_Q18

# 4. If you want only participants who failed both, subset the data frame
df_failed_both <- subset(pre_test_2, Failed_Both)

# View the results
df
df_failed_both


pre_test_2











library(tidyverse)
library(readxl)

pre_test_3 <- read_excel("C:\\Users\\GustavPirich\\Dropbox\\climate_nature_narratives\\input\\rawdata\\experiment_pretest\\pre_test_large_3.xlsx")


pre_test_3 <- pre_test_3[-1,]

str(pre_test_3)
# 1. Check whether participants responded to Q2 with something DIFFERENT 
#    than "Disagree,Strongly agree".
pre_test_3$Check_Q2 <- pre_test_3$Q2 != "Disagree,Strongly agree"

# 2. Check whether participants 'managed to pass' Q18, 
#    i.e. Q18 is NOT "Disagree,Somewhat disagree".
pre_test_3$Check_Q19 <- pre_test_3$Q19 != "Disagree,Somewhat disagree"

# Print the results
pre_test_3

# 1. Flag whether participant failed Q2
#    Here, "failed" means they did NOT choose "Disagree,Strongly agree"
pre_test_3$Failed_Q2 <- pre_test_3$Q2 != "Disagree,Strongly agree"

# 2. Flag whether participant failed Q18
#    Here, "failed" means they did NOT choose "Disagree,Somewhat disagree"
pre_test_3$Failed_Q19 <- pre_test_3$Q19 != "Disagree,Strongly agree"

# 3. Identify who failed BOTH
pre_test_3$Failed_Both <- pre_test_3$Failed_Q2 & pre_test_3$Failed_Q19

# 4. If you want only participants who failed both, subset the data frame
df_failed_both <- subset(pre_test_3, Failed_Both)

# View the results
df
df_failed_both

view(df_failed_both$PROLIFIC_PID)












library(tidyverse)
library(readxl)

pre_test_4 <- read_excel("C:\\Users\\GustavPirich\\Dropbox\\climate_nature_narratives\\input\\rawdata\\experiment_pretest\\pre_test_large_4.xlsx")


pre_test_4 <- pre_test_4[-1,]

str(pre_test_4)
# 1. Check whether participants responded to Q2 with something DIFFERENT 
#    than "Disagree,Strongly agree".
pre_test_4$Check_Q2 <- pre_test_4$Q2 != "Disagree,Strongly agree"

# 2. Check whether participants 'managed to pass' Q18, 
#    i.e. Q18 is NOT "Disagree,Somewhat disagree".
pre_test_4$Check_Q18 <- pre_test_4$Q18 != "Disagree,Somewhat agree"


# 1. Flag whether participant failed Q2
#    Here, "failed" means they did NOT choose "Disagree,Strongly agree"
pre_test_4$Failed_Q2 <- pre_test_4$Q2 != "Disagree,Strongly agree"

# 2. Flag whether participant failed Q18
#    Here, "failed" means they did NOT choose "Disagree,Somewhat disagree"
pre_test_4$Failed_Q18 <- pre_test_4$Q18 != "Disagree,Somewhat disagree"

# 3. Identify who failed BOTH
pre_test_4$Failed_Both <- pre_test_4$Failed_Q2 & pre_test_4$Failed_Q18

# 4. If you want only participants who failed both, subset the data frame
df_failed_both <- subset(pre_test_4, Failed_Both)
# 3. Identify who failed BOTH
pre_test_4$Failed_Both <- pre_test_4$Check_Q2 & pre_test_4$Check_Q18

# 4. If you want only participants who failed both, subset the data frame
df_failed_both <- subset(pre_test_4, Failed_Both)

df_failed_both$PROLIFIC_PID







pre_test_2 <- read_excel("C:\\Users\\GustavPirich\\Dropbox\\climate_nature_narratives\\input\\rawdata\\experiment_pretest\\follow_up_large_2.xlsx")

pre_test_2 <- pre_test_2[-1,]

# 1. Check whether participants responded to Q2 with something DIFFERENT 
#    than "Disagree,Strongly agree".
pre_test_2$Check_Q2 <- pre_test_2$Q2 != "Strongly Disagree,Agree"


# 4. If you want only participants who failed both, subset the data frame
df_failed_both <- subset(pre_test_2, Check_Q2)
# 3. Identify who failed BOTH
df_failed_both$Q10
