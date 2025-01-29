library(tidyverse)
library(haven)
library(fixest)

dataset_tweets_all <- read_dta("C:\\Users\\GustavPirich\\Dropbox\\climate_nature_narratives\\output\\data\\dta\\dataset_tweets_all.dta")


dataset_tweets_all


dataset_tweets_all %>%
  filter(pricing_hero == 1) %>%
  select(year, original_text) %>%
  view()


dataset_tweets_all$republicans_hero
dataset_tweets_all$republicans_villain

dataset_tweets_all$

feols(senan_trust ~ year | author_id, data = dataset_tweets_all)

sdataset_tweets_all$senan_trust
dataset_tweets_all$year
