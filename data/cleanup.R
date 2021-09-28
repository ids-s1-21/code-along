#Cleanup script
#Included for reference, but *you do not need to run this* to follow the
#code-along
library(tidyverse)
nominees <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-21/nominees.csv"
)

nominees_clean <- nominees %>%
  mutate(category = str_to_title(category)) %>%
  separate(category, into = c("category", "year2"), sep = " - (?=\\d)") %>%
  mutate(year2 = as.integer(year2))

# nominees_clean %>% mutate(test = year2 == year) %>%
#   count(test)
#Run the above code to check we don't lose anything by dropping the year2
#column

nominees_clean <- nominees_clean %>%
  select(-year2)

saveRDS(nominees_clean, file = "data/nominees_clean.rds")
