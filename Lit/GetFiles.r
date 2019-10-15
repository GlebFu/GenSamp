library(stringr)
library(dplyr)

docs <- list.files()[str_detect(list.files(), ".pdf")]

docs %>% 
  str_sub(end = -5) %>% 
  str_split(" - ", simplify = T) %>%
  write.csv("RefList.csv")

