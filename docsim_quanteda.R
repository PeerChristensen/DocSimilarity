library(tidyverse)
library(epubr)
library(quanteda)
library(tidytext)

files <- list.files(pattern="epub")

df <- list.files(pattern = "*.epub") %>% 
  map_df(~epub(.)) %>% 
  select(title,creator,data) %>%
  unnest(cols=c(data)) %>%
  select(-nword,-nchar,-section)

dfm <- df %>% 
  unnest_tokens(word,text) %>%
  filter(!word %in% stopwords("english")) %>%
  count(title,word) %>%
  filter(n > 10) %>%
  cast_dfm(title,word,n)

dfm <- dfm_weight(dfm, scheme = "prop")

doc <- "The Da Vinci Code"
doc <- "The Circle"

mat <- textstat_simil(
  dfm,
  y = dfm[doc,],
  margin = "documents",
  method = "cosine",
  min_simil = NULL)

mat %>%
  as_tibble() %>%
  select(-document2) %>%
  arrange(desc(cosine))



