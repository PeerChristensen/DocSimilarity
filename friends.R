library(tidyverse)
library(h2o)
library(friends)
library(tidytext)
library(quanteda)

df <- friends %>%
  filter(speaker %in% c("Phoebe Buffay","Joey Tribbiani", "Monica Geller",
                        "Chandler Bing","Ross Geller","Rachel Green"))

dfm <- df %>% 
  unnest_tokens(word,text) %>%
  filter(!word %in% tm::stopwords("english")) %>%
  count(speaker,word) %>%
  filter(n > 10) %>%
  cast_dfm(speaker,word,n)


dfm <- dfm_weight(dfm, scheme = "prop")

speaker <- "Chandler Bing"

mat <- textstat_simil(
  dfm,
  y = dfm[speaker,],
  margin = "documents",
  method = "cosine",
  min_simil = NULL)

mat %>%
  as_tibble() %>%
  select(-document2) %>%
  arrange(desc(cosine))


#### word2vec

df_row <- df %>%
  group_by(speaker) %>%
  #mutate(text = str_replace_all(text,"\\.","")) %>%
  ungroup() %>%
  tidytext::unnest_tokens(words,text) %>%
  group_by(speaker) %>%
  summarise(text = paste(words,collapse = " ")) %>%
  ungroup()

h2o.init()

hf <- as.h2o(df_row)

words <- h2o.tokenize(hf$text,split=" ")

w2v_model <- h2o.word2vec(words, sent_sample_rate = 0, epochs = 10)

# Find synonyms for the word "teacher":
print(h2o.findSynonyms(w2v_model, "salmon", count = 5))

vecs <- h2o.transform_word2vec(w2v_model, words, aggregate_method = "AVERAGE")

df <- vecs %>% 
  as.data.frame() %>% 
  mutate(speaker = df_row$speaker) %>% 
  select(speaker,everything())

# predict on new book



# distance...
row.names(df) <- df$speaker

df <- df %>% 
  select(-speaker) 




library(factoextra)
library(viridisLite)
library(FNN)

df_dist <- get_dist(df, stand = TRUE)

fviz_dist(df_dist,gradient = list(low = inferno(5)[2], mid = "white", high = inferno(5)[4])) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1,angle = 45),
        axis.title = element_blank())


df_dist %>% 
  tidy() %>%
  filter(item1 == doc | item2 == doc) %>%
  mutate(book = paste0(item1,item2,"")) %>%
  select(-item1,-item2) %>%
  mutate(book = str_remove(book,doc)) %>%
  select(book,distance) %>%
  arrange(distance)

