library(tidyverse)
library(epubr)
library(h2o)

files <- list.files("books",pattern="epub")

df <- list.files("books",pattern = "*.epub",full.names = T) %>% 
  map_df(~epub(.)) %>% 
 # filter(title != "The Da Vinci Code") %>%
  select(title,creator,data) %>%
  unnest(cols=c(data)) %>%
  select(-nword,-nchar,-section)

df_row <- df %>%
  group_by(title,creator) %>%
  mutate(text = str_replace_all(text,"\\.","")) %>%
  ungroup() %>%
  tidytext::unnest_tokens(words,text) %>%
  group_by(title,creator) %>%
  summarise(text = paste(words,collapse = " ")) %>%
  ungroup()
  
titles <- df_row %>% select(Title=title,Author=creator)
write_csv(titles,"titles.csv")

h2o.init()

hf <- as.h2o(df_row)

words <- h2o.tokenize(hf$text,split=" ")

# words <- hf %>%
#   as_tibble()
#   tidytext::unnest_tokens()
# 
# keep <- as_tibble(words) %>% count(C1) %>% filter(n>=5) %>% pull(C1)
# 
# words <- words %>%
#   as_tibble() %>%
#   filter(C1 %in% keep) %>%
#   as.h2o()

w2v.model <- h2o.word2vec(words, sent_sample_rate = 0, epochs = 3)
h2o.saveModel(w2v.model,"/Users/peerchristensen/Desktop/Projects/DocSimilarity/models")
print(h2o.findSynonyms(w2v.model, "blood", count = 5))

vecs <- h2o.transform_word2vec(w2v.model, words, aggregate_method = "AVERAGE")
h2o.exportFile(vecs,"/Users/peerchristensen/Desktop/Projects/DocSimilarity/vectors.csv",force=T)

# new book
library(tictoc)
tic()

new_title <- epub("DaVinciCode.epub") %>% pull(title)

titles <- c(df_row$title,new_title)

new <- epub("DaVinciCode.epub") %>%
  select(data) %>%
  unnest(cols=c(data)) %>%
  select(-nword,-nchar,-section) %>%
  mutate(text = str_replace_all(text,"\\.","")) %>%
  tidytext::unnest_tokens(words,text) %>%
  summarise(text = paste(words,collapse = " ")) %>%
  as.h2o() %>%
  h2o.tokenize(split= " ")

vecs_new <- h2o.transform_word2vec(w2v.model, new, aggregate_method = "AVERAGE")

df <- h2o.rbind(vecs,vecs_new) %>% 
  as.data.frame() 

row.names(df) <- titles

ind <- knnx.index(df, df[row.names(df)==new_title,], k=5) %>% as.vector()

dist <- knnx.dist(df, df[row.names(df)==new_title,], k=5) %>% as.vector()

tibble(book = row.names(df[ind[-1],]),dist = dist[-1])
toc()

# distance...
row.names(df) <- df$title

df <- df %>% 
  select(-title) 

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
  

