library(tidyverse)
library(FNN)
library(h2o)
library(tictoc)
library(epubr)

tic()

h2o.init()

file <- "books/dracula.epub"

new_title <- epub(file) %>% 
  mutate(Title = paste0(title,"_new"),
         Author = paste0(creator,"_new")) %>%
  select(Title,Author)

titles <- read_csv("titles.csv") %>%
  rbind(new_title)

new <- epub(file) %>%
  select(data) %>%
  unnest(cols=c(data)) %>%
  select(-nword,-nchar,-section) %>%
  mutate(text = str_replace_all(text,"\\.","")) %>%
  tidytext::unnest_tokens(words,text) %>%
  summarise(text = paste(words,collapse = " ")) %>%
  as.h2o() %>%
  h2o.tokenize(split= " ")

w2v_model <- h2o.loadModel(list.files("/Users/peerchristensen/Desktop/Projects/DocSimilarity/models",pattern="Word2Vec",full.names = T)[2])

vecs <- h2o.importFile("/Users/peerchristensen/Desktop/Projects/DocSimilarity/vectors.csv")

vecs_new <- h2o.transform_word2vec(w2v_model, new, aggregate_method = "AVERAGE")

vecs_all <-  h2o.rbind(vecs,vecs_new) %>% 
  as.data.frame() 

ind <- knnx.index(vecs_all, vecs_all[nrow(vecs_all),], k=7) %>% as.vector()

dist <- knnx.dist(vecs_all, vecs_all[nrow(vecs_all),], k=7) %>% as.vector()

tibble(book = titles[ind,],
       dist = dist) %>%
  filter(dist > .0001)

# tibble(book = titles[ind[-1],],
#        dist = dist[-1]) %>%
#   filter(dist > .0001)
toc()
