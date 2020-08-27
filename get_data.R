library(tidyverse)
library(epubr)
library(h2o)

files <- list.files(pattern="epub")

df <- list.files(pattern = "*.epub") %>% 
  map_df(~epub(.)) %>% 
  select(title,creator,data) %>%
  unnest(cols=c(data)) %>%
  select(-nword,-nchar,-section)

df_row <- df %>%
  group_by(title,creator) %>%
  mutate(text = str_replace_all(text,"\\.","")) %>%
  ungroup() %>%
  tidytext::unnest_tokens(words,text) %>%
  group_by(title) %>%
  summarise(text = paste(words,collapse = " ")) %>%
  ungroup()
  
h2o.init()

hf <- as.h2o(df_row)

words <- tokenize(hf$text)

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

print(h2o.findSynonyms(w2v.model, "doctor", count = 5))

vecs <- h2o.transform_word2vec(w2v.model, words, aggregate_method = "AVERAGE")

df <- vecs %>% 
  as.data.frame() %>% 
  mutate(title = df_row$title) %>% 
  select(title,everything())

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

ind <- knnx.index(df, df[row.names(df)==doc,], k=10) %>% as.vector()

dist <- knnx.dist(df, df[row.names(df)==doc,], k=10) %>% as.vector()

tibble(book = row.names(df[ind[-1],]),dist = dist[-1])


doc <- "The Da Vinci Code"

df_dist %>% 
  tidy() %>%
  filter(item1 == doc | item2 == doc) %>%
  mutate(book = paste0(item1,item2,"")) %>%
  select(-item1,-item2) %>%
  mutate(book = str_remove(book,doc)) %>%
  select(book,distance) %>%
  arrange(distance)
  

######################################################

# Import the craigslist dataset into H2O:
job_titles <- h2o.importFile("https://s3.amazonaws.com/h2o-public-test-data/smalldata/craigslistJobTitles.csv",
                             col.names = c("category", "jobtitle"),
                             col.types = c("Enum", "String"),
                             header = TRUE)

STOP_WORDS = c("ax", "i", "you", "edu", "s", "t", "m", "subject", "can",
               "lines", "re", "what", "there", "all", "we", "one", "the",
               "a", "an", "of", "or", "in", "for", "by", "on", "but", "is",
               "in", "a", "not", "with", "as", "was", "if", "they", "are",
               "this", "and", "it", "have", "from", "at", "my", "be", "by",
               "not", "that", "to", "from", "com", "org", "like", "likes",
               "so")

# Make the 'tokenize' function:
tokenize <- function(sentences, stop.words = STOP_WORDS) {
  tokenized <- h2o.tokenize(sentences, "\\\\W+")
  tokenized.lower <- h2o.tolower(tokenized)
  tokenized.lengths <- h2o.nchar(tokenized.lower)
  tokenized.filtered <- tokenized.lower[is.na(tokenized.lengths) || tokenized.lengths >= 2,]
  tokenized.words <- tokenized.filtered[h2o.grep("[0-9]", tokenized.filtered, invert = TRUE, output.logical = TRUE),]
  tokenized.words[is.na(tokenized.words) || (! tokenized.words %in% STOP_WORDS),]
}

# Make the 'predict' function:
.predict <- function(job_title, w2v, gbm) {
  words <- tokenize(as.character(as.h2o(job_titles)))
  job_title_vec <- h2o.transform(w2v, words, aggregate_method = "AVERAGE")
  h2o.predict(gbm, job_title_vec)
}

# Break job titles into sequence of words:
words <- tokenize(job_titles$jobtitle)

# Build the word2vec model:
w2v_model <- h2o.word2vec(words, sent_sample_rate = 0, epochs = 10)

# Find synonyms for the word "teacher":
print(h2o.findSynonyms(w2v_model, "teacher", count = 5))

# Calculate a vector for each job title:
job_title_vecs <- h2o.transform(w2v_model, words, aggregate_method = "AVERAGE")



