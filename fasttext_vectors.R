# fasttext

library(fastTextR)
library(tidyverse)
library(epubr)

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

text <- df_row %>%
  select(text) %>%
  summarise(text = paste(text,collapse = " ")) %>% 
  pull(text)

titles <- df_row %>% select(Title=title,Author=creator)

library(fastrtext)

texts <- tolower(df_row$text)
tmp_file_txt <- tempfile()
tmp_file_model <- tempfile()
writeLines(text = texts, con = tmp_file_txt)
execute(commands = c("skipgram", "-input", tmp_file_txt, "-output", tmp_file_model, "-verbose", 1))

model_file <- build_vectors(df_row$text, 'my_model')

model <- load_model(tmp_file_model)
model <- load_model(model_file)

fastrtext::get_sentence_representation(model,df_row$text[5:6])

get_nn(model,"throw",k=5)
