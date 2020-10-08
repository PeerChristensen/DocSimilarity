
library(tidyverse)
library(h2o)
library(FNN)
library(Rtsne)
library(plotly)

h2o.init()

titles <- read_csv("titles.csv") 

vecs <- h2o.importFile("/Users/peerchristensen/Desktop/Projects/DocSimilarity/vectors.csv") 
#vecs <- read_csv("vectors.csv")

user1 <- mean(vecs[6:7,])
user2 <- mean(vecs[c(5,9,10),])
user3 <- mean(vecs[c(15,17,19),])
user4 <- mean(vecs[c(3,8,18),])

vecs <- vecs %>% as_tibble()

n_books <- nrow(vecs)

#users <- list(vecs,user1,user2,user3,user4)
vecs <- rbind(vecs,user1,user2,user3,user4)

plot_titles <- titles %>% 
  pull(Title)

plot_titles <- c(plot_titles,"user1","user2","user3","user4")

plot_authors <- titles %>% 
  #slice(-nrow(titles)) %>%
  pull(Author)

plot_authors <- c(plot_authors,"","","","")


tsne <- Rtsne(as.matrix(vecs), perplexity = 8, pca = F,initial_dims = 100,
              dims = 3)

plot_3d <- tsne$Y %>%
  as.data.frame() %>%
  mutate(titles = plot_titles,
         authors = plot_authors) %>%
  #slice(1:n_books)
  mutate(Type = if_else(str_detect(titles,"user"),"User","Book"))

users <- plot_3d %>%
    filter(Type=="User")
  
# user_trace <- tsne$Y %>%
#   as.data.frame() %>%
#   mutate(titles = plot_titles,
#          authors = plot_authors) %>%
#   slice(n_books+1:nrow(tsne$Y))

axis <- list(
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE
  #showgrid = FALSE
)

scene = list(
  xaxis = axis,
  yaxis = axis,
  zaxis = axis)

fig <- plot_ly(plot_3d, x = ~V1, y = ~V2, z = ~V3,
               # colors = c("#cc0000"),
               type="scatter3d",mode = 'text',text = plot_3d$titles,
               hoverinfo = "text") %>% 
  add_markers(text = ~authors,
              # color = I("#cc0000"),
              name="Type",color = ~Type) %>%
  add_markers(data = users,text = ~titles,x = ~V1, y = ~V2, z = ~V3) %>%
  layout(scene = scene) 
fig




### works

fig <- plot_ly(plot_3d, x = ~V1, y = ~V2, z = ~V3,
               # colors = c("#cc0000"),
               type="scatter3d",mode = 'text',text = plot_3d$titles,
               hoverinfo = "text") %>% 
  add_markers(text = ~authors,
             # color = I("#cc0000"),
              name="Type",color = ~Type) %>%
  layout(scene = scene) 
fig

fig <- plot_ly(plot_3d, x = ~V1, y = ~V2, z = ~V3,
               # colors = c("#cc0000"),
               type="scatter3d",mode = 'markers',text = plot_3d$titles,
               hoverinfo = "text",color = ~Type) %>% 
  layout(scene = scene) 
fig
