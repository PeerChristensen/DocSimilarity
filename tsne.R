
library(tidyverse)
library(Rtsne)
library(plotly)

vecs <- read_csv("vectors.csv")
titles <- read_csv("titles.csv")

plot_titles <- titles %>% 
  pull(Title)

plot_authors <- titles %>% 
  #slice(-nrow(titles)) %>%
  pull(Author)

tsne <- Rtsne(as.matrix(vecs), perplexity = 8, pca = F,initial_dims = 100,
              dims = 3)

# tsne_plot <- tsne$Y %>%
#   as.data.frame() %>%
#   mutate(plot_titles = plot_titles) %>%
#   ggplot(aes(x = V1, y = V2, label = plot_titles)) + 
#   geom_text(size = 3) #+
#   #ggrepel::geom_text_repel()
# tsne_plot


plot_3d <- tsne$Y %>%
  as.data.frame() %>%
  mutate(titles = plot_titles,
         authors = plot_authors)

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
  add_markers(text = ~authors,color = I("#cc0000")) %>%
  layout(scene = scene)
fig


