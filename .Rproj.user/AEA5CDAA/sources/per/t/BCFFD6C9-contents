library(dplyr)
library(tidyr)
library(rvest)  # for web-scraping
library(sjmisc) # for str_contains function
# the following packages are great for visualization
library(tidygraph)
library(igraph)
library(ggraph)
library(networkD3)
library(collapsibleTree) 

library(htmlwidgets) # if you want to save html results 

#Data preprocessing-----
link <- "https://scholar.google.com.ua/citations?hl=uk&user=5HX--AYAAAAJ"
page <- read_html(link)

researches <- page %>% html_nodes(".gsc_a_at") %>% html_text()
coauthors_row <- page %>% html_nodes(".gsc_a_at+ .gs_gray") %>% html_text()

coauthors_unlisted <- as.data.frame(unlist(strsplit(coauthors_row,", ")))

authors <- distinct(coauthors_unlisted)
authors <- authors %>%  select(Surname=`unlist(strsplit(coauthors_row, ", "))`) %>% filter(Surname != "...")

AdjacencyMat <- matrix(0,nrow = nrow(authors), ncol =  nrow(authors))

rownames(AdjacencyMat) <- authors$Surname
colnames(AdjacencyMat) <- authors$Surname


for(x in authors$Surname){
  for(y in coauthors_row ){
    if (str_contains(y,x)){
      for(xx in authors$Surname){
        if (str_contains(y,xx)){
          AdjacencyMat[x,xx] <- AdjacencyMat[x,xx] + 1 
        }
      }
    }
  }
}

for(x in authors$Surname){
  AdjacencyMat[x,x] <- 0
}




edge_from<-c()
edge_to<-c()
edge_value<-c()

for (i in 1:length(authors$Surname)){
  for (j in 1:length(authors$Surname)){
    if (AdjacencyMat[authors$Surname[i],authors$Surname[j]]!=0)
    {
      edge_from <- append(edge_from,authors$Surname[i])
      edge_to <- append(edge_to,authors$Surname[j])
      edge_value <- append(edge_value,AdjacencyMat[authors$Surname[i],authors$Surname[j]])
    }
  }
}

edge_value <- factor(edge_value)

#Chord diagram----

nodes.df <- data.frame(Surname = authors$Surname)
edges.df <- data.frame(from = edge_from, to = edge_to, value = edge_value)

graph.df <- tbl_graph(nodes = nodes.df, edges = edges.df, directed = F, node_key = "name")

ggraph(graph.df, layout = "linear", circular=T) +
  geom_edge_arc(aes(width=value, color=value), alpha=0.7) +
  geom_node_label(aes(label=Surname, color=Surname), show.legend = F)


#interactive network diagram -----

df <- data_frame(from = edge_from ,to = edge_to )

p <- simpleNetwork(df, height="100px", width="100px")
saveWidget(p, file=paste0( getwd(), "/Diagrams/network_interactive.html"))

p <- simpleNetwork(df, height="100px", width="100px",        
              Source = 1,                 # column number of source
              Target = 2,                 # column number of target
              linkDistance = 10,          # distance between node. Increase this value to have more space between nodes
              charge = -900,                # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
              fontSize = 14,               # size of the node names
              fontFamily = "serif",       # font og node names
              linkColour = "#666",        # colour of edges, MUST be a common colour for the whole graph
              nodeColour = "#69b3a2",     # colour of nodes, MUST be a common colour for the whole graph
              opacity = 0.9,              # opacity of nodes. 0=transparent. 1=no transparency
              zoom = T                    # Can you zoom on the figure?
)
p

# Saving network diagram
saveWidget(p, file=paste0( getwd(), "/Diagrams/improved_network_interactive.html"))

# Dendrograma -----

edge_from<-c()
edge_to<-c()

for (i in 1:length(coauthors_row)){
  for (j in 1:length(authors$Surname)){
    if (str_contains(coauthors_row[i],authors$Surname[j])){
      edge_from <-append(edge_from,researches[i])
      edge_to <- append(edge_to,authors$Surname[j])
    }
  }
}

edges <- data.frame(from = edge_from,to = edge_to)

p <- collapsibleTree(edges, c("from","to"), root = "Researches")
p

# Saving dendrogram
saveWidget(p, file=paste0( getwd(), "/Diagrams/dendrogram_interactive.html"))