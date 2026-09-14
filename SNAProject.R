library(dplyr)
library(tidyr)
install.packages("graphlayouts", type = "binary")
library(graphlayouts)
library(ggplot2)
library(igraph)
library(ggraph)
library(scales)
library(arrow)
library(RColorBrewer)

#dataset
url2 <- "https://raw.githubusercontent.com/sundeepblue/movie_rating_prediction/master/movie_metadata.csv"
tropes <- read.csv(url2, stringsAsFactors = FALSE)

#Pulizia dataset per avere solo film horror
htropes <- tropes[tropes$plot_keywords != "" & !is.na(tropes$plot_keywords) & grepl("Horror", tropes$genres), ]

#Separa keywor
lista_tropes <- strsplit(htropes$plot_keywords, "\\|")

#Creazione della tabella di coppia (film+trope)
coppia_film_tropes <- data.frame(
  film = rep(htropes$movie_title, sapply(lista_tropes, length)),
  trope = trimws(unlist(lista_tropes)),
  stringsAsFactors = FALSE
)

# matrice di adiacenza Bipartita 
mat_bipartita <- table(coppia_film_tropes$film, coppia_film_tropes$trope)
#matrice unipartita genere e genere
mat_cooccorrenza <- t(mat_bipartita) %*% mat_bipartita

# Grafo1 matrice di adiacenza
g_tropes <- graph_from_adjacency_matrix(
  mat_cooccorrenza, 
  mode = "undirected", 
  weighted = TRUE, 
  diag = FALSE
)

gg_tropes <- graph_from_adjacency_matrix(
  mat_cooccorrenza, 
  mode = "undirected", 
  weighted = TRUE, 
  diag = FALSE
)

g_tropes <- delete_edges(g_tropes, E(g_tropes)[weight < 4])
g_tropes <- delete_vertices(g_tropes, V(g_tropes)[degree(g_tropes) == 0])

gg_tropes <- delete_edges(gg_tropes, E(gg_tropes)[weight < 10])
gg_tropes <- delete_vertices(gg_tropes, V(gg_tropes)[degree(gg_tropes) == 0])

#GRAFO1
ggraph(g_tropes, layout = "kk") + 
  
  geom_edge_link(
    aes(width = weight),
    alpha = 0.5,
    colour = "red") + 
  
  scale_edge_width(
    range = c(0.1, 8)
  ) +
  
  geom_node_point(
    size = 1,
    ) + 
  geom_node_text(
    aes(label = name), 
    repel = TRUE,
    size = 3,
    box.padding = 0.15,
    point.padding = 0.1)+ 
  
  labs(
    title = "Rete di cooccorrenza tra tropes",
    subtitle = "Lo spessore degli archi rappresenta la frequenza di film in cui accade",
    edge_width = "Film con questi tropes"
  ) +
  theme_void()

#Trova max TROPE degree e strength

trope_stats <- data.frame(
  trope = V(g_tropes)$name,
  degree = degree(g_tropes),
  strength = strength(g_tropes, weights = E(g_tropes)$weight)
) %>%
  arrange(desc(strength))

head(trope_stats, 50)

#Trova BETWEENESSS
comp <- components(g_tropes)
cat("Numero di componenti:", comp$no, "\n")
print(table(comp$csize))
giant_id <- which.max(comp$csize)
g_giant <- induced_subgraph(g_tropes, which(comp$membership == giant_id))
cat("Nodi nella componente gigante:", vcount(g_giant), "\n")

btw <- betweenness(g_giant, weights = E(g_giant)$weight, normalized = TRUE)

btw_df <- data.frame(
  trope = V(g_giant)$name,
  betweenness = round(btw, 4)
) %>%
  arrange(desc(betweenness))

head(btw_df, 15)



#GRAFO 2

#Community detection
set.seed(123)
comm <- cluster_louvain(g_tropes, weights = E(g_tropes)$weight)

V(g_tropes)$community <- membership(comm)

cat("Numero di community trovate:", length(unique(membership(comm))), "\n")
cat("Modularità della partizione:", modularity(comm), "\n")

#Matrice di densità tra cluster
n_comm <- length(unique(membership(comm)))
dens_matrix <- matrix(0, nrow = n_comm, ncol = n_comm)
rownames(dens_matrix) <- 1:n_comm
colnames(dens_matrix) <- 1:n_comm

edge_list <- as_data_frame(g_tropes, what = "edges")
edge_list$from_comm <- V(g_tropes)$community[match(edge_list$from, V(g_tropes)$name)]
edge_list$to_comm   <- V(g_tropes)$community[match(edge_list$to, V(g_tropes)$name)]

comm_sizes <- table(V(g_tropes)$community)

for (i in 1:n_comm) {
  for (j in 1:n_comm) {
    if (i == j) {
      n_nodes <- comm_sizes[as.character(i)]
      possibili <- n_nodes * (n_nodes - 1) / 2
      reali <- sum(edge_list$from_comm == i & edge_list$to_comm == i)
      dens_matrix[i, j] <- ifelse(possibili > 0, reali / possibili, 0)
    } else {
      n_i <- comm_sizes[as.character(i)]
      n_j <- comm_sizes[as.character(j)]
      possibili <- n_i * n_j
      reali <- sum((edge_list$from_comm == i & edge_list$to_comm == j) |
                     (edge_list$from_comm == j & edge_list$to_comm == i))
      dens_matrix[i, j] <- ifelse(possibili > 0, reali / possibili, 0)
    }
  }
}

dens_df <- as.data.frame(as.table(dens_matrix))
colnames(dens_df) <- c("Cluster1", "Cluster2", "Densita")

# Etichette
cluster_labels <- sapply(1:n_comm, function(i) {
  nodi <- V(g_tropes)$name[V(g_tropes)$community == i]
  n_tot <- length(nodi)
  paste0(paste(head(nodi, 3), collapse = ", "), " (+"," altri ", n_tot - 3, ")")
})
names(cluster_labels) <- 1:n_comm

# Ordine per size decrescente
comm_sizes_vec <- as.integer(comm_sizes[as.character(1:n_comm)])
ordine_size <- order(comm_sizes_vec, decreasing = TRUE)

dens_df$Cluster1 <- factor(
  cluster_labels[as.character(dens_df$Cluster1)],
  levels = cluster_labels[ordine_size]
)
dens_df$Cluster2 <- factor(
  cluster_labels[as.character(dens_df$Cluster2)],
  levels = cluster_labels[ordine_size]
)

# Heatmap
ggplot(dens_df, aes(x = Cluster1, y = Cluster2, fill = Densita)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Densita, 2)), size = 2.2, color = "black") +
  scale_fill_gradient(low = "white", high = "darkred", trans = "sqrt" ) +
  labs(
    title = "Densità di connessione tra cluster (community)",
    subtitle = paste("Modularità complessiva:", round(modularity(comm), 3)),
    x = "Cluster (nodi principali)",
    y = "Cluster (nodi principali)",
    fill = "Densità"
  ) +
  theme_dark() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7)
  ) +
  coord_fixed()