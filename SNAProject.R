library(dplyr)
library(tidyr)
install.packages("graphlayouts", type = "binary")
library(graphlayouts)
library(ggplot2)
library(igraph)
library(ggraph)
library(scales)
library(arrow)

url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2022/2022-11-01/horror_movies.csv"
horrors <- read.csv(url, stringsAsFactors = FALSE)
head(horrors);




















#DELETE ALL
#PRIMA SOURCE
url <- "https://huggingface.co/datasets/electricsheepeurope/europe-owid-proportion-of-labor-force-who-are-women/resolve/main/data/train-00000-of-00001.parquet"
hugface <- read_parquet(url)
head(hugface)

#Seconda surce GITHUB
github<- read.csv("Merged_Dataset.csv")

#Ultima surce
eurostat<- read.csv("Eurostat.csv")

#Dataframe per solo le nazioni che mi interessano
hugface_piccolo <- hugface %>%
  filter(country_iso3 %in% c(
    "ITA", "FRA", "NOR", "IRL",
    "GRC", "FIN", "ALB", "DEU"
  ))

#Dataframe per solo le nazioni che mi interessano
github_piccolo <- github %>%
  filter(Country %in% c(
    "Italy", "France", "Norway", "Ireland",
    "Greece", "Finland", "Albania", "Germany"
  ))

#Dataframe per solo le nazioni che mi interessano
eurostat_piccolo <- eurostat %>%
  filter(Geopolitical.entity..reporting. %in% c(
    "Italy", "France", "Norway", "Ireland",
    "Greece", "Finland", "Albania", "Germany"
  ))


#Grafo 1 a linee 
ggplot(
  eurostat_piccolo,
  aes(
    x = TIME_PERIOD,
    y = OBS_VALUE,
    color = Geopolitical.entity..reporting.
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  labs(
    title = "% of all tertiary education graduates in STEM education fields",
    x = "Anno",
    y = "% lauree ♀ in STEM"
  ) +
  theme_classic()

#Trova minima value per ogni anno
minn<-eurostat_piccolo %>%
  group_by(TIME_PERIOD) %>%
  slice_min(`OBS_VALUE`, n = 1, with_ties = FALSE) %>%
  select(TIME_PERIOD, Geopolitical.entity..reporting., OBS_VALUE)
print(minn, n=50)

#Trova minima valueper ogni anno
maxx<-eurostat_piccolo %>%
  group_by(TIME_PERIOD) %>%
  slice_max(OBS_VALUE, n = 1, with_ties = FALSE) %>%
  select(TIME_PERIOD, Geopolitical.entity..reporting., OBS_VALUE) 
print(maxx, n=50)

#Grafo 2

# Archi Paese -> Anno
edges <- github_piccolo %>%
  transmute(
    from = Country,
    to = as.character(Year),
    weight = Female_Researchers
  )

# grafo
ggraph(g, layout = "bipartite") +
  
  # ARCHI
  geom_edge_link(
    aes(width = weight),
    alpha = 0.5,
    colour = "grey50"
  ) +
  
  # NODI
  geom_node_point(
    aes(
      shape = type,
      colour = type
    ),
    size = 7
  ) +
  
  # ETICHETTE
  geom_node_text(
    aes(label = name),
    repel = TRUE,
    size = 3,
    box.padding = 1,
    point.padding = 0.25
  ) +
  
  # Spessore archi
  scale_edge_width(
    range = c(0.1, 10)
  ) +
  
  # Forme paesi/anni
  scale_shape_manual(
    name = "Tipo di nodo",
    values = c(
      "FALSE" = 18,
      "TRUE" = 16
    ),
    labels = c(
      "FALSE" = "Paese",
      "TRUE" = "Anno"
    )
  ) +
  
  # Colori paesi/anni
  scale_colour_manual(
    name = "Tipo di nodo",
    values = c(
      "FALSE" = "blue",
      "TRUE" = "pink"
    ),
    labels = c(
      "FALSE" = "Paese",
      "TRUE" = "Anno"
    )
  ) +
  
  labs(
    title = "Rete Paese–Anno della percentuale di ricercatrici nell'ambito STEM",
    subtitle = "Lo spessore degli archi rappresenta la percentuale di donne ricercatrici in STEM",
    edge_width = "% donne STEM"
  ) +
  
  theme_void() +
  
  theme(
    plot.title = element_text(
      size = 18,
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 11,
      face = "italic",
      hjust = 0.5
    ),
    legend.position = "top"
  )

#Grafo 3 
ggplot(
  hugface_piccolo,
  aes(
    x = year,
    y = `Share of the labor force that is female`,
    color = country_iso3
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  labs(
    title = "% donne nella forza lavoro",
    x = "Anno",
    y = "% donne",
    color = "Paese"
  ) +
   theme_classic() 
