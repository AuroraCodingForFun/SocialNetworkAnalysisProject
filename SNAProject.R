library(dplyr)
library(tidyr)
library(ggplot2)
library(igraph)
library(ggraph)
library(scales)
library(arrow)

#PRIMA SOURCE
url <- "https://huggingface.co/datasets/electricsheepeurope/europe-owid-proportion-of-labor-force-who-are-women/resolve/main/data/train-00000-of-00001.parquet"
df_owid <- read_parquet(url)
head(df_owid)

#Dataframe con un nome diverso perchè Share of the labor force that is female è troppo lungo
df <- df_owid %>%
  select(
    country_name,
    country_iso3,
    year,
    value = `Share of the labor force that is female`
  ) %>%
  filter(!is.na(value))

#Dataframe per solo le nazioni che mi interessano
df_reduced <- df %>%
  filter(country_iso3 %in% c(
    "ITA", "FRA", "NOR", "IRL",
    "GRC", "FIN", "CZE", "ALB"
  ))

#Grafo a linee bello 
ggplot(
  df_reduced %>% filter(country_iso3 %in% country_iso3),
  aes(x = year, y = value, color = country_iso3)
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Traiettorie della partecipazione femminile alla forza lavoro",
    x = "Anno",
    y = "% della forza lavoro ♀"
  ) +
  theme_classic()

#Trova minima valueper ogni anno
min<-df_reduced %>%
  group_by(year) %>%
  slice_min(value, n = 1, with_ties = FALSE) %>%
  select(year, country_name, country_iso3, value) #adoro il mio paese T.T
print(min, n=50)

#Trova minima valueper ogni anno
max<-df_reduced %>%
  group_by(year) %>%
  slice_max(value, n = 1, with_ties = FALSE) %>%
  select(year, country_name, country_iso3, value) #DC è solo la finlandia e la francia
print(max, n=50)


