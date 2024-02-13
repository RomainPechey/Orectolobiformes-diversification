###### Code pour distribution des fossiles d'orectolobiformes (graphique interactif)


library("abind")
library("fields")
library("plotrix")
library("methods")
library("utils")
library("grDevices")
library("sp")
library("ncdf4")
library("stats")
library("lubridate")
library("ggplot2")
library("sf")
library("mapsf")
library("rnaturalearth")
library("plotly")
library(htmlwidgets)
library(openxlsx)


couleurs_hex <- c(
  "#F4A460", "#FF8C00", "#EE82EE", "#DA70D6", "#BA55D3",
  "#9932CC", "#9400D3", "#8A2BE2", "#9370DB", "#8B008B",
  "#800080", "#4B0082", "#8dd3c7", "#fb8072", "#80b1d3",
  "#ADFF2F", "#7CFC00", "#32CD32", "#98FB98", "#228B22",
  "#3CB371", "#9ACD32", "#6B8E23", "#2F4F4F", "#00FFFF",
  "#00CED1", "#87CEFA", "#00BFFF", "#1E90FF", "#0000FF",
  "#0000CD", "#E0FFFF", "#008B8B", "#008B8B", "#FF69B4",
  "#FFB6C1", "#FFC0CB", "#FFE4E1", "#696969", "#C0C0C0",
  "#FFFF00","#B8860B","#FF00FF","#1E90FF")

# Spécifier le chemin du dossier contenant les fichiers
chemin_fichier <- "Desktop/Stage_M2/Données_brute/Occurences_fossiles_orecto.xlsx"
donnees_excel <- read.xlsx(chemin_fichier)
donnees_excel[,28] <-as.numeric(donnees_excel[,28])
donnees_excel[,29] <- as.numeric(donnees_excel[,29])
str(donnees_excel)
## Ajouter une colonne age qui fait la moyenne estimation sup et inf
donnees_excel <- donnees_excel%>%
  mutate(age=(donnees_excel[,23]+donnees_excel[,22])/2)


# Télécharger les données du monde pour la carte de fond
countries <- ne_countries(scale = 50, type = "countries", continent = NULL,
                          country = NULL, geounit = NULL, sovereignty = NULL,
                          returnclass = "sf")

## Réalisation de la carte avec les étiquettes interactif pour les points
fig <- plot_geo(donnees_excel, lat = donnees_excel[,28], lon = donnees_excel[,29])
fig <- fig %>% add_markers(
  text = paste("Genre_espèce : ", donnees_excel[,5],
               "<br> Lieu: ", donnees_excel[,27],
               "<br> Epoque : ",paste(donnees_excel[, 23], "à", donnees_excel[, 22]),
               "<br> ID : ", donnees_excel[,33]),color = donnees_excel$age)
fig <- fig %>% layout(
  title = 'Occurences fossiles Orectolobiformes', geo = countries,width = 1200,
  height = 800)
fig <- fig %>% colorbar(title = "Age des fossiles <br /> (en millions d'années)")
fig


# Sauvegarder la carte
saveWidget(fig, file = "Desktop/Stage_M2/Figures/graphique_interactif_occurence.html")
