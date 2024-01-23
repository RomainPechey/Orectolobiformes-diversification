###### Code pour distribution des orectolobiformes (graphique interactif)


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
chemin_dossier <- "Desktop/Stage_M2/Données_brute/Orecto"

# Lister tous les fichiers dans le dossier
fichiers <- list.files(chemin_dossier, full.names = TRUE)

# Créer une liste pour stocker les données de chaque fichier
liste_dataframes <- list()
a <- 1
for (fichier in fichiers) {
  # Utiliser la fonction correspondante de readr pour lire le fichier
  dataframe <- read.table(fichier, header = TRUE, sep = ",")
  
  # Extraire les noms des deux premières colonnes
  nom_colonne1 <- colnames(dataframe)[1]
  nom_colonne2 <- colnames(dataframe)[2]
  
  # Ajouter une troisième colonne contenant la concaténation des noms des deux premières colonnes
  dataframe <- dataframe %>%
    mutate(espece = paste0(nom_colonne1, " ", nom_colonne2)) %>%
    mutate(couleur = couleurs_hex[a])
  
  # Renommer les colonnes
  colnames(dataframe) <- c("longitude", "latitude", "espece","couleur")
  
  # Stocker le dataframe dans la liste
  liste_dataframes[[basename(fichier)]] <- dataframe
  a <- a + 1
}

# Accéder aux données de chaque fichier dans la liste (ici accéder aux données du premier fichier) 
print(liste_dataframes[[1]])

# Créer un dataframe unique à partie d'une liste avec les données de latitude et longitude
resultat <- do.call(bind_rows, liste_dataframes)

# Convertir le dataframe en objet sf
sf_data <- st_as_sf(resultat, coords = c("longitude", "latitude"), crs = 4326)

# Tracer la carte avec des points colorés en fonction de l'espèce
map <- ggplot() +
  geom_sf(data = countries, fill = "808080") +
  geom_sf(data = sf_data, aes(color = espece), size = 1) +
  theme_void()

# Convertir le graphique ggplot en plotly
fig <- ggplotly(map) %>% layout(showlegend = FALSE)

# Afficher le graphique interactif
fig

# Sauvegarder la carte
saveWidget(fig, file = "Desktop/Stage_M2/Figures/graphique_interactif.html")
